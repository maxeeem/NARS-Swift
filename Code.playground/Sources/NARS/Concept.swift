
public protocol Item: Equatable {
    var identifier: String { get }
    var priority: Double { get set }
}

public struct TermLink: Item {
    public var identifier: String { term.description }
    public var priority: Double = 0.9
    public let term: Term
}

public struct Belief: Item {
    public var identifier: String { judgement.statement.description }
    public var priority: Double = 0.9
    public let judgement: Judgement
}

//public struct Task: Item {
//    public var identifier: String { sentence.description }
//    public var priority: Double = 0.9
//    public let sentence: Sentence
//}

// MARK: Concept

public struct Concept: Item {
    public var identifier: String { term.description }
    public var priority: Double = 0.9
    
    let term: Term
    
    private var _termLinks = Bag<TermLink>()
    internal var termLinks: WrappedBag<TermLink>
    //let tasks = Bag<TermLink>() // sentences
    private var _beliefs = Bag<Belief>() // judgements
    internal var beliefs: WrappedBag<Belief>// = Bag<Belief>() // judgements

    init(term: Term) {
        self.term = term
        self.termLinks = WrappedBag(_termLinks)
        self.beliefs = WrappedBag(_beliefs)
    }
    
    // TODO: how much should the input change
    // before it is considered different?
    // for how long should we keep the cache?
    // after n seconds or instances of the same input
    // should we still permit the signal to go through?
    // implementing a debounce of sorts
//    internal var lastInput: Judgement!
//    internal var lastAccepted: Set<Judgement> = []
//    internal var lastQuestion: Question!
//    internal var lastAnswered: Set<Judgement> = []
}

extension Bag: Equatable {
    public static func == (lhs: Bag<I>, rhs: Bag<I>) -> Bool {
        Set(lhs.items.keys) == Set(rhs.items.keys)
    }
}

extension WrappedBag: Equatable {
    public static func == (lhs: WrappedBag<I>, rhs: WrappedBag<I>) -> Bool {
        lhs.bag == rhs.bag && lhs.wrapped == rhs.wrapped
    }
}

extension Concept: Equatable {
    public static func == (lhs: Concept, rhs: Concept) -> Bool {
        lhs.term == rhs.term
        && lhs.termLinks == rhs.termLinks
        && lhs.beliefs == rhs.beliefs
    }
}

extension Concept {
    // returns derived judgements if any
    func accept(_ j: Judgement, isSubject: Bool = true, derive: Bool) -> [Judgement] {
//        if j == lastInput { return Array(lastAccepted) }
//        lastInput = j

        var originalPriority: Double?
        
        var derived: [Judgement] = []

        var j = j
        
        // revision goes first
        if let b = beliefs.get(j.statement.description) {
            originalPriority = b.priority
            var judgement: Judgement
            if j.evidenceOverlap(b.judgement) {
                judgement = choice(j1: j, j2: b.judgement)
            } else {
                if j.truthValue.rule == .conversion {
                    judgement = b.judgement
                } else if b.judgement.truthValue.rule == .conversion {
                    judgement = j
                } else {
                    judgement = revision(j1: j, j2: b.judgement)
                }
            }
            // wait to put back original belief to process another one
            if j != judgement {
                j = judgement
                derived.append(judgement)
            }
        }
        
        // store symmetrical statement
        if case .statement(let sub, let cop, let pre) = j.statement, (cop == .equivalence || cop == .similarity) {
            let flipped: Statement = .statement(pre, cop, sub)
            if beliefs.peek(flipped.description) == nil {
                derived.append(Judgement(flipped, j.truthValue, j.derivationPath))
            }
        }
        
        
        defer {
            switch j.statement {
            case .word: // TODO: is this accurate?
                termLinks.put(TermLink(j.statement, 0.9))
            case .compound(let c, _):
                if ![.c, .d, .n].contains(c) {
                    termLinks.put(TermLink(j.statement, 0.9))
                }
            case .statement(let subject, _, let predicate):
                if !j.statement.isTautology {
                    let term = isSubject ? predicate : subject
                    termLinks.put(TermLink(term, 0.9))
                }
            case .variable:
                break // TODO: is this accurate?
            case .operation:
                break // TODO: is this accurate?
            }

            let newPriority: Double
            if let maxPriority = derived.map({$0.truthValue.confidence}).max() {
                newPriority = ((originalPriority ?? 0.9) + maxPriority) / 2
            } else {
                newPriority = originalPriority ?? 0.9
            }
//            print(">>>", newPriority)
//            if j.truthValue.rule != .conversion {
                beliefs.put(j + min(newPriority, 0.9)) // store new belief
//            }
        }
        
        // return if no recursion
        guard derive else { return derived }
        
        /// apply two-premise rules
        if var b = beliefs.get() {
//            print("--", b, j)
            // apply rules
            let results = Rules.allCases
                .flatMap { r in
                    r.apply((b.judgement, j))
                }
                .compactMap { $0 }
            
            derived.append(contentsOf: results)
            
            // TODO: wait to put back
            // modify its "usefullness" value
            if let maxPriority = results.map({$0.truthValue.confidence}).max() {
                let newPriority = (b.priority + maxPriority) / 2
                b.priority = min(newPriority, 0.9)
            }
            beliefs.put(b) // put back another belief
            
//            derived = derived
//                .filter { beliefs.items[$0.statement.description] == nil && $0.statement != j.statement }

//            lastAccepted = Set(derived)
            if !derived.isEmpty {
//                print("because...")
//                print("+++", j, "\n", "&&", b)
//                print("it follows...")
            }
        }
//            print("\n\n=========\n\n")
//            derived.forEach { print($0) }
//        derived = derived.filter { $0.truthValue.rule != nil }
        let dict = Dictionary(grouping: derived) { el in
            el.statement.description
        }
//        print("---", dict)
        let r = dict.values.flatMap { judgements in
            judgements.max { j1, j2 in
                let c = choice(j1: j1, j2: j2)
                return c.statement == j2.statement
            }
        }
            .filter { beliefs.peek($0.statement.description) == nil }//&& $0.statement != j.statement }
//        print("\n\n\n", j)
//        print(beliefs)
//        print(r)
//        print("\n\n")
        
        derived = r
        
        return derived
    }
    
    // returns relevant belief or derived judgements if any
    func answer(_ q: Question) -> [Judgement] {
        var result: [Judgement] = []
        switch q.statement {
        case .statement(let subject, let copula, let predicate):
            if case .variable(let v) = subject {
                if case .query = v {
                    // special
                    result = answer { s in
                        switch s {
                        case .word: fallthrough // TODO: is this accurate?
                        case .compound:
                            return predicate == s
                        case .statement(_, let c, let p):
                            return copula == c && predicate == p
                        case .variable:
                            return false // TODO: is this accurate?
                        case .operation:
                            return false // TODO: is this accurate?
                        }
                    }
                } // TODO: handle other cases
            } else if case .variable(let v) = predicate {
                if case .query = v {
                    // general
                    result = answer { s in
                        switch s {
                        case .word: fallthrough // TODO: is this accurate?
                        case .compound:
                            return subject == s
                        case .statement(let s, let c, _):
                            return subject == s && copula == c
                        case .variable:
                            return false // TODO: is this accurate?
                        case .operation:
                            return false // TODO: is this accurate?
                        }
                    }
                }
            } else { // TODO: handle other cases 
                result = answer(q.statement)
                let dict = Dictionary(grouping: result) { el in
                    el.statement.description
                }
//                print("---", dict)
                let r = dict.values.flatMap { judgements in
                    judgements.max { j1, j2 in
                        let c = choice(j1: j1, j2: j2)
                        return c.statement == j2.statement
                    }
                }
                result = r
//                print("\(self.identifier.uppercased())")
//                result.forEach { print("+ \($0)") }
            }
        default:
            return [] // TODO: handle other cases
        }
//        if q == lastQuestion &&
//            Set(result) == lastAnswered {
//            return []
//        }
//        lastQuestion = q
//        lastAnswered = Set(result)
        return result
    }
    
    // MARK: Private
    
    private func answer(_ s: Statement) -> [Judgement] {
        if let b = beliefs.get(s.description) {
            beliefs.put(b) // put back
            return [b.judgement]
        } else if let c = conversion(j1: s-*), let b = beliefs.get(c.statement.description) {
            beliefs.put(b) // put back
            let conv = conversion(j1: b.judgement)!
            beliefs.put(conv + 0.9)
            return [conv]
            
        } else if let b = beliefs.get() {
            beliefs.put(b) // put back
            // all other rules // backwards inference
//            print("---picked out", b)
            
            let r = Theorems.apply(b.judgement)
                .filter { beliefs.peek($0.description) == nil }

//            print("+++", r)
//            print(">>>", b, results)

            if let answer = r.first(where: { $0.statement == s }) {
                return [answer]
            }
            
            return r +
             Rules.allCases
                .flatMap { r in
                    r.apply((s-*, b.judgement))
                }
                .compactMap { $0 }
        }
        return [] // no results found
    }
    
    private func answer(_ f: (Statement) -> Bool) -> [Judgement] {
        let winner = beliefs.items
            .filter { b in
                f(b.value.judgement.statement)
            }.map { b in
                b.value.judgement
            }.max { j1, j2 in
                let c = choice(j1: j1, j2: j2)
                return c.statement == j2.statement
            }
        return winner == nil ? [] : [winner!]
    }
}
