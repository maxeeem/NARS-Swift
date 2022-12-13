
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
    public var identifier: String { judgement.identifier }
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
    private var _beliefs = Bag<Belief>()
    internal var beliefs: WrappedBag<Belief>

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

extension Concept {
    // returns derived judgements if any
    func accept(_ j: Judgement, isSubject: Bool = true, derive: Bool) -> [Judgement] {
//        if j == lastInput { return Array(lastAccepted) }
//        lastInput = j

        var originalPriority: Double?
        
        var derived: [Judgement] = []

        var j = j
        
        // revision goes first
        if let b = beliefs.get(j.identifier) {
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
        
        var jflipped: Judgement = j
        // store symmetrical statement
        if case .statement(let sub, let cop, let pre) = j.statement, (cop == .equivalence || cop == .similarity) {
            let flipped: Statement = .statement(pre, cop, sub)
            jflipped = Judgement(flipped, j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp)
            if beliefs.peek(jflipped.identifier) == nil {
                derived.append(jflipped)
            }
        }
        
        // store symmetrical compound
        if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
            if terms.count == 2 { // TODO: handle compounds with multiple terms
                let flipped: Statement = .compound(conn, terms.reversed())
                jflipped = Judgement(flipped, j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp)
                if beliefs.peek(jflipped.identifier) == nil {
                    derived.append(jflipped)
                }
            }
        }
        
        defer {
            switch j.statement {
            case .symbol: // TODO: is this accurate?
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

            var b = j + (originalPriority ?? 0.9)
            b.adjustPriority(derived)
//            if j.truthValue.rule != .conversion {
                beliefs.put(b) // store new belief
//            }
        }
        
        // return if no recursion
        guard derive else { return derived }
        
        /// apply two-premise rules
        twoPremiseRules:
        if var b = beliefs.get() {
            
            if b.judgement.statement == jflipped.statement {
                if let b1 = beliefs.get() {
                    beliefs.put(b)
                    b = b1 // use another belief
                } else {
                    beliefs.put(b)
                    break twoPremiseRules
                }
            }
            
            // apply rules
            let results = Rules.allCases
                .flatMap { r in
                    r.apply((b.judgement, j))
                }
                .compactMap { $0 }
            
            derived.append(contentsOf: results)
            
            // TODO: wait to put back
            // modify its "usefullness" value
            b.adjustPriority(results)
            beliefs.put(b) // put back another belief

//            lastAccepted = Set(derived)
            if !derived.isEmpty {
//                print("because...")
//                print("+++", j, "\n", "&&", b)
//                print("it follows...")
            }
        }
        
        derived = derived.removeDuplicates().filter {
            beliefs.peek($0.identifier) == nil
        }//&& $0.statement != j.statement }

        // TODO: process `values`
        // like rules but modifiable by the system
        // statements using variables
        
        return derived
    }
    
    
    //
    // TODO: account for tense in question answering
    //
    
    
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
                        case .symbol: fallthrough // TODO: is this accurate?
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
                        case .symbol: fallthrough // TODO: is this accurate?
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
                result = result.removeDuplicates()
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
            
            let r = Theorems.apply(b.judgement)
                .filter { beliefs.peek($0.description) == nil }

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
