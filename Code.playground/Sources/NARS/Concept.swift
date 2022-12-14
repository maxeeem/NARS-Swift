import NAL

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
    
    //let tasks = Bag<TermLink>() // sentences
    internal var beliefs = Bag<Belief>()
//    internal var beliefs: WrappedBag<Belief>
    
    public var anticipations: [String: (Term, TruthValue)] = [:]

    init(string: String) {
        self.init(term: Term(stringLiteral: string))
    }
    
    init(term: Term) {
        self.term = term
//        self.beliefs = WrappedBag(_beliefs)
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
    func accept(_ j: Judgement, derive: Bool) -> [Judgement] {
//        if j == lastInput { return Array(lastAccepted) }
//        lastInput = j
        if j.statement.description == "[blue] -> (/ likes cat º)" {
            
        }
        var j = j
        var originalPriority: Double?
        var derived: [Judgement] = []
        
        /// helper
        func store(_ j: Judgement) {
            var b = j + (originalPriority ?? 0.9)
            b.adjustPriority(derived)
            beliefs.put(b)
        }
        
        // revision goes first
        if let b = beliefs.get(j.identifier) {
            originalPriority = b.priority
            var judgement: Judgement
            let sameRule: Bool = {
                let r1 = j.truthValue.rule
                let r2 = b.judgement.truthValue.rule
                if (r1 == nil && r2 == nil) {
                    return false // both user input – revise
//                } else if (r1 == nil || r2 == nil) {
//                    return true // one is input, another derived – choose best
                }
                return r1 == r2
            }()
//            let guess: Bool = {
//                let t1 = j.truthValue
//                let t2 = b.judgement.truthValue
//                return t1 == t2 && t1 == .guess
//            }()
            if sameRule || j.evidenceOverlap(b.judgement) {
                judgement = choice(j1: j, j2: b.judgement)
            } else {
                if j.truthValue.rule == .conversion {
                    judgement = b.judgement
                } else if b.judgement.truthValue.rule == .conversion {
                    judgement = j
                } else {
                    if j.statement == ("{Sandy}" --> "dog") {
                        
                    }
                    judgement = revision(j1: b.judgement, j2: j)
                }
            }
            // wait to put back original belief to process another one
            if j != judgement {
                j = judgement
                derived.append(judgement)
            }
        }
        
        if let jf = j.flipped, beliefs.peek(jf.identifier) == nil {
            store(jf) // store symmetrical belief
        }
        
        defer {
            store(j) // store original belief
        }

        
        /*
         * EXIT – return if no recursion
         */
        guard derive else { return derived }
        
        /// apply theorems
        derived.append(contentsOf: Theorems.apply(j))
        
//        if j.statement == "P" {
//            _ = anticipations(for: j)
//        }
        
        /// apply two-premise rules
        twoPremiseRules:
        if var b = beliefs.get() {
            
            if b.judgement.statement == j.flipped?.statement {
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
            
        } else {
            // apply term decomposition
            let judgement = Judgement(s, TruthValue(1.0, reliance)) // TODO: should we handle this better an pass actual timestamp?
            let decomposed = Theorems.apply(judgement)
                .filter { beliefs.peek($0.identifier) != nil }
            if let answer = decomposed.first,
               let j = beliefs.items.values.first(where: { $0.judgement.statement == answer.statement })?.judgement {
                let f = and(j.truthValue.f, answer.truthValue.f)
                let c = and(j.truthValue.c, answer.truthValue.c)
                let tv = TruthValue(f, c) // intersection
                return [Judgement(s, tv)]
            }

            if let b = beliefs.get() {
                beliefs.put(b) // put back
                // all other rules // backward inference
                let theorems = Theorems.apply(b.judgement)
                    .filter { beliefs.peek($0.identifier) == nil }
                if let answer = theorems.first(where: { $0.statement == s }) {
                    return [answer]
                } else {
                    let backward = Rules.allCases.flatMap { r in
                        r.backward((s-*, b.judgement))
                    }.compactMap { $0 }
                    if theorems.isEmpty && backward.isEmpty {
                        return [] // no clue
                    }
                    return [b.judgement] + theorems + backward
                }
            }
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
//
//<(*, a, singular) --> represent>.
//<(*, (*, a, $1), <$1 --> singular>) --> represent>.
//<(*, ?, (*, a, dog)) --> represent>?
//
//
//
//<(*, {C}, {subset}) --> represent>.
//<(*, (*, {#x}, {C}, {#y}), <(*, {#x}, {#y}) --> {subset}>) --> represent>.
//<{(*, (*, {dog}, {C}, {animal}), {?1})} --> represent>?
//
//
//
//<{(*, C, subset)} --> represent>.
//<(*, {(*, $x, C, $y)}, {<(*, $x, $y) --> subset>}) --> represent>.
//<{(*, (*, dog, C, animal), ?1)} --> represent>?
//
//<<(C * ($x * $y)) --> SetTheoryExpression> ==> <($x * $y) --> Subset>>.
//<(C * (dog * animal)) --> SetTheoryExpression>.



//<(dog * animal) --> ?1>?

extension Concept {
    func anticipations(for j: Judgement) -> [String : (Term, TruthValue)] {
        /// process anticipations
        let anticipations = beliefs.items.compactMapValues { b in
            if let ant = b.judgement.statement.anticipation(for: j.statement) {
                return (ant, b.judgement.truthValue)
            }
            return nil
        }
        
//        if !anticipations.isEmpty {
//            print("anticipate", "from \(term):", anticipations.mapValues({$0}).map({$0.value}))
//            
//            let tv = anticipations.first!.value.1
//            let original = anticipations.first!.value.0 + (tv.f, tv.c, 0)
//            let c = tv.c / (tv.c + k)
//            let observed = anticipations.first!.value.0 + (tv.f, c, 0)
//            let rev = revision(j1: original, j2: observed)
//            print("anticipate revised", rev)
//        }
        
        return anticipations
    }
}
