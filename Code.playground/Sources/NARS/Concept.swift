
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
    func accept(_ j: Judgement, derive: Bool, store: Bool = true) -> [Judgement] {
//        if j == lastInput { return Array(lastAccepted) }
//        lastInput = j

        var j = j
        var originalPriority: Double?
        var derived: [Judgement] = []
        
        // revision goes first
        if let b = beliefs.get(j.identifier) {
            originalPriority = b.priority
            var judgement: Judgement
            let sameRule: Bool = {
                let r1 = j.truthValue.rule
                let r2 = b.judgement.truthValue.rule
                if (r1 == nil && r2 == nil) {
                    return false // both user input – revise
                }
                return r1 == r2
            }()
            if sameRule || j.evidenceOverlap(b.judgement) {
                judgement = choice(j1: j, j2: b.judgement)
            } else {
                if j.truthValue.rule == .conversion {
                    judgement = b.judgement
                } else if b.judgement.truthValue.rule == .conversion {
                    judgement = j
                } else {
                    judgement = revision(j1: b.judgement, j2: j)
                }
            }
            // wait to put back original belief to process another one
            if j != judgement {
                j = judgement
                derived.append(judgement)
            }
        }

        
        defer {
            if store {
                // store original belief
                var b = j + (originalPriority ?? 0.9)
                b.adjustPriority(derived)
                beliefs.put(b)
            }
        }


        /*
         * EXIT – return if no recursion
         */
        guard derive else { return derived }
        
        
        /// apply theorems
        derived.append(contentsOf: Theorems.apply(j))
        
        /// apply two-premise rules
        twoPremiseRules:
        if var b = beliefs.get() {            
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

        
        derived.compactMap { $0.represent(j) }
            .filter {
                beliefs.peek($0.identifier) == nil
            }.forEach { jr in
                derived.append(jr) // add represented belief
        }
        
        // TODO: process `values`
        // like rules but modifiable by the system
        // statements using variables
        
        return derived
    }
    
    
    //
    // TODO: account for tense in question answering
    //
    

// TODO: handle other cases
//        if q == lastQuestion &&
//            Set(result) == lastAnswered {
//            return []
//        }
//        lastQuestion = q
//        lastAnswered = Set(result)
//        return result
//    }
    
    public func answer(_ s: Statement) -> [Judgement] {
        if let b = beliefs.get(s.description) {
            beliefs.put(b) // put back
            return [b.judgement]
        } else if let c = conversion(j1: s-*), let b = beliefs.get(c.statement.description) {
            beliefs.put(b) // put back
            let conv = conversion(j1: b.judgement)!
            beliefs.put(conv + 0.9)
            return [conv]
            
        } else {
            
            let answer = beliefs.items.filter { b in
                let t1 = b.value.judgement.statement
                return Term.logic_match(t1: t1, t2: s)
            }.map { b in
                b.value.judgement
            }.max { j1, j2 in
                let c = choice(j1: j1, j2: j2)
                return c.statement == j2.statement
            }
            
            if let ans = answer {
                if let solution = Term.logic_solve(t1: ans.statement, t2: s) {
                    return [Judgement(solution, ans.truthValue, ans.derivationPath, tense: ans.tense, timestamp: ans.timestamp)]
                }
            }
            
            // apply term decomposition
            let judgement = Judgement(s, TruthValue(1.0, reliance)) // TODO: should we handle this better and pass actual timestamp?
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
                } else if case .statement(let qs, let qc, _) = s, !(qc == .predictiveImp && qs.description.hasPrefix("?")) {
                    let backward = Rules.allCases.flatMap { r in
                        r.backward((s-*, b.judgement))
                    }.compactMap { $0 }
                    if !(theorems.isEmpty && backward.isEmpty) {
                        // .NULL is a separator for derived questions
                        return [.NULL-*, b.judgement] + theorems + backward
                    }
                }
            }
        }
        return [] // no results found
    }
}


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
