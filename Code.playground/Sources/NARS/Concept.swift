
public protocol Copying { // TODO: remove
    func copy() -> Self
}

public protocol Item: Copying {
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

public struct Task: Item {
    public var identifier: String { sentence.description }
    public var priority: Double = 0.9
    public let sentence: Sentence
}

// MARK: Concept

public struct Concept: Item {
    public var identifier: String { term.description }
    public var priority: Double = 0.9
    
    let term: Term
    
    internal var termLinks = Bag<TermLink>()
    //let tasks = Bag<TermLink>() // sentences
    internal var beliefs = Bag<Belief>() // judgements
    
    // TODO: how much should the input change
    // before it is considered different?
    // for how long should we keep the cache?
    // after n seconds or instances of the same input
    // should we still permit the signal to go through?
    // implementing a debounce of sorts
    internal var lastInput: Judgement!
    internal var lastAccepted: Set<Judgement> = []
    internal var lastQuestion: Question!
    internal var lastAnswered: Set<Judgement> = []
}

extension Concept {
    // returns derived judgements if any
    mutating func accept(_ j: Judgement, isSubject: Bool = true, derive: Bool) -> [Judgement] {
        if j == lastInput { return Array(lastAccepted) }
        lastInput = j
        defer {
            switch j.statement {
            case .word: fallthrough // TODO: is this accurate?
            case .compound:
                termLinks.put(TermLink(j.statement, 0.9))
            case .statement(let subject, _, let predicate):
                if !j.statement.isTautology {
                    let term = isSubject ? predicate : subject
                    termLinks.put(TermLink(term, 0.9))
                }
            case .variable:
                break // TODO: is this accurate?
            }
            beliefs.put(j + 0.9) // store new belief
        }
        
        // return if no recursion
        guard derive else { return [] }

        
        var derived: [Judgement] = []
        
        // revision goes first
        if let b = beliefs.get(j.statement.description) {
            let judgement = revision(j1: j, j2: b.judgement)
            // wait to put back original belief to process another one
            if j != judgement {
                derived.append(judgement)
            }
        }

        //TODO: these rules should not produce new statements
        // only to be used for inference and answering questions
        //
        // could this be taking place in imagination first?
        
        // conversion is special
        if let c = conversion(j1: j), beliefs.items[c.statement.description] == nil {
            derived.append(c)
        }
        /*
        
        // TODO: add handling of S <-> P |- S -> P |- P -> S
        if case .statement(let s, let c, let p) = j.statement, c == .similarity {
            let c1 = s --> p
            if beliefs.items[c1.description] == nil {
                var tv1 = TruthValue.deduction(j.truthValue, .tautology)
                tv1 = TruthValue(tv1.f, tv1.c, .deduction)
                let j1 = Judgement(c1, tv1)
                derived.append(j1)
            }
            let c2 = p <-> s
            if beliefs.items[c2.description] == nil {
                var tv2 = TruthValue.deduction(j.truthValue, .tautology)
                tv2 = TruthValue(tv2.f, tv2.c, .deduction)
                let j2 = Judgement(c2, tv2)
                derived.append(j2)
            }
            let c3 = p --> s
            if beliefs.items[c3.description] == nil {
                var tv3 = TruthValue.deduction(j.truthValue, .tautology)
                tv3 = TruthValue(tv3.f, tv3.c, .deduction)
                let j3 = Judgement(c3, tv3)
                derived.append(j3)
            }
        }
        */
        
        let results = Teoremas.allCases.flatMap {
            $0.rules.compactMap { $0(j.statement) }
        }.flatMap { t in
            Rules.strong.flatMap {
                $0.apply((j, t-*(1,1)))
            }.compactMap { $0 }
        }
            
//        print("\n\n\(j)\n\n-098765434567890-\n\n", results)
        
        derived.append(contentsOf: results)
        
        /// apply two-premise rules
        if let b = beliefs.get() {
            // TODO: wait to put back
            // modify its "usefullness" value 
            beliefs.put(b) // put back another belief
            // apply rules
            let results = Rules.allCases
                .flatMap { r in
                    r.apply((b.judgement, j))
                    + r.apply((j, b.judgement)) // switch order of premises
                }
                .compactMap { $0 }
            derived.append(contentsOf: results)
            lastAccepted = Set(derived)
            if !derived.isEmpty {
//                print("because...")
//                print("+++", b, "\n", "&&", j)
//                print("it follows...")
            }
        }
//            print("\n\n=========\n\n")
//            derived.forEach { print($0) }
        return derived
    }
    
    // returns relevant belief or derived judgements if any
    mutating func answer(_ q: Question) -> [Judgement] {
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
                        }
                    }
                }
            } else { // TODO: handle other cases 
                result = answer(q.statement)
            }
        default:
            return [] // TODO: handle other cases
        }
        if q == lastQuestion &&
            Set(result) == lastAnswered {
            return []
        }
        lastQuestion = q
        lastAnswered = Set(result)
        return result
    }
    
    // MARK: Private
    
    private func answer(_ s: Statement) -> [Judgement] {
        if let b = beliefs.get(s.description) {
            beliefs.put(b) // put back
            return [b.judgement]
        } else if let b = beliefs.get() {
            beliefs.put(b) // put back
            // all other rules // backwards inference
            return Rules.allCases
                .flatMap { r in
                    r.apply((s-*, b.judgement))
                    + r.apply((b.judgement, s-*)) // switch order of premises
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
