
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
        var judgement = j
        defer {
            switch j.statement {
            case .term(let term):
                termLinks.put(TermLink(term, 0.9))
            case .statement(let subject, _, let predicate):
                if !j.statement.isTautology {
                    let term = isSubject ? predicate : subject
                    termLinks.put(TermLink(term, 0.9))
                }
            }
            beliefs.put(judgement + 0.9) // put back original belief 
        }
        /// apply rules
        if let b = beliefs.get(j.statement.description) {
            // revision goes first
            judgement = revision(j1: j, j2: b.judgement)
        } // wait to put back original belief to process another one 
        if derive, let b = beliefs.get() {
            // TODO: wait to put back
            // modify its "usefullness" value 
            beliefs.put(b) // put back another belief
            // apply rules
            let derived = Rules.allCases
                .flatMap { r in r.apply((b.judgement, judgement)) }
                .compactMap { $0 }
            lastAccepted = Set(derived)
            if !derived.isEmpty {
//                print("because...")
//                print("+++", b, "\n", "&&", j)
//                print("it follows...")
            }
            return derived
        }
        // values will be different if revision happened 
        let derived = j == judgement ? [] : [judgement]
        lastAccepted = Set(derived)
        return derived
    }
    
    // returns relevant belief or derived judgements if any
    mutating func answer(_ q: Question) -> [Judgement] {
        var result: [Judgement] = []
        switch q {
        case .statement(let statement):
            result = answer(statement)
        case .general(let term, let copula):
            result = answer { s in
                switch s {
                case .term(let t):
                    return term == t
                case .statement(let s, let c, _):
                    return term == s && copula == c
                }
            }
        case .special(let copula, let term):
            result = answer { s in
                switch s {
                case .term(let t):
                    return term == t
                case .statement(_, let c, let p):
                    return copula == c && term == p
                }
            }
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
            // all other rules // backwards inference // filter out identity
            return Rules.allCases
                .flatMap { r in r.apply((s-*, b.judgement)) }
                .compactMap { $0 }
//                .filter { j in
//                    j != b.judgement
//                }
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
