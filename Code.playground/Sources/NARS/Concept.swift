
public protocol Item {
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
    
    let termLinks = Bag<TermLink>()
    //let tasks = Bag<TermLink>() // sentences
    let beliefs = Bag<Belief>() // judgements
}

extension Concept {
    // returns derived judgements if any
    func accept(_ j: Judgement, subject: Bool = true) -> [Judgement] {
        var judgement = j
        defer {
            beliefs.put(Belief(judgement, 0.9))
        }
        let term = subject ? j.statement.predicate : j.statement.subject
        termLinks.put(TermLink(term, 0.9))
        /// apply rules
        if let b = beliefs.get(j.statement.description) {
            // revision goes first
            judgement = revision(j1: j, j2: b.judgement)
        }
        if let b = beliefs.get() {
            beliefs.put(b) // put back
            // apply rules
            return Rules.allCases.compactMap { r in r.apply((b.judgement, judgement)) }
        }
        return [] // revision does not produce derived judgements
    }
    
    // returns relevant belief or derived judgements if any
    func answer(_ q: Question) -> [Judgement] {
        switch q {
        case .statement(let statement):
            return answer(statement)
        case .general(let term, let copula):
            return answer { s in
                s.subject == term && s.copula == copula
            }
        case .special(let copula, let term):
            return answer { s in
                s.predicate == term && s.copula == copula
            }
        }
    }
    
    // MARK: Private
    
    private func answer(_ s: Statement) -> [Judgement] {
        if let b = beliefs.get(s.description) {
            beliefs.put(b) // put back
            return [b.judgement]
        }
        if let b = beliefs.get() {
            beliefs.put(b) // put back
            // all other rules // backwards inference
            let j = Judgement(s, TruthValue(1, 0.9)) // TODO: finish -- s-*
            // (^ should this be a question?)
            return Rules.allCases.compactMap { r in r.apply((j, b.judgement)) }
        }
        return [] // no results found
    }
    
    private func answer(_ f: (Statement) -> Bool) -> [Judgement] {
        let winner = beliefs.items
            .filter { b in
//                let (s, p) = b.value.judgement.statement.terms
//                return s == p ? false :
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
