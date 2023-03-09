
extension AbstractBag where I == Concept {
    func consider(_ s: Sentence, derive: Bool) -> [Judgement] {
        switch s {
        case .judgement(let j):
            return consider(j.statement) { c in c.accept(j, derive: derive) }
        case .question(let q):
            return consider(q.statement) { c in c.answer(q) }
        case .goal(let g):
            return consider(g, derive: derive) // TODO: finish implementation
        case .cycle: return []
        }
    }
    
    func consider(_ g: Goal, derive: Bool) -> [Judgement] {
        var derived = [Judgement]()
        let q: Question = ("?" >>|=> g.statement)-?
        derived.append(contentsOf: consider(.question(q), derive: derive))
        return derived.filter({ $0.statement != g.statement })
    }
}

// MARK: Private

extension AbstractBag where I == Concept {
    // TODO: this could/should be an asynchronous method delegating processing to each concept independently
    private func consider(_ s: Statement, _ f: (inout Concept) -> [Judgement], recurse: Bool = true) -> [Judgement] {
        switch s {
//        case .variable: return []
//        case .operation: return []
        case .symbol: if s == .º { return [] }
        default: break
        }
        
        var derivedJudgements = [Judgement]()
        var concept = get(s.description) ?? Concept(term: s)
        let derived = f(&concept)
        derivedJudgements.append(contentsOf: derived)
        concept.adjustPriority(derived)
        put(concept)
        
        if recurse { // TODO: make into a recursive function instead of hardcoding 3 levels
            if s.terms != [s] {
                let terms = Set(s.terms)
                for t in terms {
                    let derived = consider(t, f, recurse: false)
                    derivedJudgements.append(contentsOf: derived)
                }
                let t1terms = Set(terms.flatMap({$0.terms}))
                for t1 in t1terms.subtracting(terms) {
                    let derived = consider(t1, f, recurse: false)
                    derivedJudgements.append(contentsOf: derived)
                }
                let t2terms = Set(t1terms.flatMap({$0.terms}))
                for t2 in t2terms.subtracting(t1terms) {
                    let derived = consider(t2, f, recurse: false)
                    derivedJudgements.append(contentsOf: derived)
                }
            }
        }
        
        return derivedJudgements//.removeDuplicates()
    }
}
