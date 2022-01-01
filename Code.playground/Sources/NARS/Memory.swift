
extension Bag where I == Concept {
    func consider(_ s: Sentence) -> [Judgement] {
        switch s {
        case .judgement(let j): return consider(j)
        case .question(let q): return consider(q)
        case .pause: return []
        }
    }
    func consider(_ j: Judgement) -> [Judgement] {
        consider(j.statement) { c in
            c.accept(j, subject: c.term == j.statement.subject)
        }
    }
    func consider(_ q: Question) -> [Judgement] {
        if case .statement(let statement) = q {
            return consider(statement) { c in c.answer(q) }
        } else {
            return consider(q.variableTerm) { c in c.answer(q) }
        }
    }
}

// MARK: Private

extension Bag where I == Concept {
    private func consider(_ s: Statement, _ f: (Concept) -> [Judgement]) -> [Judgement] {
        var derivedJudgements = [Judgement]()
        // TODO: consider overall concept
        // let overallConcept = get(s.description) ?? Concept(term: s)
        let subjectConcept = get(s.subject.description) ?? Concept(term: s.subject)
        let predicateConcept = get(s.predicate.description) ?? Concept(term: s.predicate)
        derivedJudgements.append(contentsOf: f(subjectConcept))
        derivedJudgements.append(contentsOf: f(predicateConcept))
        put(subjectConcept)
        put(predicateConcept)
        return derivedJudgements
    }
    private func consider(_ t: Term, _ f: (Concept) -> [Judgement]) -> [Judgement] {
        guard let concept = get(t.description) else { return [] }
        defer { put(concept) } // put back
        return f(concept)
    }
}

