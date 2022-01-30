
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
            switch j.statement {
            case .term(let term):
                return c.accept(j, isSubject: c.term == term)
            case .statement(let subject, _, _):
                return c.accept(j, isSubject: c.term == subject)
            }
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
    private func consider(_ s: Statement, _ f: (inout Concept) -> [Judgement]) -> [Judgement] {
        var derivedJudgements = [Judgement]()
        // TODO: consider overall concept
        // let overallConcept = get(s.description) ?? Concept(term: s)
        switch s {
        case .term(let term):
            var concept = get(term.description) ?? Concept(term: term)
            derivedJudgements.append(contentsOf: f(&concept))
            put(concept)
            return derivedJudgements
        case .statement(let subject, _, let predicate):
            var subjectConcept = get(subject.description) ?? Concept(term: subject)
            var predicateConcept = get(predicate.description) ?? Concept(term: predicate)
            derivedJudgements.append(contentsOf: f(&subjectConcept))
            derivedJudgements.append(contentsOf: f(&predicateConcept))
            put(subjectConcept)
            put(predicateConcept)
            return derivedJudgements
        }
    }
    private func consider(_ t: Term, _ f: (inout Concept) -> [Judgement]) -> [Judgement] {
        guard var concept = get(t.description) else { return [] }
        defer { put(concept) } // put back
        return f(&concept)
    }
}

