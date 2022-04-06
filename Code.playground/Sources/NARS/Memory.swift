
extension Bag where I == Concept {
    func consider(_ s: Sentence, derive: Bool) -> [Judgement] {
        switch s {
        case .judgement(let j): return consider(j, derive: derive)
        case .question(let q): return consider(q, derive: derive)
        case .pause: return []
        }
    }
    func consider(_ j: Judgement, derive: Bool) -> [Judgement] {
        consider(j.statement, derive: derive) { c in
            switch j.statement {
            case .word: fallthrough // TODO: is this accurate?
            case .compound:
                return c.accept(j, isSubject: c.term == j.statement, derive: derive)
            case .statement(let subject, _, _):
                return c.accept(j, isSubject: c.term == subject, derive: derive)
            }
        }
    }
    func consider(_ q: Question, derive: Bool) -> [Judgement] {
        if case .statement(let statement) = q {
            return consider(statement, derive: derive) { c in c.answer(q) }
        } else {
            return considerT(q.variableTerm, derive: derive) { c in c.answer(q) }
        }
    }
}

// MARK: Private

extension Bag where I == Concept {
    private func consider(_ s: Statement, derive: Bool, _ f: (inout Concept) -> [Judgement]) -> [Judgement] {
        var derivedJudgements = [Judgement]()
        // TODO: consider overall concept
        // let overallConcept = get(s.description) ?? Concept(term: s)
        switch s {
        case .word: fallthrough // TODO: is this accurate?
        case .compound:
            var concept = get(s.description) ?? Concept(term: s)
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
    // TODO: rename
    private func considerT(_ t: Term, derive: Bool, _ f: (inout Concept) -> [Judgement]) -> [Judgement] {
        guard var concept = get(t.description) else { return [] }
        defer { put(concept) } // put back
        return f(&concept)
    }
}

