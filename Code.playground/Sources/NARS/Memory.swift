
extension AbstractBag where I == Concept {
    func consider(_ s: Sentence, derive: Bool) -> [Judgement] {
        switch s {
        case .judgement(let j): return consider(j, derive: derive)
        case .question(let q): return consider(q, derive: derive)
        case .pause, .cycle: return []
        }
    }
    func consider(_ j: Judgement, derive: Bool) -> [Judgement] {
        consider(j.statement, derive: derive) { c in
            switch j.statement {
            case .symbol: fallthrough // TODO: is this accurate?
            case .compound:
                return c.accept(j, isSubject: c.term == j.statement, derive: derive)
            case .statement(let subject, _, _):
                return c.accept(j, isSubject: c.term == subject, derive: derive)
            case .variable:
                return [] // TODO: is this accurate?
            case .operation:
                return [] // TODO: is this accurate?
            }
        }
    }
    func consider(_ q: Question, derive: Bool) -> [Judgement] {
        if case .statement = q.statement {
            return consider(q.statement, derive: derive) { c in c.answer(q) }
        } else {
            return considerT(q.variableTerm, derive: derive) { c in c.answer(q) }
        }
    }
}

// MARK: Private

extension AbstractBag where I == Concept {
    private func consider(_ s: Statement, derive: Bool, _ f: (inout Concept) -> [Judgement]) -> [Judgement] {
        var derivedJudgements = [Judgement]()
        // TODO: consider overall concept
        // let overallConcept = get(s.description) ?? Concept(term: s)
        switch s {
        case .symbol: // TODO: is this accurate?
            var concept = get(s.description) ?? Concept(term: s)
            derivedJudgements.append(contentsOf: f(&concept))
            if let maxPriority = derivedJudgements.map({$0.truthValue.confidence}).max() {
                let newPriority = (concept.priority + maxPriority) / 2
                concept.priority = min(newPriority, 0.9)
            }
            put(concept)
            return derivedJudgements
        case .compound(let c, let ts):
            if [.c, .d, .n].contains(c) {
                let terms = Set(ts.flatMap{$0.terms})
                for t in terms {
                    if var concept = get(t.description) {
                        derivedJudgements.append(contentsOf: f(&concept))
                        if let maxPriority = derivedJudgements.map({$0.truthValue.confidence}).max() {
                            let newPriority = (concept.priority + maxPriority) / 2
                            concept.priority = min(newPriority, 0.9)
                        }
                        put(concept)
                    }
                }
                return derivedJudgements
            }
            var concept = get(s.description) ?? Concept(term: s)
            derivedJudgements.append(contentsOf: f(&concept))
            if let maxPriority = derivedJudgements.map({$0.truthValue.confidence}).max() {
                let newPriority = (concept.priority + maxPriority) / 2
                concept.priority = min(newPriority, 0.9)
            }
            put(concept)
            return derivedJudgements
        case .statement(let subject, _, let predicate):
            var subjectConcept = get(subject.description) ?? Concept(term: subject)
            var predicateConcept = get(predicate.description) ?? Concept(term: predicate)
            derivedJudgements.append(contentsOf: f(&subjectConcept))
            if let maxPriority = derivedJudgements.map({$0.truthValue.confidence}).max() {
                let newPriority = (subjectConcept.priority + maxPriority) / 2
                subjectConcept.priority = min(newPriority, 0.9)
            }
            derivedJudgements.append(contentsOf: f(&predicateConcept))
            if let maxPriority = derivedJudgements.map({$0.truthValue.confidence}).max() {
                let newPriority = (predicateConcept.priority + maxPriority) / 2
                predicateConcept.priority = min(newPriority, 0.9)
            }
            put(subjectConcept)
            put(predicateConcept)
            return derivedJudgements
        case .variable:
            return [] // TODO: is this accurate?
        case .operation:
            return [] // TODO: is this accurate?
        }
    }
    // TODO: rename
    private func considerT(_ t: Term, derive: Bool, _ f: (inout Concept) -> [Judgement]) -> [Judgement] {
        guard var concept = get(t.description) else { return [] }
        defer { put(concept) } // put back
        return f(&concept)
    }
    
    func contains(_ j: Judgement) -> Bool {
        let identifier = j.statement.description
        switch j.statement {
        case .symbol(let word):
            if let c = peek(word) {
                return c.beliefs.peek(identifier) != nil
            }
        case .compound(_, _):
            return false // TODO: finish implementation
        case .statement(let s, _, let p):
            if let sc = peek(s.description), let pc = peek(p.description) {
                return sc.beliefs.peek(identifier) != nil && pc.beliefs.peek(identifier) != nil
            }
        case .variable(_):
            return false // TODO: finish implementation
        case .operation(_, _):
            return false // TODO: finish implementation
        }
        return false
    }
}

