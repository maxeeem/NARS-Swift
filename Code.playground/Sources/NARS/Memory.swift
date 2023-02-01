
extension AbstractBag where I == Concept {
    func consider(_ s: Sentence, derive: Bool) -> [Judgement] {
        switch s {
        case .judgement(let j): return consider(j, derive: derive)
        case .goal: return [] // TODO: finish implementation
        case .question(let q): return consider(q, derive: derive)
        case .cycle: return []
        }
    }
    func consider(_ j: Judgement, derive: Bool) -> [Judgement] {
        consider(j.statement, isQuestion: false, j: j, derive: derive) { c in
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
            return consider(q.statement, isQuestion: true, j: nil, derive: derive) { c in c.answer(q) }
        } else {
            // TODO: change this; just a temporary modification
            if let vari = q.variableTerm {
                return considerVar(q.variableTerm, derive: derive) { c in c.answer(q) }
            } else {
                return []
            }
        }
    }
}

// MARK: Private

extension AbstractBag where I == Concept {
    private func consider(_ s: Statement, isQuestion: Bool, j: Judgement?, derive: Bool, _ f: (inout Concept) -> [Judgement]) -> [Judgement] {
        var derivedJudgements = [Judgement]()
        // TODO: consider overall concept
        // let overallConcept = get(s.description) ?? Concept(term: s)
        switch s {
        case .symbol: // TODO: is this accurate?
            var concept = get(s.description) ?? Concept(term: s)
            derivedJudgements.append(contentsOf: f(&concept))
            concept.adjustPriority(derivedJudgements)
            put(concept)
            return derivedJudgements
        case .compound(let c, let ts):
//            if c == .n, ts.count == 1 { // TODO: is this correct?
//                return consider(ts[0], derive: derive, f)
//            }
            if [.c, .d, .x].contains(c) {
                let terms = Set(ts.flatMap{$0.terms})
                for t in terms {
                    var concept = get(t.description) ?? Concept(term: t)
                        var derived: [Judgement] = []
                        if isQuestion {
                            derived = concept.answer(s-?)
                        } else {
                            let j = j!
                            switch j.statement {
                            case .symbol: fallthrough // TODO: is this accurate?
                            case .compound:
                                derived = concept.accept(j, isSubject: concept.term == j.statement, derive: derive)
                            case .statement(let subject, _, _):
                                derived = concept.accept(j, isSubject: concept.term == subject, derive: derive)
                            case .variable:
                                derived = [] // TODO: is this accurate?
                            case .operation:
                                derived = [] // TODO: is this accurate?
                            }
                        }
                        derivedJudgements.append(contentsOf: derived)
                        concept.adjustPriority(derived)
                        put(concept)
                }
                if isQuestion {
                    print(derivedJudgements)
                    
                }
                return derivedJudgements
            }
            var concept = get(s.description) ?? Concept(term: s)
            let derived = f(&concept)
            derivedJudgements.append(contentsOf: derived)
            concept.adjustPriority(derived)
            put(concept)
            return derivedJudgements
        case .statement(let subject, _, let predicate):
            var subjectConcept = get(subject.description) ?? Concept(term: subject)
            var predicateConcept = get(predicate.description) ?? Concept(term: predicate)
            derivedJudgements.append(contentsOf: f(&subjectConcept))
            subjectConcept.adjustPriority(derivedJudgements)
            derivedJudgements.append(contentsOf: f(&predicateConcept))
            predicateConcept.adjustPriority(derivedJudgements)
            switch subject {
            case .statement: fallthrough
            case .compound:
                derivedJudgements.append(contentsOf: consider(subject, isQuestion: isQuestion, j: j, derive: derive, f))
            default: break
            }
            switch predicate {
            case .statement: fallthrough
            case .compound:
                derivedJudgements.append(contentsOf: consider(predicate, isQuestion: isQuestion, j: j, derive: derive, f))
            default: break
            }
            put(subjectConcept)
            put(predicateConcept)
            if isQuestion {
                
            }
            return derivedJudgements
        case .variable:
            return [] // TODO: is this accurate?
        case .operation:
            return [] // TODO: is this accurate?
        }
    }

    private func considerVar(_ t: Term, derive: Bool, _ f: (inout Concept) -> [Judgement]) -> [Judgement] {
        guard var concept = get(t.description) else { return [] }
        defer { put(concept) } // put back
        return f(&concept)
    }
    
    func contains(_ j: Judgement) -> Bool {
        let identifier = j.identifier
        switch j.statement {
        case .symbol(let word):
            if let c = peek(word) {
                return c.beliefs.peek(identifier) != nil
            }
        case .compound(let c, let ts):
            if c == .n, ts.count == 1 {
                return contains(ts[0]-*)
            }
//            return !ts.map { contains($0-*) }.contains(false) // TODO: finish implementation
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

