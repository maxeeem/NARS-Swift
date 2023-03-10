
extension AbstractBag where I == Concept {
    func consider(_ s: Sentence, derive: Bool) -> [Judgement] {
        switch s {
        case .judgement(let j):
            return consider(j.statement) { c in c.accept(j, derive: derive) }
        case .question(let q):
            return consider(q.statement, question: true) { c in c.answer(q.statement) }
        case .goal(let g):
            return consider(g, derive: derive) // TODO: finish implementation
        case .cycle: return []
        }
    }
    
    func consider(_ g: Goal, derive: Bool) -> [Judgement] {
        var derived = [Judgement]()
        let q: Question = ("?" >>|=> g.statement)-?
        derived.append(contentsOf: consider(.question(q), derive: derive))
        return derived.filter({ $0.statement != g.statement && $0.statement != q.statement })
    }
}

// MARK: Private

extension AbstractBag where I == Concept {
    // TODO: this could/should be an asynchronous method delegating processing to each concept independently
    private func consider(_ s: Statement, question: Bool = false, _ f: (inout Concept) -> [Judgement], recurse: Bool = true) -> [Judgement] {
        switch s {
        case .variable: return []
//        case .operation: return []
        case .symbol: if s == .º || s == .NULL { return [] }
        default: break
        }
        
        var derivedJudgements = [Judgement]()
        
        if var concept = get(s.description) {

            let derived = f(&concept)
            derivedJudgements.append(contentsOf: derived)
            concept.adjustPriority(derived)
            put(concept)
            
        } else {
            
            let matches = match(s)
            if !matches.isEmpty {
                for m in matches {
                    var concept = get(m.identifier)!
                    
                    let derived = f(&concept)
                    if derived.count == 1 {
                        derivedJudgements.append(contentsOf: derived)
                    } else {
                        let filtered = derived.filter({ !Term.logic_match(t1: $0.statement, t2: s) })
                        derivedJudgements.append(contentsOf: filtered)
                    }
                    concept.adjustPriority(derived)
                    put(concept)
                }
                
            } else if !question { // we don't want to create concepts for questions
                var concept = Concept(term: s)
                
                let derived = f(&concept)
                derivedJudgements.append(contentsOf: derived)
                concept.adjustPriority(derived)
                put(concept)
            }
        }
        
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
        // in case of a question we want to keep order with .NULL as separators
        // TODO: can remove duplicates within .NULL separate blocks
        return question ? derivedJudgements : derivedJudgements.removeDuplicates()
    }
    
    func match(_ item: Term) -> [Concept] {
        let matches = items.values.filter { it in
            if item != it.term {
                return Term.logic_match(t1: item, t2: it.term)
            }
            return false
        }
        return matches
    }
}
