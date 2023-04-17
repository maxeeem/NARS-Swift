
extension AbstractBag where I == Concept {
    func consider(_ s: Sentence, derive: Bool) -> [Judgement] {
        switch s {
        case .judgement(let j):
            // recurse here means processing elements of judgement
            return consider(j.statement, recurse: true) { c, s in c.accept(j, derive: derive, store: s) }
        case .question(let q):
            return consider(q.statement, true, recurse: derive) { c, _ in c.answer(q.statement) }
        case .goal(let g):
            return consider(g, derive: derive) // TODO: finish implementation
        case .cycle: return []
        }
    }
    
    func consider(_ g: Goal, derive: Bool) -> [Judgement] {
        var derived = [Judgement]()
        let q: Question = ("?" >>|=> g.statement)-?
        derived.append(contentsOf: consider(.question(q), derive: derive)) // TODO: should recurse be disabled for goal questions?
        return derived.filter({ $0.statement != g.statement && $0.statement != q.statement })
    }
}

// MARK: Private

extension AbstractBag where I == Concept {
    // TODO: this could/should be an asynchronous method delegating processing to each concept independently
    private func consider(_ s: Statement, _ question: Bool = false, recurse: Bool = true, _ f: (inout Concept, _ store: Bool) -> [Judgement]) -> [Judgement] {
        switch s {
        case .variable: return []
//        case .operation: return []
        case .symbol: if s == .º || s == .NULL { return [] }
        default: break
        }
        
        var derivedJudgements = [Judgement]()
        
        if var concept = get(s.description) {

            let derived = f(&concept, true)
            derivedJudgements.append(contentsOf: derived)
            concept.adjustPriority(derived)
            put(concept)
            
        } else {
            
            let matches = match(s)
            if !matches.isEmpty {
                for m in matches {
                    var concept = get(m.identifier)!
                    var derived: [Judgement]
                    if recurse {
                        derived = f(&concept, false)
                    } else { // TODO: pass real truth value for `s` instead of using default
                        derived = question ? concept.answer(s) : concept.accept(s-*, derive: true, store: false)
                    }
                    derivedJudgements.append(contentsOf: derived)
                    concept.adjustPriority(derived)
                    put(concept)
                }
                
            }
        }
        
        if recurse { // TODO: make into a recursive function instead of hardcoding 3 levels
            if s.terms != [s] {
                let terms = Set(s.terms)
                for t in terms {
                    let derived = consider(t, question, recurse: false, f)
                    derivedJudgements.append(contentsOf: derived)
                }
                let t1terms = Set(terms.flatMap({$0.terms}))
                for t1 in t1terms.subtracting(terms) {
                    let derived = consider(t1, question, recurse: false, f)
                    derivedJudgements.append(contentsOf: derived)
                }
                let t2terms = Set(t1terms.flatMap({$0.terms}))
                for t2 in t2terms.subtracting(t1terms) {
                    let derived = consider(t2, question, recurse: false, f)
                    derivedJudgements.append(contentsOf: derived)
                }
            }
        }
        
        if question { // we don't want to create concepts for questions
            
            // TODO: MAYBE: if question was instantiating a variable, filter out
            
            // remove duplicates within .NULL separated blocks
            if derivedJudgements.contains(.NULL-*) {
                var result: [[Judgement]] = []
                for chunk in derivedJudgements.split(separator: .NULL-*) {
                    var filtered = Array(chunk).removeDuplicates()
                    if let root = filtered.firstIndex(where: {$0 == chunk.first}) {
                        filtered.swapAt(filtered.startIndex, root)
                    }
                    result.append(filtered)
                }
                return result.flatMap({ [.NULL-*] + $0 })
            } else {
                return derivedJudgements.removeDuplicates()
            }
        }
        
        var concept = get(s.description) ?? Concept(term: s)

        let derived = f(&concept, true)
        derivedJudgements.append(contentsOf: derived)
        concept.adjustPriority(derived)
        put(concept)
        
        derivedJudgements = derivedJudgements.removeDuplicates()
        
        var otras = [Judgement]()
        let rel = derivedJudgements.compactMap {
            if case .statement(let sub, let cop, let pre) = $0.statement, cop == .inheritance {
                if case .compound(let con, let ts) = pre, con == .e, ts.count == 3, ts[0] == .represent, ts[1] == .º {
                    return Judgement(sub <-> ts[2], $0.truthValue)
                }
            }
            otras.append($0)
            return nil
        }
        
        for r in rel {
            for o in otras {
                let repl = Rules.analogy.apply((o, r)).compactMap({$0})
                derivedJudgements.append(contentsOf: repl)
            }
        }
        
        return derivedJudgements.removeDuplicates()
    }
    
    func match(_ item: Term) -> [Concept] {
        let matches = items.values.filter { it in
            if item != it.term, Set(Term.getTerms(item)) != Set(Term.getTerms(it.term)) {
                return Term.logic_match(t1: item, t2: it.term)
            }
            return false
        }
        return matches
    }
}
