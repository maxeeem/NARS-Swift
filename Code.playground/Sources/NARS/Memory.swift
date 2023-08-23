
extension AbstractBag where I == Concept {

    func consider(_ s: Sentence, derive: Bool) -> [Judgement] {
        switch s {
        case .judgement(let j):
            var derived: [Judgement] = []
            
            func addTask(_ s: Sentence, to: Statement) {
                let concept = get(to.description) ?? Concept(term: to)
                
                // revision
                
                if let revised = concept.accept(belief: j) {
                    derived.append(revised)
                }
                
                if derive { // pre-process task
                    
                    if let oldTask = concept.tasks.get(s.description) { // remove old
                        concept.tasks.put(
                            Task(priority: oldTask.priority, sentence: s))
                    } else {
                        concept.tasks.put(Task(sentence: s))
                    }
                    
                    // local inference
                    
                    let immediate = Rules.immediate(j)
                    let structural = Theorems.apply(j)
                    let results = (immediate + structural)
                        .removeDuplicates()
                        .filter({ $0.truthValue.confidence != 0 })
                    
                    derived.append(contentsOf: results)
                }
                    
                // storage
                put(concept)
            }
            
//            addTask(s, to: j.statement)

            for t in Set(j.statement.terms) {
                addTask(s, to: t)

                for t1 in Set(Term.getTerms(t)) {
                    if t1 != t {
                        addTask(s, to: t1)
                    }
                }
            }
            
            return derived.removeDuplicates().filter({ $0.truthValue.confidence != 0 })
            
        case .question(let q):
            var answers: [Judgement] = []
            
            func addTask(_ s: Sentence, to: Statement) {
                let concept = get(to.description) ?? Concept(term: to)
                concept.tasks.put(Task(sentence: s))

                if derive {
                    let results = concept.answer(q.statement)
                    answers.append(contentsOf: results)
                }
                put(concept)
            }
            
            for t in Set(q.statement.terms) {
                addTask(s, to: t)
                
                for t1 in Set(Term.getTerms(t)) {
                    if t1 != t {
                        addTask(s, to: t1)
                    }
                }
            }

            return answers

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
