public typealias Theorem = (Statement) -> Statement?

public enum Theorems: CaseIterable {
    case inheritance
    case similarity
    case implication
    case equivalence
}

extension Theorems {
    public static var cache: [String: [Judgement]] = [:] // TODO: do this properly
    public static func apply(_ j: Judgement) -> [Judgement] {
        if cache[j.identifier] == nil {
            cache[j.identifier] = _apply(j)
        }
        return cache[j.identifier]!
    }
    public static func _apply(_ j: Judgement) -> [Judgement] {
        let res: [[Statement]] = self.allCases.map {
            var results = $0.rules.flatMap { t in [Term.match(t: t, s: j.statement)] + j.statement.terms.map({ s in Term.match(t: t, s: s) }) }.compactMap({$0})
            if case .statement(let s, let c, let p) = j.statement, c == .similarity || c == .equivalence {
                results.append(contentsOf: $0.rules.compactMap { Term.match(t: $0, s: .statement(p, c, s)) })
            }
            if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
                results.append(contentsOf: $0.rules.compactMap { Term.match(t: $0, s: .compound(conn, terms.reversed())) })
            }
            return results
        }

        let results: [[Judgement]] = res.flatMap{$0}.map { t in
            var results = Rules.strong.flatMap {
                $0.apply((j, t-*(1.0, reliance, ETERNAL)))
            }.compactMap { $0 }
            
            if case .statement(let s, let c, let p) = j.statement, c == .similarity || c == .equivalence {
                results.append(contentsOf:
                    Rules.strong.flatMap {
                    $0.apply((Judgement(.statement(p, c, s), j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp), t-*(1.0 ,reliance, ETERNAL)))
                    }.compactMap { $0 }
               )
            }
            if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
                results.append(contentsOf:
                    Rules.strong.flatMap {
                    $0.apply((Judgement(.compound(conn, terms.reversed()), j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp), t-*(1.0 ,reliance, ETERNAL)))
                    }.compactMap { $0 }
                )
            }
            return results
        }

        let unique = results.flatMap({$0})/*.filter({$0.truthValue.rule != nil})*/.removeDuplicates()
        return unique
    }
}


// MARK: - Helper

extension Term {
    static func match(t: Statement, s: Statement) -> Statement? {
        var results = [Term]()
        
        let goal = t.terms.map({ $0.logic() === s.logic() }).reduce(success, ||)
        
        for sol in solve(goal) {
            let solutionVars = sol.map({ $0.LogicVariable.name })
            let theoremVars = Term.getTerms(t).filter({ if case .variable = $0 { return true } else { return false } }).map({$0.description})
            let valid = Set(solutionVars) == Set(theoremVars)
       
            if valid {
                var result = t
                for item in sol {
                    result = result.replace(termName: item.LogicVariable.name, term: .from(logic: item.LogicTerm))
                }
                if result != t {
                    results.append(result)
                }
            }
        }
        
        return results.min(by: { $0.complexity < $1.complexity })
    }
    
    
    static func match_backward(t: Statement, s: Statement, r: Statement) -> Statement? {
        var results = [Term]()

        for sol in solve(t.logic() === s.logic()) {
            let solutionVars = sol.map({ $0.LogicVariable.name })
            let theoremVars = Term.getTerms(t).filter({ if case .variable = $0 { return true } else { return false } }).map({$0.description})
            let valid = Set(solutionVars) == Set(theoremVars)
       
            if valid {
                var result = r
                for item in sol {
                    result = result.replace(termName: item.LogicVariable.name, term: .from(logic: item.LogicTerm))
                }
                if result != r {
                    results.append(result)
                }
            }
        }
        
        return results.min(by: { $0.complexity < $1.complexity })
    }
}
