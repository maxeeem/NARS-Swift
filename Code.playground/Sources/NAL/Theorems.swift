
public typealias Theorem = (Statement) -> Statement?

public enum Theorems: CaseIterable {
    case inheritance
    case similarity
    case implication
    case equivalence
}

extension Theorems {
    public static func apply(_ j: Judgement) -> [Judgement] {
        let res: [[Statement]] = self.allCases.map {
            var results = $0.rules.compactMap { Term.match(t: $0, s: j.statement) }
            if case .statement(let s, let c, let p) = j.statement, c == .similarity || c == .equivalence {
                results.append(contentsOf: $0.rules.compactMap { Term.match(t: $0, s: .statement(p, c, s)) })
            }
            if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
                if terms.count == 2 { // TODO: handle compounds with multiple terms
                    results.append(contentsOf: $0.rules.compactMap { Term.match(t: $0, s: .compound(conn, terms.reversed())) })
                }
            }
            return results
        }

        let results: [[Judgement]] = res.flatMap{$0}.map { t in
            var rel = reliance
            if case .statement(let sub, let cop, _) = t, cop == .equivalence {
                rel = j.statement == sub ? 0.9 : 1.0
            }
            var results = Rules.strong.flatMap {
                $0.apply((j, t-*(1,rel, ETERNAL)))
            }.compactMap { $0 }
            
            if case .statement(let s, let c, let p) = j.statement, c == .similarity || c == .equivalence {
                results.append(contentsOf:
                    Rules.strong.flatMap {
                    $0.apply((Judgement(.statement(p, c, s), j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp), t-*(1,reliance, ETERNAL)))
                    }.compactMap { $0 }
               )
            }
            if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
                if terms.count == 2 { // TODO: handle compounds with multiple terms
                    results.append(contentsOf:
                        Rules.strong.flatMap {
                        $0.apply((Judgement(.compound(conn, terms.reversed()), j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp), t-*(1,reliance, ETERNAL)))
                        }.compactMap { $0 }
                   )
                }
            }
            return results
        }
        
        let unique = Dictionary(grouping: results.flatMap({$0})) {
            $0.identifier
        }.values.compactMap {
            $0.max { j1, j2 in
                let j = choice(j1: j1, j2: j2)
                return j.statement == j2.statement
            }
        }
        
        return unique
    }
}


// MARK: - Helper

extension Term {
    static func match(t: Statement, s: Statement) -> Statement? {
        var results = [Term]()
        let goal = t.terms.map({ $0.logic() === s.logic() }).reduce(success, ||)
        
        for sol in solve(goal) {
            //                print(sol)
            let ts = s.terms.flatMap({ $0.terms.map({ $0.logic() }) })
            
            let valid = sol.allSatisfy { (v, _) in
                !ts.contains { $0.equals(v) }
            }
            
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
}
