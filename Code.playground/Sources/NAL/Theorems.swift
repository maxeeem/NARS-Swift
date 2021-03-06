

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
            var results = $0.rules.compactMap { $0(j.statement) }
            if case .statement(let s, let c, let p) = j.statement, c == .similarity || c == .equivalence {
                results.append(contentsOf: $0.rules.compactMap { $0(.statement(p, c, s)) })
            }
            if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
                if terms.count == 2 { // TODO: handle compounds with multiple terms
                    results.append(contentsOf: $0.rules.compactMap { $0(.compound(conn, terms.reversed())) })
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
                        $0.apply((Judgement(.statement(p, c, s), j.truthValue, j.derivationPath), t-*(1,reliance, ETERNAL)))
                    }.compactMap { $0 }
               )
            }
            if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
                if terms.count == 2 { // TODO: handle compounds with multiple terms
                    results.append(contentsOf:
                        Rules.strong.flatMap {
                            $0.apply((Judgement(.compound(conn, terms.reversed()), j.truthValue, j.derivationPath), t-*(1,reliance, ETERNAL)))
                        }.compactMap { $0 }
                   )
                }
            }
            return results
        }
        
        let unique = Dictionary(grouping: results.flatMap({$0})) {
            $0.statement.description
        }.values.compactMap {
            $0.max { j1, j2 in
                let j = choice(j1: j1, j2: j2)
                return j.statement == j2.statement
            }
        }
        
        return unique
    }
}
