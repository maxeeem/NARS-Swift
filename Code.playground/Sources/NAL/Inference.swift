
/// Local
public func revision(j1: Judgement, j2: Judgement) -> Judgement {
    let (f1, c1) = (j1.truthValue.f, j1.truthValue.c)
    let (f2, c2) = (j2.truthValue.f, j2.truthValue.c)
    let f = ((f1 * c1) * (1 - c2) + (f2 * c2) * (1 - c1)) / (c1 * (1 - c2) + c2 * (1 - c1))
    let c = (c1 * (1 - c2) + c2 * (1 - c1)) / (c1 * (1 - c2) + c2 * (1 - c1) + (1 - c1) * (1 - c2))
    return Judgement(j1.statement, TruthValue(f, c))
}

public func choice(j1: Judgement, j2: Judgement) -> Judgement {
    j1.statement == j2.statement ?
        (j1.truthValue.c > j2.truthValue.c) ? j1 : j2
    :
        (and(j1.truthValue.e, j1.statement.simplicity) 
            > 
            and(j2.truthValue.e, j1.statement.simplicity)) ? j1 : j2
}

public func conversion(j1: Judgement) -> Judgement? {
    guard case .statement(let s, let copula, let p) = j1.statement,
          copula == .inheritance || copula == .implication else {
        return nil // invalid statement
    }
    let (f, c) = (j1.truthValue.f, j1.truthValue.c)
    let c1 = f * c / (f * c + k)
//    let w = and(f, c)
//    let c1 = w2c(w)
    return .statement(p, copula, s)-*(1, c1)
}

public func w2c(_ w: Double) -> Double {
    w / (w + evidentialHorizon)
}

extension Rules {
    public var allRules: [Rule] {
        let rules = /*local +*/ firstOrder + higherOrder + conditional
        var permutations: [Rule] = []
        for r in rules {
            let (p1, p2, c, tf) = r
            var sp1: Statement!
            var sp2: Statement!
            if case .statement(let s, let copula, let p) = p1 {
                if copula == .similarity || copula == .equivalence {
                    sp1 = .statement(p, copula, s)
                }
            }
            if case .statement(let s, let copula, let p) = p2 {
                if copula == .similarity || copula == .equivalence {
                    sp2 = .statement(p, copula, s)
                }
            }
            if sp1 != nil {
                permutations.append((sp1, p2, c, tf))
            }
            if sp2 != nil {
                permutations.append((p1, sp2, c, tf))
            }
            if sp1 != nil && sp2 != nil {
                permutations.append((sp1, sp2, c, tf))
            }
        }
        return rules + permutations
    }
//    var local: [Rule] {
//        let S = Term.word("S")
//        let P = Term.word("P")
//        switch self {
//        case .similarityFromReversedInheritance:
//            return []//(S --> P,     P --> S, S <-> P, tf)]
//        case .inheritanceFromSimilarityAndReversedInheritance:
//            return []//(S <-> P,     P --> S, S --> P, tf)]
//        default:
//            return []
//        }
//    }
    var higherOrder: [Rule] {
        return firstOrder.map { (arg) in
            var (p1, p2, c, tf) = arg
            p1 = replaceCopulas(p1)
            p2 = replaceCopulas(p2)
            c = replaceCopulas(c)
            return (p1, p2, c, tf)
        }
    }
    var firstOrder: [Rule] {
        let S = Term.word("S")
        let P = Term.word("P")
        let M = Term.word("M")
        let T1 = Term.word("T1")
        let T2 = Term.word("T2")
        /// first unique term is `false`, second and third are `nil`
        /// if there is no common term term identified by `true`
        /// then a conclusion could not be derived
        switch self {
        case .deduction:
            ///    true, false, nil, true
            return [(M --> P,     S --> M, S --> P, tf),
                    (P --> M,     M --> S, P --> S, tfi)]
        case .induction:
            return [(M --> P,     M --> S, S --> P, tf),
                    (M --> P,     M --> S, P --> S, tfi)]
        case .abduction:
            return [(P --> M,     S --> M, S --> P, tf),
                    (P --> M,     S --> M, P --> S, tfi)]
        case .exemplification:
            return [(P --> M,     M --> S, S --> P, tf),
                    (M --> P,     S --> M, P --> S, tfi)]
        case .comparison:
            return [(M --> P,     M --> S, S <-> P, tf),
                    (P --> M,     S --> M, S <-> P, tfi)]
        case .analogy:
            return [(M --> P,     S <-> M, S --> P, tf),
                    (P --> M,     S <-> M, P --> S, tf),
                    (M <-> P,     S --> M, S --> P, tfi),
                    (M <-> P,     M --> S, P --> S, tfi)]
        case .resemblance:
            return [(M <-> P,     S <-> M, S <-> P, tf)]
//                    (M <-> P,     M <-> S, S <-> P, tf),
//                    (P <-> M,     S <-> M, S <-> P, tf),
//                    (P <-> M,     M <-> S, S <-> P, tf)]
            
        // Compositional rules
        
        case .intersection:
            return [(M --> T1,    M --> T2,    M --> ç.Ω_(T1, T2), tf),
                    (T1 --> M,    T2 --> M,    ç.U_(T1, T2) --> M, tf)]
        case .union:
            return [(M --> T1,    M --> T2,    M --> ç.U_(T1, T2), tf),
                    (T1 --> M,    T2 --> M,    ç.Ω_(T1, T2) --> M, tf)]
        case .difference:
            return [(M --> T1,    M --> T2,    M --> ç.l_(T1, T2), tf),
                    (M --> T1,    M --> T2,    M --> ç.l_(T2, T1), tfi),
                    (T1 --> M,    T2 --> M,    ç.ø_(T1, T2) --> M, tf),
                    (T1 --> M,    T2 --> M,    ç.ø_(T2, T1) --> M, tfi)]
        default:
            return [] // other rules are handled separately
        }
    }
    var conditional: [Rule] {
        let S = Term.word("S")
        let P = Term.word("P")
        let T1 = Term.word("T1")
        let T2 = Term.word("T2")
        switch self {
        case .deduction:
            return [(S  => P,           S,       P, tf)]
        case .induction:
            return []//(      P,           S, S  => P, tf)]
        case .abduction:
            return [(P  => S,           S,       P, tf)]
        case .comparison:
            return []//(      S,           P, S <=> P, tf)]
        case .analogy:
            return [(      S,     S <=> P,       P, tf)]
        case .intersection:
            return [(     T1,          T2,       .compound(.c, [T1, T2]), tf)]
        default: 
            return []
        }
    }
    
    private func replaceCopulas(_ statement: Statement) -> Statement {
        var statement = statement
        if case .statement(let s, let c, let p) = statement {
            if c == .inheritance {
                statement = .statement(s, .implication, p)
            }
            if c == .similarity {
                statement = .statement(s, .equivalence, p)
            }
        }
        return statement
    }
}

extension Theorems {
    public var rules: [Theorem] {
        switch self {
        case .inheritance:
            return [
                { var t: Statement?
                    match(.compound(.Ω, ["T1", "T2"]), $0, compound: { T1, T2 in
                        t = (.compound(.Ω, [T1, T2])) --> (T1)
                    }); return t
                },
                { var t: Statement?
                    match(.compound(.l, ["T1", "T2"]), $0, compound: { T1, T2 in
                        t = (.compound(.l, [T1, T2])) --> (T1)
                    }); return t
                }
            ]
        case .implication:
            return [
                { var t: Statement?
                    match("S" <-> "P", $0, extractCompound: false, statement: { S, P in
                        t = (S <-> P) => (S --> P)
                    }); return t
                },
                { var t: Statement?
                    match("S" <=> "P", $0, statement: { S, P in
                        t = (S <=> P) => (S => P)
                    }); return t
                },
                { var t: Statement?
                    match(.compound(.c, ["S1", "S2"]), $0, compound: { S1, S2 in
                        t = .compound(.c, [S1, S2]) => (S1)
                    }); return t
                }
            ]
        case .equivalence:
            return [
                { var t: Statement?
                    match("S" <-> "P", $0, statement: { S, P in
                        t = (S <-> P) <=> .compound(.c, [(S --> P), (P --> S)])
                    }); return t
                },
                
                
                
                { var t: Statement?
                    //if $0.description.contains("{Birdie} -> {Tweety} ∧ {Tweety} -> {Birdie}") {
                        
                    //}
                    match(.compound(.c, [("S" --> "P"), ("P" --> "S")]), $0, compound: { S, P in
                        t = (S <-> P) <=> .compound(.c, [(S --> P), (P --> S)])
//                        t = .compound(.c, [(S --> P), (P --> S)]) <=> (S <-> P)
                    }); return t
                },
                
                
                
//                { var t: Statement? // TODO: rewrite using conjunction
//                    match("S" <-> "P", $0, statement: { S, P in
//                        t = (S <-> P) <=> (P --> S)
//                    }); return t
//                },
                { var t: Statement?
                    match("S" <=> "P", $0, statement: { S, P in
                        t = (S <=> P) <=> .compound(.c, [(S => P), (P => S)])
                    }); return t
                },
//                { var t: Statement? // TODO: rewrite using conjunction
//                    match("S" <=> "P", $0, statement: { S, P in
//                        t = (S <=> P) <=> (P => S)
//                    }); return t
//                },
                { var t: Statement?
                    match("S" <-> "P", $0, statement: { S, P in
//                        print("here1", S, P)
                        t = (S <-> P) <=> (.instance(S) <-> .instance(P))
                    }); return t
                },
                
                
                
                
                { var t: Statement?
                    match("{S}" <-> "{P}", $0, statement: { S, P in
//                        print("here2", S, P)
                        t = (S <-> P) <=> (.instance(S) <-> .instance(P))
                    }); return t
                },
                
                
                
                
//                { var t: Statement?
//                    match("{S}" <-> "{P}", $0, statement: { S, P in
//                        t = (.instance(S) <-> .instance(P)) <=> (S <-> P)
//                    }); return t
//                },
                
                { var t: Statement?
                    match("S" <-> "P", $0, statement: { S, P in
                        t = (S <-> P) <=> (.property(S) <-> .property(P))
                    }); return t
                },
                
                { var t: Statement?
                    match("[S]" <-> "[P]", $0, statement: { S, P in
                        t = (S <-> P) <=> (.property(S) <-> .property(P))
                    }); return t
                },
                
                
                
                
                
                
                { var t: Statement?
                    match("S" --> "{P}", $0, statement: { S, P in
                        t = (S --> .instance(P)) <=> (S <-> .instance(P))
                    }); return t
                },
                { var t: Statement?
                    match("[S]" --> "P", $0, statement: { S, P in
                        t = (.property(S) --> P) <=> (.property(S) <-> P)
                    }); return t
                }
            ]
        }
    }
}

// MARK: Helpers
@discardableResult
private func match(_ lhs: Statement, _ rhs: Statement,
           extractCompound: Bool = true, // TODO: come up with a better way
           statement: (Term, Term) -> Void = {_,_ in },
           compound: (Term, Term) -> Void = {_,_ in })
-> Bool {
    if case .word = lhs, case .word = rhs {
        return true
    }
    if case .compound(let cl, let tl) = lhs,
       case .compound(let cr, let tr) = rhs {
        if cl == cr {
            if tr.count == 1 { // instance/property
                compound(tr[0], .NULL)
            } else { // TODO: expand to more than two terms
                if case .statement = tr[0], case .statement = tr[1] {
                    match(tr[1], tr[0], extractCompound: false, statement: { s, p in
                        compound(s, p)
                    })
                } else {
                    compound(tr[0], tr[1])
                }
            }
            return true
        }
        return false
    }
    if case .statement(let sl, let cl, let pl) = lhs,
        case .statement(let sr, let cr, let pr) = rhs {
        if match(sl, sr) && cl == cr && match(pl, pr) {
//            print("one")
            if extractCompound { // TODO: come up with a better way
//                print("a", sr, pr)
                //print(extract(sr), extract(pr))
                statement(extract(sr), extract(pr))
            } else {
                statement(sr, pr)
            }
            return true
        } else if match(sl, pl) && cl == cr && match(sr, pr) {
//            statement(sr, pr) // TODO: come up with better implementation
            if extractCompound { // TODO: come up with a better way
                //                print("a", sr, pr)
                //print(extract(sr), extract(pr))
                statement(extract(sr), extract(pr))
            } else {
                statement(sr, pr)
            }
            return true
        }
        return false
    }
    return false
}

private func extract(_ term: Term) -> Term {
    // extract instance/property
    if case .compound(let c, let ts) = term,
       ts.count == 1 && (c == .intSet || c == .extSet) {
        return ts[0]
    }
    return term
}
