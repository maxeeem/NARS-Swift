
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
        local + firstOrder + higherOrder + conditional
    }
    var local: [Rule] {
        let S = Term.word("S")
        let P = Term.word("P")
        switch self {
        case .similarityFromReversedInheritance:
            return [(S --> P,     P --> S, S <-> P, tf)]
        case .inheritanceFromSimilarityAndReversedInheritance:
            return [(S <-> P,     P --> S, S --> P, tf)]
        default:
            return []
        }
    }
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
            return [(M <-> P,     S <-> M, S <-> P, tf),
                    (M <-> P,     M <-> S, S <-> P, tf),
                    (P <-> M,     S <-> M, S <-> P, tf),
                    (P <-> M,     M <-> S, S <-> P, tf)]
            
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

extension Teoremas {
    public var rules: [Teorema] {
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
                    match("S" <-> "P", $0, statement: { S, P in
                        t = (S <-> P) => (S --> P)
                    }); return t
                },
                { var t: Statement?
                    match("S" <=> "P", $0, statement: { S, P in
                        t = (S <=> P) => (S => P)
                    }); return t
                }
            ]
        case .equivalence:
            return [
                { var t: Statement? // TODO: rewrite using conjunction
                    match("S" <-> "P", $0, statement: { S, P in
                        t = (S <-> P) <=> (S --> P)
                    }); return t
                },
                { var t: Statement? // TODO: rewrite using conjunction
                    match("S" <-> "P", $0, statement: { S, P in
                        t = (S <-> P) <=> (P --> S)
                    }); return t
                },
                { var t: Statement? // TODO: rewrite using conjunction
                    match("S" <=> "P", $0, statement: { S, P in
                        t = (S <=> P) <=> (S => P)
                    }); return t
                },
                { var t: Statement? // TODO: rewrite using conjunction
                    match("S" <=> "P", $0, statement: { S, P in
                        t = (S <=> P) <=> (P => S)
                    }); return t
                },
                { var t: Statement?
                    match("S" <-> "P", $0, statement: { S, P in
                        t = (S <-> P) <=> (.instance(S) <-> .instance(P))
                    }); return t
                },
                { var t: Statement?
                    match("S" <-> "P", $0, statement: { S, P in
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
                compound(tr[0], tr[1])
            }
            return true
        }
        return false
    }
    if case .statement(let sl, let cl, let pl) = lhs,
        case .statement(let sr, let cr, let pr) = rhs {
        if match(sl, sr) && cl == cr && match(pl, pr) {
            statement(extract(sr), extract(pr))
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

/*
extension Theorems {
    public var theorem: [Theorem] {
        let S = Term.word("S")
        let P = Term.word("P")

        switch self {
        case .negation:
            return []
        case .conversion:
            return [(S --> P,   P --> S, tf)]
        case .contraposition:
            return []
            
            
        case .inheritance:
            return []
        case .similarity:
            return []
        case .implication:
            return [(S --> P,   S <-> P, tf),
                    (S --> P,   P <-> S, tf),
//                    (S <-> P,   S --> P, tf), // similiarity is symmetrical
                    (S <-> P,   P --> S, tf)]//, // similiarity is symmetrical
//                    (S <-> P,   P <-> S, tf)]
        case .equavalence:
//            return []
            return []//(S <-> P,   S•-> <-> P•->, tf),
                    //(S <-> P,   ->•S <-> ->•P, tf),
//                    (S --> P•->,   S <-> P•->, tf),
//                    (->•S --> P,   ->•S <-> P, tf)]
        }
    }
}
*/
