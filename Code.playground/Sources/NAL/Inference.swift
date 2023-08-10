
/// Local
///
// TODO: handle variables properly
// independent #x can be merged with independent #y
//
public func revision(j1: Judgement, j2: Judgement) -> Judgement {
    let (f1, c1) = (j1.truthValue.f, j1.truthValue.c)
    let (f2, c2) = (j2.truthValue.f, j2.truthValue.c)
    let f = ((f1 * c1) * (1 - c2) + (f2 * c2) * (1 - c1)) / (c1 * (1 - c2) + c2 * (1 - c1))
    let c = (c1 * (1 - c2) + c2 * (1 - c1)) / (c1 * (1 - c2) + c2 * (1 - c1) + (1 - c1) * (1 - c2))
    return Judgement(j1.statement, TruthValue(f, c, .revision), Judgement.mergeEvidence(j1, j2))
}

public func choice(j1: Judgement, j2: Judgement) -> Judgement {
    j1.statement == j2.statement ?
        (j1.truthValue.c > j2.truthValue.c) ? j1 : j2
    :
        (and(j1.truthValue.e, j1.statement.simplicity) 
            > 
            and(j2.truthValue.e, j2.statement.simplicity)) ? j1 : j2
}

/// Immediate

public func negation(j1: Judgement) -> Judgement? {
    if j1.truthValue.rule == .negation {
        return nil // avoid cyclic conversion
    }
    let f = 1 - j1.truthValue.f
    let c = j1.truthValue.c
    let cs = neg(j1.statement)
    let cj = cs + (f, c, ETERNAL)
    return Judgement(cs, TruthValue(f, c, .negation), Judgement.mergeEvidence(j1, cj), tense: j1.tense, timestamp: j1.timestamp)
}

public func conversion(j1: Judgement) -> Judgement? {
    if j1.truthValue.rule == .conversion {
        return nil // avoid cyclic conversion
    }
    guard case .statement(let s, let copula, let p) = j1.statement,
          copula == .inheritance || copula == .implication else {
        return nil // invalid statement
    }
    let (f, c) = (j1.truthValue.f, j1.truthValue.c)
    let c1 = f * c / (f * c + k)
    let cs = Term.statement(p, copula, s)
    let cj = cs + (1, c1, ETERNAL)
    return Judgement(cs, TruthValue(1, c1, .conversion), Judgement.mergeEvidence(j1, cj), tense: j1.tense, timestamp: j1.timestamp)
}

public func contraposition(j1: Judgement) -> Judgement? {
    if j1.truthValue.rule == .contraposition {
        return nil // avoid cyclic conversion
    }
    guard case .statement(let s, let copula, let p) = j1.statement,
          copula == .implication else {
        return nil // invalid statement
    }
    let (f, c) = (j1.truthValue.f, j1.truthValue.c)
    let c1 = (f == 1) ? 0 : (1 - f) * c / ((1 - f) * (c + k))
    let cs = neg(p) => neg(s)
    let cj = cs + (0, c1, ETERNAL)
    return Judgement(cs, TruthValue(0, c1, .contraposition), Judgement.mergeEvidence(j1, cj), tense: j1.tense, timestamp: j1.timestamp)
}

private func neg(_ s: Statement) -> Statement {
    if case .compound(let conn, let terms) = s, conn == .n, terms.count == 1 {
        return terms[0] // double negative
    } else {
        return .compound(.n, [s])
    }
}

public extension Rules {

    var allRules: [Rule] {
        let rules = firstOrder + higherOrder + compositional + conditionalSyllogistic
        return rules + permutations(rules) + decomposition
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
        let S = Term.var("S")
        let P = Term.var("P")
        let M = Term.var("M")

        switch self {
        case .deduction:
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
            
        default:
            return [] // other rules are handled separately
        }
    }
    
    var compositional: [Rule] {
        let M = Term.var("M")
        let T1 = Term.var("T1")
        let T2 = Term.var("T2")
        
        switch self {
        case .intersection:
            return [ /// first order
                (M --> T1,    M --> T2,    M --> (T1 & T2), tf),
                (T1 --> M,    T2 --> M,    (T1 | T2) --> M, tf),
                /// higher order
                ( M => T1,    M => T2 ,    M => (T1 && T2), tf),
                ( T1 => M,    T2 => M ,    (T1 || T2) --> M, tf),
                /// conditional
                (T1 --> T2,   T2 --> T1,   ((T1 --> T2) && (T2 --> T1)), tf) // TODO: verify nothing else needs to be checked
            ]
        case .union:
            return [ /// first order
                (M --> T1,    M --> T2,    M --> (T1 | T2), tf),
                (T1 --> M,    T2 --> M,    (T1 & T2) --> M, tf),
                /// higher order
                ( M => T1,    M => T2 ,    M => (T1 || T2), tf),
                ( T1 => M,    T2 => M ,    (T1 && T2) --> M, tf),
                /// conditional
                (T1 --> T2,   T2 --> T1,   ((T1 --> T2) || (T2 --> T1)), tf) // TODO: verify nothing else needs to be checked
            ]
        case .difference:
            return [
                (M --> T1,    M --> T2,    M --> (T1 - T2), tf),
                (M --> T1,    M --> T2,    M --> (T2 - T1), tfi),
                (T1 --> M,    T2 --> M,    (T1 ~ T2) --> M, tf),
                (T1 --> M,    T2 --> M,    (T2 ~ T1) --> M, tfi)
            ]
        default:
            return []
        }
    }
    
    var decomposition: [Rule] {
        let M = Term.var("M")
        let T1 = Term.var("T1")
        let T2 = Term.var("T2")

        switch self {
        case .deduction:
            return [
                (-(M --> (T1 & T2)),     (M --> T1),    -(M --> T2), tf),
                ( (M --> (T1 | T2)),    -(M --> T1),     (M --> T2), tf),
                (-(M --> (T1 - T2)),     (M --> T1),     (M --> T2), tf),
                (-(M --> (T2 - T1)),    -(M --> T1),    -(M --> T2), tf),
                (-((T2 | T1) --> M),     (T1 --> M),    -(T2 --> M), tf),
                ( ((T2 & T1) --> M),    -(T1 --> M),     (T2 --> M), tf),
                (-((T1 ~ T2) --> M),     (T1 --> M),     (T2 --> M), tf),
                (-((T2 ~ T1) --> M),    -(T1 --> M),    -(T2 --> M), tf),
                (       -(T1 && T2),           (T1),          -(T2), tf),
                (        (T1 || T2),          -(T1),           (T2), tf)
            ]
        default:
            return []
        }
    }
    
    var conditionalSyllogistic: [Rule] {
        let S = Term.var("S")
        let P = Term.var("P")
        switch self {
        case .deduction:
            return [(S  => P,           S,       P, tf)]
        case .abduction:
            return [(P  => S,           S,       P, tf)]
        case .analogy:
            return [(      S,     S <=> P,       P, tf)]
        default:
            return []
        }
    }
    
    /// special set of rules handled separately during inference
    /// premises must be seen as based on the same implicit condition
    
    var conditional: [Rule] {
        let S = Term.var("S")
        let P = Term.var("P")
        let M = Term.var("M")
        let C = Term.var("C")
        let T1 = Term.var("T1")
        let T2 = Term.var("T2")
        switch self {
        case .deduction:
            return [
                ((C && S) => P,                 S,             C  => P, tf),
                ((C && S) => P,            M => S,        (C && M) => P, tf)
            ]
        case .abduction:
            return [
                ((C && S) => P,            C => P,                   S, tf),
                ((C && S) => P,     (C && M) => P,              M => S, tf)
            ]
        case .induction:
            return [
                (       C => P,                 S,     (C && S) => P, tf),
                ((C && M) => P,            M => S,     (C && S) => P, tf)
            ]
        case .intersection:
            return [
                (             T1,                T2,        (T1 && T2), tf) // TODO: verify nothing else needs to be checked
            ]
        case .union:
            return [
                (             T1,                T2,        (T1 || T2), tf) // TODO: verify nothing else needs to be checked
            ]
        default:
            return []
        }
    }
    
    var variable_and_temporal: [Rule] {
        let S = Term.var("S")
        let P = Term.var("P")
        switch self {
        case .induction:
            return [(P,  S,  S  => P, tf)]
        case .comparison:
            return [(S,  P,  S <=> P, tf)]
        default:
            return []
        }
    }
    
    // Private
    
    private func permutations(_ rules: [Rule]) -> [Rule] {
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
        return permutations
    }
}

extension Theorems {
    public var rules: [Statement] {
        let S = Term.var("S")
        let P = Term.var("P")
        let M = Term.var("M")
        
        let S1 = Term.var("S1")
        let S2 = Term.var("S2")
        let S3 = Term.var("S3")
        let P1 = Term.var("P1")
        let P2 = Term.var("P2")

        let T1 = Term.var("T1")
        let T2 = Term.var("T2")
        
        let R = Term.var("R")
        let T = Term.var("T")

        switch self {
        case .inheritance:
            return [
                (T1 & T2) --> (T1),
                (T1) --> (T1 | T2),
                (T1 - T2) --> (T1),
                (T1) --> (T1 ~ T2),
                
                ((ç.e_(R, .º, T) * T) --> R),
                (R --> (ç.i_(R, .º, T) * T))
            ]
        case .similarity:
            return [
                -(-T) <-> (T),
                 
                 // TODO: need to verify if this is correct and that it handles multiple components
                 
                 .compound(.U, [.instance(T1), .instance(T2)]) <-> .compound(.extSet, [T1, T2]),
                 .compound(.Ω, [.property(T1), .property(T2)]) <-> .compound(.intSet, [T1, T2]),
                 
                 (.compound(.l, [.compound(.extSet, [T1, T2]), .instance(T2)])) <-> .instance(T1),
                 (.compound(.ø, [.compound(.intSet, [T1, T2]), .property(T2)])) <-> .property(T1),

                 ç.e_((T1 * T2), .º, T2) <-> T1,
                 ç.i_((T1 * T2), .º, T2) <-> T1
            ]
        case .implication:
            return [
                (S <-> P) => (S --> P),
                (S <=> P) => (S => P),
                
                (S1 && S2) => (S1),
                (S1) => (S1 || S2),
                
                (S --> P) => ((S | M) --> (P | M)),
                (S --> P) => ((S & M) --> (P & M)),
                (S <-> P) => ((S | M) --> (P | M)),
                (S <-> P) => ((S & M) --> (P & M)),

                (S  => P) => ((S || M)  => (P || M)),
                (S  => P) => ((S && M)  => (P && M)),
                (S <=> P) => ((S || M) <=> (P || M)),
                (S <=> P) => ((S && M) <=> (P && M)),
                
                (S --> P) => ((S - M) --> (P - M)),
                (S --> P) => ((M - P) --> (M - S)),
                (S --> P) => ((S ~ M) --> (P ~ M)),
                (S --> P) => ((M ~ P) --> (M ~ S)),

                (S <-> P) => ((S - M) <-> (P - M)),
                (S <-> P) => ((M - P) <-> (M - S)),
                (S <-> P) => ((S ~ M) <-> (P ~ M)),
                (S <-> P) => ((M ~ P) <-> (M ~ S)),

                (M --> (T1 - T2)) => -(M --> T2),
                ((T1 ~ T2) --> M) => -(T2 --> M),
                
                (S --> P) => (ç.e_(S, .º, M) --> ç.e_(P, .º, M)),
                (S --> P) => (ç.i_(S, .º, M) --> ç.i_(P, .º, M)),
                (S --> P) => (ç.e_(M, .º, P) --> ç.e_(M, .º, S)),
                (S --> P) => (ç.i_(M, .º, P) --> ç.i_(M, .º, S)),
            ]
        case .equivalence:
            return [
                (S <-> P) <=> &&[(S --> P), (P --> S)],
                (S <=> P) <=> &&[(S  => P), (P  => S)],
                
                (S <-> P) <=> (.instance(S) <-> .instance(P)),
                (S <-> P) <=> (.property(S) <-> .property(P)),
                
                (S --> .instance(P)) <=> (S <-> .instance(P)),
                (.property(S) --> P) <=> (.property(S) <-> P),
                
                ((S1 * S2) --> (P1 * P2)) <=> ((S1 --> P1) && (S2 --> P2)),
                ((S1 * S2) <-> (P1 * P2)) <=> ((S1 <-> P1) && (S2 <-> P2)),
                
                (S --> P) <=> ((M * S) --> (M * P)),
                (S --> P) <=> ((S * M) --> (P * M)),
                (S <-> P) <=> ((M * S) <-> (M * P)),
                (S <-> P) <=> ((S * M) <-> (P * M)),

                (*[T1, T2] --> R) <=> (T1 --> ç.e_(R, .º, T2)),
                (*[T1, T2] --> R) <=> (T2 --> ç.e_(R, T1, .º)),
                (R --> *[T1, T2]) <=> (ç.i_(R, .º, T2) --> T1),
                (R --> *[T1, T2]) <=> (ç.i_(R, T1, .º) --> T2),
                
                ((S1 => (S2 => S3)) <=> ((S1 && S2) => S3)),
                
                -(S1 && S2) <=> .compound(.d, [-(S1), -(S2)]),
                -(S1 || S2) <=> .compound(.c, [-(S1), -(S2)]),
                
                (S1 <=> S2) <=> (-(S1) <=> -(S2)),

                // EXTRA RULES
                // not in the book but
                // alternative forms and/or derived from above rules
                // goal is to ease some derivations
                // note: nars will work without these
                // but will have to derive them during its lifetime
                (T1 --> ç.e_(R, .º, T2)) <=> (T2 --> ç.e_(R, T1, .º))
            ]
        }
    }
}


// MARK: - Helpers

extension Rules {
    // utility
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

