
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

extension Rules {
    public var rule: [Rule] {
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
            return [(M --> P,     S --> M, S --> P, tf)]
        case .induction:
            ///    true, false, true, nil
            return [(M --> P,     M --> S, S --> P, tf)]
        case .abduction:
            ///    false, true, nil, true
            return [(P --> M,     S --> M, S --> P, tf)]
        case .conversion:
            ///    false, true, true, true
            return [(P --> S,     S --> S, S --> P, tf)]
        case .exemplification:
            ///    false, true, true, nil
            return [(P --> M,     M --> S, S --> P, tf)]
        case .comparison:
            ///    true, false, true, nil
            return [(M --> P,     M --> S, S <-> P, tf)]
        case .analogy:
            ///    true, false, nil, true
            return [(M --> P,     S <-> M, S --> P, tf),
            ///    true, false, true, nil
                    (M --> P,     M <-> S, S --> P, tf)]
        case .resemblance:
            ///    true, false, nil, true
            return [(M <-> P,     S <-> M, S <-> P, tf),
            ///    true, false, true, nil
                    (M <-> P,     M <-> S, S <-> P, tf),
            ///    false, true, nil, true
                    (P <-> M,     S <-> M, S <-> P, tf),
            ///    false, true, true, nil
                    (P <-> M,     M <-> S, S <-> P, tf)]
            
        // Compositional rules
        
        case .intersection:
            ///    true, false, true, nil
            return [(M --> T1,    M --> T2,    M --> ç.Ω_(T1, T2), tf),
                    (T1 --> M,    T2 --> M,    ç.U_(T1, T2) --> M, tf)]
        case .union:
            return [(M --> T1,    M --> T2,    M --> ç.U_(T1, T2), tf),
                    (T1 --> M,    T2 --> M,    ç.Ω_(T1, T2) --> M, tf)]
        case .difference:
            return [(M --> T1,    M --> T2,    M --> ç.l_(T1, T2), tf),
                    (M --> T1,    M --> T2,    M --> ç.l_(T2, T1), tf),
                    (T1 --> M,    T2 --> M,    ç.ø_(T1, T2) --> M, tf),
                    (T1 --> M,    T2 --> M,    ç.ø_(T2, T1) --> M, tf)]
        }
    }
}

