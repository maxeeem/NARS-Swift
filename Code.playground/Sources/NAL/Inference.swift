
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
        (j1.truthValue.e > j2.truthValue.e) ? j1 : j2
}

extension Rules {
    public var rule: Rule {
        let S = Term.word("S")
        let P = Term.word("P")
        let M = Term.word("M")
        /// first unique term is `false`, second and third are `nil`
        /// if there is no common term term identified by `true`
        /// then a conclusion could not be derived
        switch self {
        case .identity:
            /// all true
            return (S --> S, S --> S, S --> S, tf)
        case .deduction:
            ///    true, false, nil, true
            return (M --> P,     S --> M, S --> P, tf)
        case .induction:
            ///    true, false, true, nil
            return (M --> P,     M --> S, S --> P, tf)
        case .abduction:
            ///    false, true, nil, true
            return (P --> M,     S --> M, S --> P, tf)
        case .conversion:
            ///    false, true, true, true
            return (P --> S,     S --> S, S --> P, tf)
        case .exemplification:
            ///    false, true, true, nil
            return (P --> M,     M --> S, S --> P, tf)
        case .comparison:
            ///    true, false, true, nil
            return (M --> P,     M --> S, S <-> P, tf)
        }
    }
}

