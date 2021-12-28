
/// Extended Boolean operators
/// bounded by the range from 0 to 1
public func not(_ x: Double) -> Double {
    1 - x
}
public func and(_ xs: Double...) -> Double {
    xs.reduce(1, { $0 * $1 })
}
public func or(_ xs: Double...) -> Double {
    1 - xs.reduce(1, { $0 * (1 - $1)})
}

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

/// Forward
extension Rules {
    public var rule: Rule {
        let S = Term.word("S")
        let P = Term.word("P")
        let M = Term.word("M")
        switch self {
        case .identity:
            return (S --> S, S --> S, S --> S, tf)
        case .deduction:
            return (M --> P, S --> M, S --> P, tf)
        case .induction:
            return (M --> P, M --> S, S --> P, tf)
        case .abduction:
            return (P --> M, S --> M, S --> P, tf)
        case .conversion:
            return (P --> S, S --> S, S --> P, tf)
        case .exemplification:
            return (P --> M, M --> S, S --> P, tf)
        case .comparison:
            return (M --> P, M --> S, S <-> P, tf)
        }
    }
}

