
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

/// NAL-1
public func revision(j1: Judgement, j2: Judgement) -> Judgement {
    let (f1, c1) = (j1.truthValue.frequency, j1.truthValue.confidence)
    let (f2, c2) = (j2.truthValue.frequency, j2.truthValue.confidence)
    let f = ((f1 * c1) * (1 - c2) + (f2 * c2) * (1 - c1)) / (c1 * (1 - c2) + c2 * (1 - c1))
    let c = (c1 * (1 - c2) + c2 * (1 - c1)) / (c1 * (1 - c2) + c2 * (1 - c1) + (1 - c1) * (1 - c2))
    return Judgement(j1.statement, TruthValue(f, c))
}

public func choice(j1: Judgement, j2: Judgement) -> Judgement {
    if j1.statement == j2.statement {
        return (j1.truthValue.confidence > j2.truthValue.confidence) ? j1 : j2
    } else {
        return (j1.truthValue.expectation > j2.truthValue.expectation) ? j1 : j2
    }
}

public func conversion(j1: Judgement) -> Judgement? {
    // { P -> S } |- S -> P
    if j1.statement.copula != .inheritance {
        return nil // N/A
    }
    let truthValue = TruthValue.conversion(j1.truthValue)
    let statement = (j1.statement.predicate --> j1.statement.subject)
    return Judgement(statement, truthValue)
}

// MARK: Data-driven rules

extension Rules {
    public var rule: Rule {
        let S = Term.word("S")
        let P = Term.word("P")
        let M = Term.word("M")
        switch self {
        case .deduction:
            return (M --> P, S --> M, S --> P, tf)
        case .induction:
            return (M --> P, M --> S, S --> P, tf)
        case .abduction:
            return (P --> M, S --> M, S --> P, tf)
        case .exemplification:
            return (P --> M, M --> S, S --> P, tf)
        case .comparison:
            return (M --> P, M --> S, S <-> P, tf)
        }
    }
    private var tf: TruthFunction {
        switch self {
        case .deduction: return TruthValue.deduction
        case .induction: return TruthValue.induction
        case .abduction: return TruthValue.abduction
        case .exemplification: return TruthValue.exemplification
        case .comparison: return TruthValue.comparison
        }
    }
    public var apply: (_ judgements: (Judgement, Judgement)) -> Judgement? {
        rule_generator(rule)
    }
}

