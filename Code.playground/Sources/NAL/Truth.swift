
public struct TruthValue: Hashable {
    let frequency: Double
    let confidence: Double
    
    let rule: Rules // for derived values
}

extension TruthValue: Equatable {
    public static func ==(_ lhs: TruthValue, _ rhs: TruthValue) -> Bool {
        lhs.f == rhs.f && lhs.c == rhs.c // ignore rule
    }
}

extension TruthValue {
    var f: Double {frequency}
    var c: Double {confidence}
    var l: Double {lowerFrequency}
    var u: Double {upperFrequency}
    var wpos: Double {positiveEvidence}
    var wtot: Double {totalEvidence}
    var e: Double {expectation}
    
    var positiveEvidence: Double { k * f * c / (1 - c) }
    var totalEvidence: Double { k * c / (1 - c) }
    var lowerFrequency: Double { f * c }
    var upperFrequency: Double { 1 - c * (1 - f) }
    var expectation: Double { (l + u) / 2 }
}

public typealias TruthFunction = (TruthValue, TruthValue) -> TruthValue

infix operator ~
private func ~(_ r: Rules, _ tf: @escaping TruthFunction) -> TruthFunction {
    { (tv1, tv2) in
        let tv = tf(tv1, tv2)
        return TruthValue(tv.f, tv.c, r)
    }
}

extension TruthValue {
    static func truthFunction(_ r: Rules) -> TruthFunction {
        switch r {
        case .identity:        return r~{t,_ in t}
        case .deduction:       return r~deduction
        case .induction:       return r~induction
        case .abduction:       return r~abduction
        case .conversion:      return r~conversion
        case .exemplification: return r~exemplification
        case .comparison:      return r~comparison
        }
    }
    
    static var deduction: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, f2)
        let c = and(f1, f2, c1, c2)
        return TruthValue(f, c)
    }
    static var induction: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let positive = and(f2, c2, f1, c1) // w+
        let total = and(f2, c2, c1) // w
        let evidence = Evidence(positive, total)
        return TruthValue(evidence)
    }
    static var abduction: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let positive = and(f1, c1, f2, c2) // w+
        let total = and(f1, c1, c2) // w
        let evidence = Evidence(positive, total)
        return TruthValue(evidence)
    }
    static var conversion: TruthFunction = { (tv1, _) in
        let (f, c) = (tv1.f, tv1.c)
        let c1 = f * c / (f * c + k)
        return TruthValue(1, c1)
    }
    static var exemplification: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let positive = and(f1, c1, f2, c2) // w+
        let total = and(f1, c1, f2, c2) // w
        let evidence = Evidence(positive, total)
        return TruthValue(evidence)
    }
}

extension TruthValue {
    static var comparison: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let positive = and(f1, c1, f2, c2) // w+
        let total = and(or(f1, f2), c1, c2) // w
        let evidence = Evidence(positive, total)
        return TruthValue(evidence)
    }
}
