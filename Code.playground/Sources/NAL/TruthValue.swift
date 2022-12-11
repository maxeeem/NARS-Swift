
public struct TruthValue: Hashable, Codable {
    public let frequency: Double
    public let confidence: Double
    
    public let rule: Rules! // for derived values
}

public extension TruthValue {
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

extension TruthValue {
    static var tautology: TruthValue { TruthValue(1, 1) }
}

public typealias TruthFunction = (TruthValue, TruthValue) -> TruthValue

infix operator ~ // rule to truth function mapping
private func ~(_ r: (Rules, Bool), _ tf: @escaping TruthFunction) -> TruthFunction {
    { (tv1, tv2) in
        let (rule, inverse) = r
        let tv = inverse ? tf(tv2, tv1) : tf(tv1, tv2)
        return TruthValue(tv.f, tv.c, rule)
    }
}

extension TruthValue {
    static func truthFunction(_ r: Rules, _ i: Bool) -> TruthFunction {
        switch r {
        case .deduction:       return (r,i)~deduction
        case .induction:       return (r,i)~induction
        case .abduction:       return (r,i)~abduction
        case .exemplification: return (r,i)~exemplification
        case .comparison:      return (r,i)~comparison
        case .analogy:         return (r,i)~analogy
        case .resemblance:     return (r,i)~resemblance
            
        case .intersection:    return (r,i)~intersection
        case .union:           return (r,i)~union
        case .difference:      return (r,i)~difference
        
        default:               return (r,i)~{_,_ in .tautology }
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
    static var analogy: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, f2)
        let c = and(f2, c1, c2)
        return TruthValue(f, c)
    }
    static var resemblance: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, f2)
        let c = and(or(f1, f2), c1, c2)
        return TruthValue(f, c)
    }
}

extension TruthValue {
    static var intersection: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, f2)
        let c = and(c1, c2)
        return TruthValue(f, c)
    }
    static var union: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = or(f1, f2)
        let c = and(c1, c2)
        return TruthValue(f, c)
    }
    static var difference: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, not(f2))
        let c = and(c1, c2)
        return TruthValue(f, c)
    }
}


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
