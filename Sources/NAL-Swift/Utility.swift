public struct NAL_Swift {
    public private(set) var text = "Hello, World!"

    public init() {
    }
}

struct Evidence {
    let positive: Double
    let total: Double
}

struct FrequencyInterval {
    let lower: Double
    let upper: Double
}

// convenience initializer for Judgement
public func +(_ s: Statement, fc: (Double, Double)) -> Judgement {
    Judgement(s, TruthValue(fc.0, fc.1))
}

infix operator -* : Copula
public func -* (_ s: Statement, _ fc: (Double, Double)) -> Judgement {
    s + fc
}

postfix operator -*
extension Statement {
    public static postfix func -*(_ s: Statement) -> Judgement {
        s -* (1, 0.9)
    }
    
}

//prefix operator •
//
//public extension Copula {
//    func makeStatement(_ subject: Term, _ predicate: Term) -> Statement {
//        Statement(subject: subject, copula: self, predicate: predicate)
//    }
//    static func makeStatement(_ copula: Copula) -> (_ s: Term, _ p: Term) -> Statement {
//        { s, p in
//            copula.makeStatement(s, p)
//        }
//    }
//    init(_ copula: Copula) {
//        self = copula
//    }
//}

extension Statement {
    public init(_ subject: Term, _ copula: Copula, _ predicate: Term) {
        self.subject = subject
        self.copula = copula
        self.predicate = predicate
    }
    public var terms: (Term, Term) { (subject, predicate) }
}

public func ==(_ s1: Statement, s2: Statement) -> Bool {
    s1.copula == s2.copula && s1.terms == s2.terms
}

extension Judgement {
    public init(_ statement: Statement, _ truthValue: TruthValue) {
        self.statement = statement
        self.truthValue = truthValue
    }
}

extension Evidence {
    init(_ positive: Double, _ total: Double) {
        self.positive = positive
        self.total = total
    }
    init(_ positive: Int, _ total: Int) {
        self.positive = Double(positive)
        self.total = Double(total)
    }
    var negative: Double { total - positive }
    var frequency: Double { positive / total }
    var confidence: Double { total / (total + evidentialHorizon) }
    var lowerFrequency: Double { positive / (total + evidentialHorizon) }
    var upperFrequency: Double { (positive + evidentialHorizon) / (total + evidentialHorizon) }
    var truthValue: TruthValue { TruthValue(frequency, confidence) }
}

extension TruthValue {
    public init(_ frequency: Double, _ confidence: Double) {
        self.frequency = rounded(frequency)
        self.confidence = rounded(confidence)
    }
    init(_ ev: Evidence) {
        self.frequency = rounded(ev.frequency)
        self.confidence = rounded(ev.confidence)
    }
    var positiveEvidence: Double { Double(evidentialHorizon) * frequency * confidence / (1 - confidence) }
    var totalEvidence: Double { Double(evidentialHorizon) * confidence / (1 - confidence) }
    var lowerFrequency: Double { frequency * confidence }
    var upperFrequency: Double { 1 - confidence * (1 - frequency) }
    var expectation: Double { (lowerFrequency + upperFrequency) / 2 }
}

extension FrequencyInterval {
    init(_ lower: Double, _ upper: Double) {
        self.lower = lower
        self.upper = upper
    }
    var ignorance: Double { upper - lower }
    var positiveEvidence: Double { evidentialHorizon * lower / ignorance }
    var totalEvidence: Double { evidentialHorizon * (1 - ignorance) / ignorance }
    var frequency: Double { lower / (1 - ignorance) }
    var confidence: Double { 1 - ignorance }
}

// MARK: CustomStringConvertible

extension Term: CustomStringConvertible {
    public var description: String {
        switch self {
        case .word(let word):
            return word
        case .compound(let connector, let terms):
            if terms.count == 2, let copula = Copula(rawValue: connector.description) {
                return "\(Statement(terms[0], copula, terms[1]))"
            }
            return "\(connector) \(terms)"
        }
    }
}

extension Evidence: CustomStringConvertible {
    public var description: String {
        "(\(positive), \(total))"
    }
}

extension TruthValue: CustomStringConvertible {
    public var description: String {
        "<\(frequency), \(confidence)>"
    }
}

extension Question: CustomStringConvertible {
    public var description: String {
        switch self {
        case .statement(let statement):
            return "\(statement)"
        case .general(let term, let copula):
            return "\(term) \(copula.rawValue) ?"
        case .special(let copula, let term):
            return "? \(copula.rawValue) \(term)"
        }
    }
}

extension Judgement: CustomStringConvertible {
    public var description: String {
        "\(statement)" + "\(truthValue)"
    }
}

extension Statement: CustomStringConvertible {
    public var description: String {
        "\(subject) " + copula.rawValue + " \(predicate)"
    }
}


/// from https://stackoverflow.com/a/34699637
extension Array where Element == Bool {
    public var allValid: Bool { !contains(false) }
}


/// from https://stackoverflow.com/a/38036978
public func rounded(_ d: Double, _ x: Int = 10000) -> Double {
    (d * Double(x)).rounded() / Double(x)
}
