
struct Evidence {
    let positive: Double
    let total: Double
}

struct FrequencyInterval {
    let lower: Double
    let upper: Double
}

extension TruthValue {
    public func hash(into hasher: inout Hasher) {
        hasher.combine(frequency)
        hasher.combine(confidence)
    }
}

extension TruthValue: Equatable {
    public static func ==(_ lhs: TruthValue, _ rhs: TruthValue) -> Bool {
        lhs.f == rhs.f && lhs.c == rhs.c // ignore rule
    }
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
        switch s {
        case .term:
            return s -* (1, 0.9)
        case .statement(let subject, _, let predicate):
            return subject == predicate ?
                s -* (1, 1) // tautology
                :
                s -* (1, 0.9) // fact
        }
    }
}

extension Statement {
    var isTautology: Bool {
        switch self {
        case .term:
            return false
        case .statement(let subject, let copula, let predicate):
            return copula == .inheritance && subject == predicate
        }
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

/*
extension Copula {
    var term: Term { .word(rawValue) }
}

extension Term {
    var copula: Copula? { Copula(rawValue: description) }
    var statement: Statement? {
        switch self {
//        case .compound(let connector, let terms):
//            // TODO: perform additional checks for number of terms and their types
//            if let copula = Copula(rawValue: connector.description) {
//                return Statement(terms[0], copula, terms[1])
//            }
//            return nil
        default: return nil
        }
    }
}
*/

extension Statement {
    public init(_ subject: Term, _ copula: Copula, _ predicate: Term) {
        self = .statement(subject, copula, predicate)
    }
    public var terms: [Term] { // TODO: can this be an array?
        switch self {
        case .term(let term):
            return [term]
        case .statement(let subject, _, let predicate):
            return [subject, predicate]
        }
    }
    public var simplicity: Double {
        switch self {
        case .term(let term):
            return term.simplicity
        case .statement(let subject, _, let predicate):
            return subject.simplicity + predicate.simplicity
        }
    }
}

// TODO: do we now get Equatable conformance for free with Statement being an enum?

//public func ==(_ s1: Statement, s2: Statement) -> Bool {
//    if case .term(let t1) = s1, case .term(let t2) = s2 {
//        return t1 == t2
//    }
//    s1.copula == s2.copula && s1.terms == s2.terms
//}

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
    public init(_ frequency: Double, _ confidence: Double, _ rule: Rules = .identity) {
        self.frequency = rounded(frequency)
        self.confidence = rounded(confidence)
        self.rule = rule
    }
    init(_ ev: Evidence, _ rule: Rules = .identity) {
        self.frequency = rounded(ev.frequency)
        self.confidence = rounded(ev.confidence)
        self.rule = rule
    }
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
        case .instance(let term):
            return term.description.first == "{" ?
                "\(term)" : "{\(term)}"
        case .property(let term):
            return term.description.first == "[" ?
                "\(term)" : "[\(term)]"
        case .compound(let connector, let terms):
            if terms.count == 2 {
                return "(\(terms[0]) \(connector.rawValue) \(terms[1]))"
            } else {
                return "(\(connector.rawValue) \(terms.map{$0.description}.joined(separator: " ")))"
            }
        case .statement(let statement):
            return "\(statement)"
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
        "<\(frequency), \(confidence)>\(rule)"
    }
}

extension Rules: CustomStringConvertible {
    public var description: String {
        switch self {
        case .identity:        
            return "."
        default:
            return "." + rawValue.prefix(3)
//        case .deduction:       return ".ded"
//        case .induction:       return ".ind"
//        case .abduction:       return ".abd"
//        case .conversion:      return ".con"
//        case .exemplification: return ".exe"
//        case .comparison:      return ".com"
//        case .analogy:         return ".ana"
//        case .resemblance:     return ".res"
        }
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
        switch self {
        case .term(let term):
            return "\(term)"
        case .statement(let subject, let copula, let predicate):
            return "\(subject) " + copula.rawValue + " \(predicate)"
        }
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
