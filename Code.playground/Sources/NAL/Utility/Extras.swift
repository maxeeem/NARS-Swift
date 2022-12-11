
extension Question {
    public init(_ statement: Statement, _ type: Quest, _ tense: Tense? = nil) {
        self.statement = statement
        self.type = type
        self.tense = tense
    }
}

extension Goal {
    public init(_ statement: Statement, _ desireValue: DesireValue) {
        self.statement = statement
        self.desireValue = desireValue
    }
}

extension TruthValue: Equatable {
    public static func ==(_ lhs: TruthValue, _ rhs: TruthValue) -> Bool {
        lhs.f == rhs.f && lhs.c == rhs.c // ignore rule
    }
}

extension Judgement: Equatable {
    public static func == (lhs: Judgement, rhs: Judgement) -> Bool {
        lhs.statement == rhs.statement && lhs.truthValue == rhs.truthValue
    }
}

extension Term: Comparable {
    public static func < (lhs: Term, rhs: Term) -> Bool {
        lhs.description < rhs.description
    }
}

//
// TODO: check all code
// TODO: Whenever a judgement is constructed from another judgement or a statement, make sure we keep temporal information
//

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


// MARK: CustomStringConvertible

extension Term: CustomStringConvertible {
    public var description: String {
        switch self {
        case .symbol(let word):
            return word
        case .compound(let connector, let terms):
            if terms.count == 2 {
                return "(\(terms[0]) \(connector.rawValue) \(terms[1]))"
            } else if connector == .intSet || connector == .extSet {
                if connector == .intSet {
                    return "[\(terms.map{$0.description}.joined(separator: " "))]"
                } else {
                    return "{\(terms.map{$0.description}.joined(separator: " "))}"
                }
            } else if connector == .n {
                return connector.rawValue + "(\(terms[0].description))"
            } else {
                return "(\(connector.rawValue) \(terms.map{$0.description}.joined(separator: " ")))"
            }
        case .statement(let subject, let copula, let predicate):
            var s = "\(subject)"
            if case .statement = subject {
                s = "(\(subject))"
            }
            var p = "\(predicate)"
            if case .statement = predicate {
                p = "(\(predicate))"
            }
            return s + " " + copula.rawValue + " " + p
        case .variable(let variable):
            switch variable {
            case .independent(let word):
                return "#\(word)" //TODO: update to use `$`
            case .dependent(let word, let variables):
                if let w = word {
                    let independents = variables.map { "#\($0)" }
                    let list = independents.joined(separator: ", ")
                    return "#\(w)(\(list))"
                }
                return "#"
            case .query(let word):
                return (word == nil) ? "?" : "?\(word!)"
            }
        case .operation(let name, let terms):
            return name + terms.map{$0.description}.joined(separator: " ")
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
        let r = rule == nil ? "." : "\(rule!)"
        let f = "\(f)".count == 3 ? "\(f)0" : "\(f)"
        let c = "\(c)".count == 3 ? "\(c)0" : "\(c)"
        return "%\(f);\(c)%\(r)"
    }
}

extension Rules: CustomStringConvertible {
    public var description: String {
        switch self {
        case .conversion:
            return ".cnv"
        case .contraposition:
            return ".cnt"
        default:
            return "." + rawValue.prefix(3)
        }
    }
}

extension Question: CustomStringConvertible {
    public var description: String {
        "<\(statement)>?"
    }
}

extension Judgement: CustomStringConvertible {
    public var description: String {
        "<\(statement)>. " + "\(truthValue)"
    }
}

extension Goal: CustomStringConvertible {
    public var description: String {
        "<\(statement)>! " + "\(desireValue)"
    }
}

extension Tense: CustomStringConvertible {
    public var description: String {
        rawValue
    }
}

// MARK: Identifiable

extension Judgement {
    public var identifier: String { tense == nil ? statement.description : tense!.description + " " + statement.description }
}


extension Question {
    public var identifier: String { tense == nil ? statement.description : tense!.description + " " + statement.description }
}


// MARK: Utility

/// from https://stackoverflow.com/a/34699637
extension Array where Element == Bool {
    public var allValid: Bool { !contains(false) }
}


/// from https://stackoverflow.com/a/38036978
public func rounded(_ d: Double, _ x: Int = 100) -> Double {
    let result = (d * Double(x)).rounded() / Double(x)
    return result.isNaN ? 0 : result
}

func pow(_ x: Double, _ y: Int) -> Double {
    let isNegative = y < 0
    var res = 1.0
    for _ in 1...abs(y) {
        res *= x
    }
    return isNegative ? 1 / res : res
}





