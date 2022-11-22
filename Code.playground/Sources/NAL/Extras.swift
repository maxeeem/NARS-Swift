
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

extension Judgement: Equatable {
    public static func == (lhs: Judgement, rhs: Judgement) -> Bool {
        lhs.statement == rhs.statement && lhs.truthValue == rhs.truthValue
    }
}

// convenience initializer for Judgement
public func +(_ s: Statement, fc: (Double, Double, UInt64)) -> Judgement {
    Judgement(s, TruthValue(fc.0, fc.1), timestamp: fc.2)
}

infix operator -* : Copula
public func -* (_ s: Statement, _ fc: (Double, Double, UInt64)) -> Judgement {
    s + fc
}

postfix operator -*
extension Statement {
    public static postfix func -*(_ s: Statement) -> Judgement {
        switch s {
        case .symbol:
            return s + (1.0, 0.9, ETERNAL)
        case .compound:
            return s + (1.0, 0.9, ETERNAL) // TODO: is this accurate?
        case .statement(let subject, _, let predicate):
            return subject == predicate ?
            s + (1.0, 1.0, ETERNAL) // tautology
                :
            s + (1.0, 0.9, ETERNAL) // fact
        case .variable:
            return s + (1.0, 0.9, ETERNAL) // TODO: is this accurate?
        case .operation:
            return .NULL + (1.0, 0.9, ETERNAL)
        }
    }
}

extension Statement {
    var isTautology: Bool {
        switch self {
        case .symbol:
            return false
        case .compound:
            return false // TODO: is this accurate?
        case .statement(let subject, _, let predicate):
            return /*copula == .inheritance &&*/ subject == predicate
//                Set(subject.terms).intersection(Set(predicate.terms)).isEmpty == false
        case .variable:
            return false
        case .operation:
            return false
        }
    }
}

extension Statement {
    static prefix func - (_ s: Statement) -> Statement { .compound(.n, [s]) }
}

//infix operator &
func & (_ lhs: Statement, _ rhs: Statement) -> Statement { ç.Ω_(lhs, rhs) }
//infix operator |
func | (_ lhs: Statement, _ rhs: Statement) -> Statement { ç.U_(lhs, rhs) }

//infix operator -
func - (_ lhs: Statement, _ rhs: Statement) -> Statement { ç.l_(lhs, rhs) }
//infix operator ~
func ~ (_ lhs: Statement, _ rhs: Statement) -> Statement { ç.ø_(lhs, rhs) }

func * (_ lhs: Statement, _ rhs: Statement) -> Statement { ç.x_(lhs, rhs) }

func && (_ lhs: Statement, _ rhs: Statement) -> Statement { ç.c_(lhs, rhs) }
func || (_ lhs: Statement, _ rhs: Statement) -> Statement { ç.d_(lhs, rhs) }

extension Array where Element == Statement { // TODO: use connect(:) method to create compounds
    static prefix func + (_ s: Array<Statement>) -> Statement { .compound(.c, s)}
    static prefix func - (_ s: Array<Statement>) -> Statement { .compound(.d, s)}
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

//
// TODO: check all code
// TODO: Whenever a judgement is constructed from another judgement or a statement, make sure we keep temporal information
//

extension Judgement {
    public init(_ statement: Statement, _ truthValue: TruthValue, _ derivationPath: [String] = [], tense: Tense? = nil, timestamp: UInt64 = 0) {
        self.statement = statement
        self.truthValue = truthValue
        self.tense = tense
        self.timestamp = tense == nil ? ETERNAL : timestamp
        if derivationPath.isEmpty {
            let description = Judgement.sortedDescription(statement)
//            print("--", description)
            self.derivationPath = ["\(description)+\((truthValue.f, truthValue.c, timestamp))"]
        } else {
            self.derivationPath = derivationPath
        }
//        print(statement)
//        print(derivationPath)
    }
    
    private static func sortedDescription(_ statement: Statement) -> String {
        var st = ""
        switch statement {
        case .statement(let s, let c, let p):
            if c == .similarity {
                st = (s < p) ? "\(s) \(c.rawValue) \(p)" : "\(p) \(c.rawValue) \(s)"
            } else if c == .equivalence {
                let sub = sortedDescription(s)
                let pre = sortedDescription(p)
                st = (sub < pre) ? "(\(sub)) \(c.rawValue) (\(pre))" : "(\(pre)) \(c.rawValue) (\(sub))"
            } else {
                st = "\(statement)"
            }
        case .compound(let c, let terms):
            if c == .c || c == .d || c == .n {
                st = "\(c.rawValue) \(terms.map{"(\(sortedDescription($0)))"}.sorted().joined(separator: ", "))"
            } else {
                st = "\(statement)"
            }
        default:
            st = "\(statement)"
        }
        return st
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
    public init(_ frequency: Double, _ confidence: Double, _ rule: Rules! = nil) {
        self.frequency = rounded(frequency)
        self.confidence = rounded(confidence)
        self.rule = rule
    }
    init(_ ev: Evidence, _ rule: Rules! = nil) {
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


extension Term: Comparable {
    public static func < (lhs: Term, rhs: Term) -> Bool {
        lhs.description < rhs.description
    }
}


extension Judgement {
    public var identifier: String { tense == nil ? statement.description : tense!.description + " " + statement.description }
}


extension Question {
    public var identifier: String { tense == nil ? statement.description : tense!.description + " " + statement.description }
}


/// from https://stackoverflow.com/a/34699637
extension Array where Element == Bool {
    public var allValid: Bool { !contains(false) }
}


/// from https://stackoverflow.com/a/38036978
public func rounded(_ d: Double, _ x: Int = 100) -> Double {
    let result = (d * Double(x)).rounded() / Double(x)
    return result.isNaN ? 0 : result
}



extension Judgement {
    static func mergeEvidence(_ j1: Judgement, _ j2: Judgement) -> [String] {
        if j1.derivationPath.isEmpty {
            return j2.derivationPath
        } else if j2.derivationPath.isEmpty {
            return j1.derivationPath
        } else {
            var tail: [String] = []
            if j1.derivationPath.count < j2.derivationPath.count {
                tail = Array(j2.derivationPath.suffix(from: j1.derivationPath.endIndex))
            } else if j2.derivationPath.count > j1.derivationPath.count {
                tail = Array(j1.derivationPath.suffix(from: j1.derivationPath.count))
            }
            return (zip(j1.derivationPath, j2.derivationPath).reduce([], { partialResult, next in
                partialResult + (next.0 == next.1 ? [next.0] : [next.0, next.1])
            }) + tail).suffix(100)
        }
    }
    
    func evidenceOverlap(_ j2: Judgement) -> Bool {
        let sameRoot = derivationPath.first == j2.derivationPath.first
        let p1 = sameRoot ? Array(derivationPath.dropFirst()) : derivationPath
        let p2 = sameRoot ? Array(j2.derivationPath.dropFirst()) : j2.derivationPath

        if p1.isEmpty && p2.isEmpty {
            return true // judgements have the same root
        } else if p1.count == 1 && p2.count == 1 {
            if p1[0].hasSuffix("\(ETERNAL))") && p2[0].hasSuffix("\(ETERNAL))") {
                // judgements are both eternal
//                print("p1", p1)
//                print("p2", p2)
                if p1[0] == p2[0] // same path or one is a theorem which has E as its evidential base
                    || p1[0].hasSuffix("+(1.0, 1.0, \(ETERNAL))") || p2[0].hasSuffix("+(1.0, 1.0, \(ETERNAL))") {
                
//                    if p1[0].prefix(while: {$0 != "+"}) == p2[0].prefix(while: {$0 != "+"}) { // NO GOOD
                        
//                    || p1[0].hasPrefix("(swan <–> bird) <=> (swan -> bird ∧ bird -> swan)") || p1[0].hasPrefix("(bird <–> swan) <=> (bird -> swan ∧ swan -> bird)") {
//                    // TODO: do proper comparison taking into account symmetrical statements
//                    // so <bird <-> swan> should be same as <swan <-> bird>
                    return true // same path
                } else {
//                    if p1[0].hasPrefix(statement.description) && p2[0].hasPrefix(j2.statement.description) {
//                        return false // both statements are user inputs
//                    }
                    return false // different path
                }
            }
        }
        
        return !Set(p1).intersection(Set(p2)).isEmpty
    }
}


extension Variable {
    var name: String? {
        switch self {
        case .independent(let string):
            return string
        case .dependent(let optional, _):
            return optional
        case .query(let optional):
            return optional
        }
    }
}


extension Term {
    func replace(varName: String, termName: String) -> Term {
        switch self {
        case .symbol:
            return self
        case .compound(let conn, let terms):
            return .compound(conn, terms.map{$0.replace(varName: varName, termName: termName)})
        case .statement(let sub, let cop, let pre):
            return .statement(sub.replace(varName: varName, termName: termName), cop, pre.replace(varName: varName, termName: termName))
        case .variable(let vari):
            switch vari {
            case .independent(let str):
                if str == varName {
                    return .symbol(termName)
                }
                return self
            case .dependent(let str, _):
                if str == varName {
                    return .symbol(termName)
                }
                return self
            default: // TODO: how to handle dependent vars?
                return self
            }
        case .operation(let name, let terms):
            return .operation(name, terms.map{$0.replace(varName: varName, termName: termName)})
        }
    }
    
    func replace(termName: String, indepVarName: String) -> Term {
        switch self {
        case .symbol(let str):
            if str == termName {
                return .variable(.independent(indepVarName))
            }
            return self
        case .statement(let sub, let cop, let pre):
            return .statement(sub.replace(termName: termName, indepVarName: indepVarName), cop, pre.replace(termName: termName, indepVarName: indepVarName))
        default: // TODO: properly handle all cases
            return self
        }
    }
    
    func replace(termName: String, depVarName: String) -> Term {
        switch self {
        case .symbol(let str):
            if str == termName {
                return .variable(.dependent(depVarName, []))
            }
            return self
        case .compound(let conn, let terms):
            return .compound(conn, terms.map{$0.replace(termName: termName, depVarName: depVarName)})
        case .statement(let sub, let cop, let pre):
            return .statement(sub.replace(termName: termName, depVarName: depVarName), cop, pre.replace(termName: termName, depVarName: depVarName))
        default: // TODO: properly handle all cases
            return self
        }
    }
    
    func replace(termName: String, term: Term) -> Term {
        switch self {
        case .symbol(let str):
            if str == termName {
                return term
            }
            return self
        case .compound(let conn, let terms):
            return .compound(conn, terms.map{$0.replace(termName: termName, term: term)})
        case .statement(let sub, let cop, let pre):
            return .statement(sub.replace(termName: termName, term: term), cop, pre.replace(termName: termName, term: term))
        case .variable:
            if description == termName {
                return term
            }
            return self
        default: // TODO: properly handle all cases
            return self
        }
    }
}


extension Sequence where Element == Term {
    func toList() -> List {
        var list: List = .empty
        for term in self.reversed() {
            list = List.cons(term.logic(), list)
        }
        return list
    }
}


extension Variable {
    init?(_ string: String) {
        if string.hasPrefix("?") {
            let name = string.suffix(from: string.index(string.startIndex, offsetBy: 1))
            self = .query(name.isEmpty ? nil : String(name))
            return
        }
        if string.hasPrefix("#") {
            let name = string.suffix(from: string.index(string.startIndex, offsetBy: 1))
            if name.isEmpty {
                self = .dependent(nil, [])
                return
            } else {
                // TODO: parse independent vars list
                if let idx = name.firstIndex(of: "(") {
                    let trimmed = name.prefix(upTo: idx)
                    self = .dependent(String(trimmed), [])
                    return
                }
                self = .independent(String(name))
                return
            }
        }
        return nil
    }
}

extension Term {
    func logic() -> LogicTerm {
        switch self {
        case .symbol:
            return LogicValue(self)
        case .compound(let c, let terms):
            return List.cons(LogicValue(c), terms.toList())
        case .statement(let s, let c, let p):
            return List.cons(LogicValue(c), List.cons(s.logic(), List.cons(p.logic(), List.empty)))
            
        case .variable(let vari):
            //        switch vari {
            //        case .independent(let name):
            //            return List.cons(LogicValue("var-ind"), List.cons(LogicVariable(named: name), List.empty))
            //        case .dependent(let name, let vars):
            //            var ll: List = .empty
            //            for v in vars.reversed() {
            //                ll = List.cons(LogicVariable(named: v), ll)
            //            }
            //            ll = List.cons(LogicValue("var-ind"), ll)
            //            return List.cons(LogicValue("var-dep"), List.cons(LogicVariable(named: name ?? "x()"), ll))
            //        }
            return LogicVariable(named: self.description) // TODO: handle nested variables
//            return LogicVariable(named: vari.name ?? "x")
            
        case .operation(let op, let terms):
            return List.cons(LogicValue(op), terms.toList())
        }
    }
        
    static func from(logic: LogicTerm) -> Term {
        if let value = logic as? LogicValue<Term> {
            return value.extract()
        }
        if let variable = logic as? LogicVariable {
            if let vari = Variable(variable.name) {
                return .variable(vari)
            }
        }
        if let list = logic as? List {
            if case .cons(let head, let tail) = list {
                if let value = head as? LogicValue<Connector> { // compound
                    return .compound(value.extract(), process(list: tail))
                }
                
                if let value = head as? LogicValue<Copula> { // statement
                    let terms = process(list: tail)
                    return .statement(terms[0], value.extract(), terms[1])
                }
                
                if let value = head as? LogicValue<String> { // operation
                    return .operation(value.extract(), process(list: tail))
                }
            }
        }
        // helper
        func process(list: LogicTerm) -> [Term] {
            var terms: [Term] = []
            if case .cons(let head, let tail) = list as? List {
                terms.append(Term.from(logic: head))
                terms.append(contentsOf: process(list: tail))
            }
            return terms
        }
        
        return .NULL // DEFAULT
    }
}
