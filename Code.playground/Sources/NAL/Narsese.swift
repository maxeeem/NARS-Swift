
// Grammar

public enum Tense: String, Hashable, Codable {
    case past    = "<<"
    case present = "||"
    case future  = ">>"
}

public typealias Statement = Term

public struct Judgement: Hashable, Codable {
    public let statement: Statement
    public let truthValue: TruthValue
    
    public let tense: Tense?
    public let derivationPath: [String]
    
    public var timestamp: UInt64 = 0
    
    // TODO: need to add desireValue
    /*
     “In NARS, a desire-value is not only attached to every goal, but to every event, because an event may become a goal in the future (if it is not already a goal).
 */
}

public typealias DesireValue = TruthValue

public struct Goal: Hashable, Codable {
    public let statement: Statement
    public let desireValue: DesireValue
}

public enum Quest: Hashable, Codable {
    case truth
    case desire
}

public struct Question: Hashable, Codable {
    public let statement: Statement
    public let type: Quest
    
    public let tense: Tense?
}

public indirect enum Term: Hashable, Codable {
    case symbol(String)
    case compound(Connector, [Term])
    case statement(Term, Copula, Term)
    case variable(Variable)
    case operation(String, [Term])
}

public enum Variable: Hashable, Codable {
    case independent(String)
    case dependent(String?, [String])
    case query(String?)
}

public typealias ç = Connector

public enum Connector: String, CaseIterable, Codable {
    /// intensional set
    case intSet = "[]"  /// Ω 
    /// extensional set
    case extSet = "{}"  /// U
    
    /// extensional intersection
    case Ω = "⋂" /// intensional set
    /// intensional intersection
    case U = "⋃" /// extensional set
    /// extensional difference
    case l = "–"
    /// intensional difference
    case ø = "ø"
    
    /// product
    case x = "⨯"
    
    /// extensional image
    case e = "/"
    /// intensional image
    case i = "\\" /// -- two slashes are because swift
    
    /// negation
    case n = "¬"
    /// conjunction
    case c = "∧"
    /// disjunction
    case d = "∨"
    
    /// sequential conjunction
    case s = ","
    /// parallel conjunction
    case p = ";"
    
    //    case a = "ø"
    // ¡™¡!`¡``````````¡™£¢∞§¶•ªº–≠«‘“πøˆ¨¥†®´∑œåß∂ƒ©˙˙∆˚¬…æ÷≥≤µ˜∫√ç≈Ω!@#$%^&*()_+|}{":>?<
}

extension Connector {
    var term: Term { Term.symbol(rawValue) }
    
    public static func Ω_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .Ω, t2) }
    public static func U_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .U, t2) }
    public static func l_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .l, t2) }
    public static func ø_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .ø, t2) }
    public static func x_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .x, t2) }

    public static func c_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .c, t2) }
    public static func d_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .d, t2) }

    public static func e_(_ r: Term, _ t1: Term, _ t2: Term) -> Term { connect(.compound(.x, [r]), .e, x_(t1, t2)) }
    public static func i_(_ r: Term, _ t1: Term, _ t2: Term) -> Term { connect(.compound(.x, [r]), .i, x_(t1, t2)) }

    internal func connect(_ ts: [Term]) -> Term! {
        var ts = ts
        if ts.count < 2 {
            return nil // invalid compound
        }
        if ts.count > 2, let tail = ts.popLast(), let head = connect(ts) {
            return ç.connect(head, self, tail)
        }
        return ç.connect(ts[0], self, ts[1])
    }
    
    internal static func connect(_ t1: Term, _ c: Connector, _ t2: Term) -> Term! {
        var con = c
        let t1t = (c == .c || c == .d) ? Set([Term.symbol(t1.description)]) : Set(t1.terms)
        let t2t = (c == .c || c == .d) ? Set([Term.symbol(t2.description)]) : Set(t2.terms)
        var res = t1t.union(t2t)
        
        if c == .c || c == .d {
            if case .compound(let c1, _) = t1, (c1 == .c || c1 == .d) {
                return nil
            }
            if case .compound(let c2, _) = t2, (c2 == .c || c2 == .d) {
                return nil
            }
        }
        
        guard case .compound = t1, case .compound = t2, (c != .c || c != .d) else {
            // at least one term is a simple term
            guard t1t.intersection(t2t).isEmpty else {
                return nil // terms should not contain each other
            }
            return validate(res) ? .compound(c, [t1, t2]) : nil
        }
        
        // TODO: should we be filtering terms by intensional/extension
        if [.intSet, .extSet, .Ω, .U, .l, .ø].contains(c) {
            if case .compound(let c1, _) = t1,
               case .compound(let c2, _) = t2 {
                if c1 == .x || c1 == .e || c1 == .i
                    || c2 == .x || c2 == .e || c2 == .i {
                    return nil
                }
            }
        }
        
        switch c {
        /// definition 7.1 -- intensional/extensional sets
        case .intSet: res = t1t.union(t2t)
        case .extSet: res = t1t.union(t2t)
        
        /// definition 7.6 -- extensional intersection
        case .Ω: res = t1t.intersection(t2t)
        /// definition 7.7 -- intensional intersection
        case .U: res = t1t.union(t2t)
            
        /// definition 7.8 -- extensional difference
        case .l: res = t1t.subtracting(t2t); con = .U
        /// definition 7.9 -- intensional difference
        case .ø: res = t1t.subtracting(t2t); con = .Ω
            
        /// definition 8.1 -- sequence
        case .x: return .compound(.x, t1.terms + t2.terms)

        /// first term is a relation // TODO: need to validate
        case .e: return .compound(.e, t1.terms + t2.terms)
        case .i: return .compound(.i, t1.terms + t2.terms)
            
        case .n: return nil // handled separately
        case .c: res = t1t.intersection(t2t) // -- extensional difference
        case .d: res = t1t.union(t2t) // -- intensional intersection
        
        case .s: return .compound(.s, t1.terms + t2.terms)
        case .p: return .compound(.p, t1.terms + t2.terms)
        }
        
        // MARK: Validation
        
        // intention/extension sets are allowed one component
        if res.count == 1, case .compound(let c, _) = res.first, c == .intSet || c == .extSet {
            return .compound(con, Array(res))
        }
        
        return validate(res) ? .compound(con, Array(res).sorted()) : nil
    }
    
    /// MARK: helpers
    
    private static func validate(_ s: Set<Term>) -> Bool {
        if s.count < 2 { return false }
        // check if terms contain each other
        let result = s.flatMap { Term.getTerms($0) }
        if result.count != Set(result).count {
            return false 
        }
        return true
    }
}

extension Term {
    public static func word(_ w: String) -> Term { .symbol(w) }
    public static func instance(_ t: Term) -> Term { .compound(ç.extSet, [t]) }
    public static func property(_ t: Term) -> Term { .compound(ç.intSet, [t]) }
    public static func variable(_ s: String) -> Term { .variable(.independent(s)) }
    
    var terms: [Term] {
        switch self {
        case .symbol:
            return [self]
        case .compound(_, let terms):
//            if conn == .n, terms.count == 1, case .statement = terms[0] {
//                return terms[0].terms
//            }
//            return terms.count == 1 ? terms + [.NULL] : terms

//            if terms.count == 1, c == .intSet || c == .extSet {
//                return [self] // do not unwrap {}, []
//            }
//            if terms.count < 3 {
                return terms
//            } else {
//                let head = Array(terms.prefix(through: terms.endIndex-2))
//                let tail = terms.last!
//                return [.compound(c, head), tail]
//            }
        case .statement(let subject, _, let predicate):
            return [subject, predicate]
        case .variable:
            return [self] //TODO: Do we need to recurse into dependent variables?
        case .operation(_, let terms):
            return terms
        }
    }
    
    public var complexity: Double {
        switch self {
        case .symbol:
            return 1
        case .compound(_, let terms):
            return 1 + terms
                .map { $0.complexity }
                .reduce(0, +)
        case .statement(let subject, _, let predicate):
            return 1 + (subject.terms + predicate.terms)
                .map { $0.complexity }
                .reduce(0, +)
        case .variable:
            return 0
        case .operation(_, let terms):
            return 1 + terms
                .map { $0.complexity }
                .reduce(0, +)
        }
    }
    
    public var simplicity: Double {
        rounded(1 / pow(complexity, occamsRazor))
    }
    
    public static let º = Term.symbol("º") // image placeholder
    public static let NULL = Term.symbol("NULL")
    public static let SELF = Term.symbol("SELF")
    
//    static func instance(_ t: Term) -> Term { Term.instance("\(t)") }
//    static func property(_ t: Term) -> Term { Term.property("\(t)") }
    public static func getTerms(_ t: Term) -> [Term] {
        if t.terms.count == 1 {
            return t.terms
        }
        return t.terms.flatMap { getTerms($0) }
    }
}

func pow(_ x: Double, _ y: Int) -> Double {
    let isNegative = y < 0
    var res = 1.0
    for _ in 1...abs(y) {
        res *= x
    }
    return isNegative ? 1 / res : res
}

postfix operator •->
prefix  operator ->•
public extension Term {
    static postfix func •->(_ t: Term) -> Term { instance(t) }
    static prefix  func ->•(_ t: Term) -> Term { property(t) }
}
