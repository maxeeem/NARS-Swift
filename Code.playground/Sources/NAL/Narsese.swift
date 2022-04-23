import Foundation

// Grammar

public struct Judgement: Hashable {
    public let statement: Statement
    public let truthValue: TruthValue
}

public struct Question: Hashable {
    public let statement: Statement
}

//public enum Question: Equatable {
//    case statement(Statement)
//    case general(Term, Copula)
//    case special(Copula, Term)
//}

//public enum Statement: Hashable {
//    case term(Term)
//    case statement(Term, Copula, Term)
//}

public typealias Statement = Term

public indirect enum Term: Hashable {
    case word(String)
    case compound(Connector, [Term])
    case statement(Term, Copula, Term)
    case variable(Variable)
}

public enum Variable: Hashable {
    case independent(String)
    case dependent(String?, [String])
    case query(String?)
}

public typealias ç = Connector

public enum Connector: String {
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
    
    //    case a = "ø"
    // ¡™¡!`¡``````````¡™£¢∞§¶•ªº–≠«‘“πøˆ¨¥†®´∑œåß∂ƒ©˙˙∆˚¬…æ÷≥≤µ˜∫√ç≈Ω!@#$%^&*()_+|}{":>?<
}

extension Connector {
    var term: Term { Term.word(rawValue) }
    
    public static func Ω_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .Ω, t2) }
    public static func U_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .U, t2) }
    public static func l_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .l, t2) }
    public static func ø_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .ø, t2) }
    public static func x_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .x, t2) }

    public static func e_(_ r: Term, _ t1: Term, _ t2: Term) -> Term { connect(.compound(.x, [r]), .e, x_(t1, t2)) }
    public static func i_(_ r: Term, _ t1: Term, _ t2: Term) -> Term { connect(.compound(.x, [r]), .i, x_(t1, t2)) }

    internal static func connect(_ t1: Term, _ c: Connector, _ t2: Term) -> Term! {
        var con = c
        let t1t = (c == .c || c == .d) ? Set([Term.word(t1.description)]) : Set(t1.terms)
        let t2t = (c == .c || c == .d) ? Set([Term.word(t2.description)]) : Set(t2.terms)
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
        }
        
        // MARK: Validation
        
        // intention/extension sets are allowed one component
        if res.count == 1, case .compound(let c, _) = res.first, c == .intSet || c == .extSet {
            return .compound(con, Array(res))
        }
        
        return validate(res) ? .compound(con, Array(res).sorted()) : nil
    }
    
    /// MARK: helpers
    private static func getTerms(_ t: Term) -> [Term] {
        if t.terms.count == 1 {
            return t.terms
        }
        return t.terms.flatMap { getTerms($0) }
    }
    
    private static func validate(_ s: Set<Term>) -> Bool {
        if s.count < 2 { return false }
        // check if terms contain each other
        let result = s.flatMap { getTerms($0) }
        if result.count != Set(result).count {
            return false 
        }
        return true
    }
}



//extension Term: ExpressibleByArrayLiteral {
//    public init(arrayLiteral elements: String...) {
//        if elements.count == 3,
//           let c = Connector(rawValue: elements[0]) {
//            let t1 = Term(stringLiteral: elements[1])
//            let t2 = Term(stringLiteral: elements[2])
//            self = Connector.connect(t1, c, t2)
//        }
//        self = "NULL"•
//    }
//}

//public protocol a: CustomStringConvertible {
//    func word(_ s: String) -> a
//    func compound(_ t: a, _ ts: [a]) -> a
//}

extension Term {
    static func instance(_ t: Term) -> Term {
        var t = t
        if case .word(let w) = t, w.first != "{" {
            t = .word("{\(t)}")
        }
        return .compound(ç.extSet, [t])
    }
    static func property(_ t: Term) -> Term {
        var t = t
        if case .word(let w) = t, w.first != "[" {
            t = .word("[\(t)]")
        }
        return .compound(ç.intSet, [t])
    }
    
    var terms: [Term] {
        switch self {
        case .word:
            return [self]
        case .compound(_, let terms):
            return terms
        case .statement(let subject, _, let predicate):
            return [subject, predicate]
        case .variable:
            return []
        }
    }
    
    public var complexity: Double {
        switch self {
        case .word:
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
        }
    }
    
    public var simplicity: Double {
        rounded(1 / pow(complexity, occamsRazor))
    }
    
    public static let º = Term.word("º")//image placeholder
    public static let NULL = Term.word("NULL")
    
//    static func instance(_ t: Term) -> Term { Term.instance("\(t)") }
//    static func property(_ t: Term) -> Term { Term.property("\(t)") }
}

postfix operator •->
prefix  operator ->•
public extension Term {
    static postfix func •->(_ t: Term) -> Term { instance(t) }
    static prefix  func ->•(_ t: Term) -> Term { property(t) }
}

postfix operator •
prefix  operator •
public extension String { // turn string into a .word
    static postfix func •(_ s: String) -> Term { Term(stringLiteral: "\(s)") }
    static prefix  func •(_ s: String) -> Term { Term(stringLiteral: "\(s)") }
}

extension Term: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        if value.first == "{" {
            self = .instance(.word(value))
            return
        }
        
        if value.first == "[" {
            self = .property(.word(value))
            return
        }
        
        if value.first == "?" {
            let word = value.dropFirst()
            let name = (word.count == 0) ? nil : String(word)
            self = .variable(.query(name))
            return
        }
        
        // TODO: handle sentences as terms
        let words = value.components(separatedBy: " ")
        
        if words.count == 3,
           let c = ç(rawValue: words[0]) {
            let t1 = Term(stringLiteral: words[1])
            let t2 = Term(stringLiteral: words[2])
            self = ç.connect(t1, c, t2)
            return
        }
        
        if words.count == 1 {
            self = .word(words[0])
            return
        }
        
        self = .NULL
    }
}

//    ("Goofy"•)•->
//    Term("Pluto")•->
//    ->•Term("yellow")

//extension Term: a {
//    public func word(_ s: String) -> a {
//        Term.word(s)
//    }
//    public func compound(_ t: a, _ ts: [a]) -> a {
//        Term.compound(t, ts)
//    }
//}
