import Foundation

// Grammar

public struct Judgement: Hashable {
    public let statement: Statement
    public let truthValue: TruthValue
}

public enum Question: Equatable {
    case statement(Statement)
    case general(Term, Copula)
    case special(Copula, Term)
}

//public struct Statement: Hashable {
//    public let subject: Term
//    public let copula: Copula
//    public let predicate: Term
//}
public enum Statement: Hashable {
    case term(Term)
    case statement(Term, Copula, Term)
}

public enum Connector: String {
//    case a = "ø"
//    case intSet = "[]"  Ω    ¡™¡!`¡``````````¡™£¢∞§¶•ªº–≠«‘“πøˆ¨¥†®´∑œåß∂ƒ©˙˙∆˚¬…æ÷≥≤µ˜∫√ç≈Ω!@#$%^&*()_+|}{":>?<
//    case extSet = "{}"  U

    case Ω = "⋂" /// intensional set
    case U = "⋃" /// extensional set
    case l = "–"
    case ø = "ø"
    
    case x = "⨯"
    
    case e = "/" /// extensional image
    case i = "\\" /// intensional image -- two slashes are because swift
}

public typealias ç = Connector

extension Connector {
    public static var intSet: Connector { Ω }
    public static var extSet: Connector { U }
    
    var term: Term { Term.word(rawValue) }
    
    // TODO: may need to return .word("NULL") if valid compound cannot be created -- investigate
    public static func Ω_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .Ω, t2) }
    public static func U_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .U, t2) }
    public static func l_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .l, t2) }
    public static func ø_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .ø, t2) }
    public static func x_(_ t1: Term, _ t2: Term) -> Term { connect(t1, .x, t2) }

    public static func e_(_ r: Term, _ t1: Term, _ t2: Term) -> Term { connect(.compound(.x, [r]), .e, x_(t1, t2)) }
    public static func i_(_ r: Term, _ t1: Term, _ t2: Term) -> Term { connect(.compound(.x, [r]), .i, x_(t1, t2)) }

    internal static func connect(_ t1: Term, _ c: Connector, _ t2: Term) -> Term! {
        guard case .compound = t1, case .compound = t2 else {
            // simple terms
            return .compound(c, [t1, t2])
        }
        var res: Set<Term> = []
        let t1t = Set(t1.terms)
        let t2t = Set(t2.terms)
        var con = c

        // TODO: should we be filtering terms by intensional/extensional?
        switch c {
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
        }
        print("--\(t1)++\(t2)==\(res)")
        
        //TODO: add validation and return nil if failed
            // used in rule_generator
        
        return .compound(con, Array(res))
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

public indirect enum Term: Hashable {
    case word(String)
    case instance(Term)
    case property(Term)
    case compound(Connector, [Term])
    case statement(Statement)
}

extension Term {
    var terms: [Term] {
        switch self {
        case .word,
             .instance,
             .property:
            return [self]
        case .compound(_, let terms):
            return terms
        case .statement(let statement):
            return statement.terms
        }
    }
    
    public var complexity: Double {
        switch self {
        case .word,
             .instance,
             .property:
            return 1
        case .compound(_, let terms):
            return 1 + terms
                .map { $0.complexity }
                .reduce(0, +)
        case .statement(let statement):
            return 1 + statement.terms
                .map { $0.complexity }
                .reduce(0, +)
        }
    }
    
    public var simplicity: Double {
        rounded(1 / pow(complexity, occamsRazor))
    }

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
        
        self = .word("NULL")
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

