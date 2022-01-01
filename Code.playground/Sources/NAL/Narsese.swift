
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

public struct Statement: Hashable {
    public let subject: Term
    public let copula: Copula
    public let predicate: Term
}

public enum Connector: String {
    case a = "ø"
    case extSet = "{}"
    case intSet = "[]"
}

extension Connector {
    var term: Term {
        Term.word(rawValue)
    }
}

//public protocol a: CustomStringConvertible {
//    func word(_ s: String) -> a
//    func compound(_ t: a, _ ts: [a]) -> a
//}

public indirect enum Term: Hashable {
    case word(String)
    case instance(String)
    case property(String)
    
    case compound(Term, [Term])
}

extension Term {
    static func instance(_ t: Term) -> Term { Term.instance("\(t)") }
    static func property(_ t: Term) -> Term { Term.property("\(t)") }
}

postfix operator •->
prefix  operator ->•
public extension Term {
    static postfix func •->(_ t: Term) -> Term { instance(t) }
    static prefix  func ->•(_ t: Term) -> Term { property(t) }
}

postfix operator •
prefix  operator •
public extension String {
    static postfix func •(_ s: String) -> Term { Term.word("\(s)") }
    static prefix  func •(_ s: String) -> Term { Term.word("\(s)") }
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

