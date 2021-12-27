
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

//public protocol a: CustomStringConvertible {
//    func word(_ s: String) -> a
//    func compound(_ t: a, _ ts: [a]) -> a
//}

public indirect enum Term: Hashable {
    case word(String)
//      case instance([Term])
//      case property([Term])
    
    case compound(Term, [Term])
}

/*
extension Term: a {
    public func word(_ s: String) -> a {
        Term.word(s)
    }
    public func compound(_ t: a, _ ts: [a]) -> a {
        Term.word(t.description)
//        Term.compound(t, ts)
    }
}
*/
