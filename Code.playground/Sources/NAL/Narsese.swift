
// Grammar

public typealias Statement = Term

public indirect enum Term: Hashable {
    case symbol(String) /// <word>
    case compound(Connector, [Term])
    case statement(Term, Copula, Term)
    case variable(Variable)
    case operation(String, [Term])
}

public enum Connector: String, CaseIterable {
    /// intensional set  Ω
    case intSet = "[]"
    /// extensional set U
    case extSet = "{}"
    
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
}

public enum Variable: Hashable {
    case independent(String)
    case dependent(String?, [String])
    case query(String?)
}

public struct Judgement: Hashable {
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

public struct Goal: Hashable {
    public let statement: Statement
    public let desireValue: DesireValue
}

public enum Quest: Hashable {
    case truth
    case desire
}

public struct Question: Hashable {
    public let statement: Statement
    public let type: Quest
    
    public let tense: Tense?
}

public enum Tense: String, Hashable {
    case past    = "<<"
    case present = "||"
    case future  = ">>"
}

