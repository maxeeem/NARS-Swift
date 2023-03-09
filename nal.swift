/*
/// Local
///
// TODO: handle variables properly
// independent #x can be merged with independent #y
//
public func revision(j1: Judgement, j2: Judgement) -> Judgement {
    let (f1, c1) = (j1.truthValue.f, j1.truthValue.c)
    let (f2, c2) = (j2.truthValue.f, j2.truthValue.c)
    let f = ((f1 * c1) * (1 - c2) + (f2 * c2) * (1 - c1)) / (c1 * (1 - c2) + c2 * (1 - c1))
    let c = (c1 * (1 - c2) + c2 * (1 - c1)) / (c1 * (1 - c2) + c2 * (1 - c1) + (1 - c1) * (1 - c2))
    return Judgement(j1.statement, TruthValue(f, c, .revision), Judgement.mergeEvidence(j1, j2))
}

public func choice(j1: Judgement, j2: Judgement) -> Judgement {
    j1.statement == j2.statement ?
        (j1.truthValue.c > j2.truthValue.c) ? j1 : j2
    :
        (and(j1.truthValue.e, j1.statement.simplicity) 
            > 
            and(j2.truthValue.e, j2.statement.simplicity)) ? j1 : j2
}

/// Immediate

public func negation(j1: Judgement) -> Judgement {
    let f = 1 - j1.truthValue.f
    let c = j1.truthValue.c
    let cs = neg(j1.statement)
    let cj = cs + (f, c, ETERNAL)
    return Judgement(cs, TruthValue(f, c, .negation), Judgement.mergeEvidence(j1, cj), tense: j1.tense, timestamp: j1.timestamp)
}

public func conversion(j1: Judgement) -> Judgement? {
    guard case .statement(let s, let copula, let p) = j1.statement,
          copula == .inheritance || copula == .implication else {
        return nil // invalid statement
    }
    let (f, c) = (j1.truthValue.f, j1.truthValue.c)
    let c1 = f * c / (f * c + k)
    let cs = Term.statement(p, copula, s)
    let cj = cs + (1, c1, ETERNAL)
    return Judgement(cs, TruthValue(1, c1, .conversion), Judgement.mergeEvidence(j1, cj), tense: j1.tense, timestamp: j1.timestamp)
}

public func contraposition(j1: Judgement) -> Judgement? {
    guard case .statement(let s, let copula, let p) = j1.statement,
          copula == .implication else {
        return nil // invalid statement
    }
    let (f, c) = (j1.truthValue.f, j1.truthValue.c)
    let c1 = (1 - f) * c / ((1 - f) * (c + k))
    let cs = neg(p) => neg(s)
    let cj = cs + (0, c1, ETERNAL)
    return Judgement(cs, TruthValue(0, c1, .contraposition), Judgement.mergeEvidence(j1, cj), tense: j1.tense, timestamp: j1.timestamp)
}

private func neg(_ s: Statement) -> Statement {
    if case .compound(let conn, let terms) = s, conn == .n, terms.count == 1 {
        return terms[0] // double negative
    } else {
        return .compound(.n, [s])
    }
}

public extension Rules {
    var allRules: [Rule] {
        let rules = firstOrder + higherOrder + compositional + conditionalSyllogistic
        var permutations: [Rule] = []
        for r in rules {
            let (p1, p2, c, tf) = r
            var sp1: Statement!
            var sp2: Statement!
            if case .statement(let s, let copula, let p) = p1 {
                if copula == .similarity || copula == .equivalence {
                    sp1 = .statement(p, copula, s)
                }
            }
            if case .statement(let s, let copula, let p) = p2 {
                if copula == .similarity || copula == .equivalence {
                    sp2 = .statement(p, copula, s)
                }
            }
            if sp1 != nil {
                permutations.append((sp1, p2, c, tf))
            }
            if sp2 != nil {
                permutations.append((p1, sp2, c, tf))
            }
            if sp1 != nil && sp2 != nil {
                permutations.append((sp1, sp2, c, tf))
            }
        }
        return rules + permutations
    }

    var higherOrder: [Rule] {
        return firstOrder.map { (arg) in
            var (p1, p2, c, tf) = arg
            p1 = replaceCopulas(p1)
            p2 = replaceCopulas(p2)
            c = replaceCopulas(c)
            return (p1, p2, c, tf)
        }
    }
    
    var firstOrder: [Rule] {
        let S = Term.var("S")
        let P = Term.var("P")
        let M = Term.var("M")

        switch self {
        case .deduction:
            return [(M --> P,     S --> M, S --> P, tf),
                    (P --> M,     M --> S, P --> S, tfi)]
        case .induction:
            return [(M --> P,     M --> S, S --> P, tf),
                    (M --> P,     M --> S, P --> S, tfi)]
        case .abduction:
            return [(P --> M,     S --> M, S --> P, tf),
                    (P --> M,     S --> M, P --> S, tfi)]
        case .exemplification:
            return [(P --> M,     M --> S, S --> P, tf),
                    (M --> P,     S --> M, P --> S, tfi)]
        case .comparison:
            return [(M --> P,     M --> S, S <-> P, tf),
                    (P --> M,     S --> M, S <-> P, tfi)]
        case .analogy:
            return [(M --> P,     S <-> M, S --> P, tf),
                    (P --> M,     S <-> M, P --> S, tf),
                    (M <-> P,     S --> M, S --> P, tfi),
                    (M <-> P,     M --> S, P --> S, tfi)]
        case .resemblance:
            return [(M <-> P,     S <-> M, S <-> P, tf)]
            
        default:
            return [] // other rules are handled separately
        }
    }
    
    var compositional: [Rule] {
        let M = Term.var("M")
        let T1 = Term.var("T1")
        let T2 = Term.var("T2")
        
        switch self {
        case .intersection:
            return [ /// first order
                (M --> T1,    M --> T2,    M --> (T1 & T2), tf),
                (T1 --> M,    T2 --> M,    (T1 | T2) --> M, tf),
                /// higher order
                ( M => T1,    M => T2 ,    M => (T1 && T2), tf),
                ( T1 => M,    T2 => M ,    (T1 || T2) --> M, tf),
                /// conditional
                (      T1,          T2,    (T1 && T2), tf) // TODO: verify nothing else needs to be checked
            ]
        case .union:
            return [ /// first order
                (M --> T1,    M --> T2,    M --> (T1 | T2), tf),
                (T1 --> M,    T2 --> M,    (T1 & T2) --> M, tf),
                /// higher order
                ( M => T1,    M => T2 ,    M => (T1 || T2), tf),
                ( T1 => M,    T2 => M ,    (T1 && T2) --> M, tf),
                /// conditional
                (      T1,          T2,    (T1 || T2), tf) // TODO: verify nothing else needs to be checked
            ]
        case .difference:
            return [
                (M --> T1,    M --> T2,    M --> (T1 - T2), tf),
                (M --> T1,    M --> T2,    M --> (T2 - T1), tfi),
                (T1 --> M,    T2 --> M,    (T1 ~ T2) --> M, tf),
                (T1 --> M,    T2 --> M,    (T2 ~ T1) --> M, tfi)
            ]
        default:
            return []
        }
    }
    
    var conditionalSyllogistic: [Rule] {
        let S = Term.var("S")
        let P = Term.var("P")
        switch self {
        case .deduction:
            return [(S  => P,           S,       P, tf)]
        case .abduction:
            return [(P  => S,           S,       P, tf)]
        case .analogy:
            return [(      S,     S <=> P,       P, tf)]
        default:
            return []
        }
    }
    
    /// special set of rules handled separately during inference
    /// premises must be seen as based on the same implicit condition
    
    var conditional: [Rule] {
        let S = Term.var("S")
        let P = Term.var("P")
        let M = Term.var("M")
        let C = Term.var("C")
        let T1 = Term.var("T1")
        let T2 = Term.var("T2")
        switch self {
        case .deduction:
            return [
                ((C && S) => P,                 S,             C  => P, tf),
                ((C && S) => P,            M => S,        (C && M) => P, tf)
            ]
        case .abduction:
            return [
                ((C && S) => P,            C => P,                   S, tf),
                ((C && S) => P,     (C && M) => P,              M => S, tf)
            ]
        case .induction:
            return [
                (       C => P,                 S,     (C && S) => P, tf),
                ((C && M) => P,            M => S,     (C && S) => P, tf)
            ]
        case .intersection:
            return [
                (             T1,                T2,        (T1 && T2), tf) // TODO: verify nothing else needs to be checked
            ]
        default:
            return []
        }
    }
    
    var variable_and_temporal: [Rule] {
        let S = Term.var("S")
        let P = Term.var("P")
        switch self {
        case .induction:
            return [(P,  S,  S  => P, tf)]
        case .comparison:
            return [(S,  P,  S <=> P, tf)]
        default:
            return []
        }
    }
    
}

extension Theorems {
    public var rules: [Statement] {
        let S = Term.var("S")
        let P = Term.var("P")
        let S1 = Term.var("S1")
        let S2 = Term.var("S2")

        let T1 = Term.var("T1")
        let T2 = Term.var("T2")

        switch self {
        case .inheritance:
            return [
                (T1 & T2) --> (T1),
                (T1 - T2) --> (T1)
            ]
        case .similarity:
            return [
                -(-T1) <-> (T1)
            ]
        case .implication:
            return [
                (S <-> P) => (S --> P),
                (S <=> P) => (S => P),
                (S1 && S2) => (S1)
            ]
        case .equivalence:
            return [
                (S <-> P) <=> &&[(S --> P), (P --> S)],
                (S <=> P) <=> &&[(S  => P), (P  => S)],
                
                (S <-> P) <=> (.instance(S) <-> .instance(P)),
                (S <-> P) <=> (.property(S) <-> .property(P)),
                
                (S --> .instance(P)) <=> (S <-> .instance(P)),
                (.property(S) --> P) <=> (.property(S) <-> P)
            ]
        }
    }
}


// MARK: - Helpers

extension Rules {
    // utility
    private func replaceCopulas(_ statement: Statement) -> Statement {
        var statement = statement
        if case .statement(let s, let c, let p) = statement {
            if c == .inheritance {
                statement = .statement(s, .implication, p)
            }
            if c == .similarity {
                statement = .statement(s, .equivalence, p)
            }
        }
        return statement
    }
}


// Grammar

public typealias Statement = Term

public indirect enum Term: Hashable {
    case symbol(String) /// <word>
    case compound(Connector, [Term])
    case statement(Term, Copula, Term)
    case variable(Variable)
    case operation(String, [Term])
}

public enum Copula: String, CaseIterable {
    //// Primary
    case inheritance       =    "->" // NAL 1
    case similarity        =   "<–>"     // 2
    case implication       =    "=>"     // 5
    case equivalence       =   "<=>"     // 5
    //// Secondary
    case instance          =   "•–>"     // 2
    case property          =    "–>•"    // 2
    case insProp           =   "•->•"    // 2
    //// Temporal
    case predictiveImp     =   "/=>"     // 7
    case retrospectiveImp  =  "\\=>"     // 7 - note: second slash is bc escape char in Swift
    case concurrentImp     =   "|=>"     // 7
    case predictiveEq      =  "/<=>"     // 7
    case retrospectiveEq   =  "\\<=>"    // 7 - note: book describes it as optional
    case concurrentEq      =  "|<=>"     // 7
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
    
    public var timestamp: UInt32 = 0
    
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


public typealias Theorem = (Statement) -> Statement?

public enum Theorems: CaseIterable {
    case inheritance
    case similarity
    case implication
    case equivalence
}

extension Theorems {
    public static func apply(_ j: Judgement) -> [Judgement] {
        let res: [[Statement]] = self.allCases.map {
            var results = $0.rules.compactMap { Term.match(t: $0, s: j.statement) }
            if case .statement(let s, let c, let p) = j.statement, c == .similarity || c == .equivalence {
                results.append(contentsOf: $0.rules.compactMap { Term.match(t: $0, s: .statement(p, c, s)) })
            }
            if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
                if terms.count == 2 { // TODO: handle compounds with multiple terms
                    results.append(contentsOf: $0.rules.compactMap { Term.match(t: $0, s: .compound(conn, terms.reversed())) })
                }
            }
            return results
        }

        let results: [[Judgement]] = res.flatMap{$0}.map { t in
            var rel = reliance
            if case .statement(let sub, let cop, _) = t, cop == .equivalence {
                rel = j.statement == sub ? 0.9 : 1.0
            }
            var results = Rules.strong.flatMap {
                $0.apply((j, t-*(1,rel, ETERNAL)))
            }.compactMap { $0 }
            
            if case .statement(let s, let c, let p) = j.statement, c == .similarity || c == .equivalence {
                results.append(contentsOf:
                    Rules.strong.flatMap {
                    $0.apply((Judgement(.statement(p, c, s), j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp), t-*(1,reliance, ETERNAL)))
                    }.compactMap { $0 }
               )
            }
            if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
                if terms.count == 2 { // TODO: handle compounds with multiple terms
                    results.append(contentsOf:
                        Rules.strong.flatMap {
                        $0.apply((Judgement(.compound(conn, terms.reversed()), j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp), t-*(1,reliance, ETERNAL)))
                        }.compactMap { $0 }
                   )
                }
            }
            return results
        }
        
        let unique = results.flatMap({$0}).removeDuplicates()
        return unique
    }
}


// MARK: - Helper

extension Term {
    static func match(t: Statement, s: Statement) -> Statement? {
        var results = [Term]()
        let goal = t.terms.map({ $0.logic() === s.logic() }).reduce(success, ||)
        
        for sol in solve(goal) {
            //                print(sol)
            let ts = s.terms.flatMap({ $0.terms.map({ $0.logic() }) })
            
            let valid = sol.allSatisfy { (v, _) in
                !ts.contains { $0.equals(v) }
            }
            
            if valid {
                var result = t
                for item in sol {
                    result = result.replace(termName: item.LogicVariable.name, term: .from(logic: item.LogicTerm))
                }
                if result != t {
                    results.append(result)
                }
            }
        }
        
        return results.min(by: { $0.complexity < $1.complexity })
    }
}

public struct TruthValue: Hashable {
    public let frequency: Double
    public let confidence: Double
    
    public let rule: Rules! // for derived values
}

public extension TruthValue {
    var f: Double {frequency}
    var c: Double {confidence}
    var l: Double {lowerFrequency}
    var u: Double {upperFrequency}
    var wpos: Double {positiveEvidence}
    var wtot: Double {totalEvidence}
    var e: Double {expectation}
    
    var positiveEvidence: Double { k * f * c / (1 - c) }
    var totalEvidence: Double { k * c / (1 - c) }
    var lowerFrequency: Double { f * c }
    var upperFrequency: Double { 1 - c * (1 - f) }
    var expectation: Double { (l + u) / 2 }
}

extension TruthValue {
    static var tautology: TruthValue { TruthValue(1, 1) }
}

public typealias TruthFunction = (TruthValue, TruthValue) -> TruthValue

infix operator ~ // rule to truth function mapping
private func ~(_ r: (Rules, Bool), _ tf: @escaping TruthFunction) -> TruthFunction {
    { (tv1, tv2) in
        let (rule, inverse) = r
        let tv = inverse ? tf(tv2, tv1) : tf(tv1, tv2)
        return TruthValue(tv.f, tv.c, rule)
    }
}

extension TruthValue {
    static func truthFunction(_ r: Rules, _ i: Bool) -> TruthFunction {
        switch r {
        case .deduction:       return (r,i)~deduction
        case .induction:       return (r,i)~induction
        case .abduction:       return (r,i)~abduction
        case .exemplification: return (r,i)~exemplification
        case .comparison:      return (r,i)~comparison
        case .analogy:         return (r,i)~analogy
        case .resemblance:     return (r,i)~resemblance
            
        case .intersection:    return (r,i)~intersection
        case .union:           return (r,i)~union
        case .difference:      return (r,i)~difference
        
        default:               return (r,i)~{_,_ in .tautology }
        }
    }
    
    static var deduction: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, f2)
        let c = and(f1, f2, c1, c2)
        return TruthValue(f, c)
    }
    static var induction: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let positive = and(f2, c2, f1, c1) // w+
        let total = and(f2, c2, c1) // w
        let evidence = Evidence(positive, total)
        return TruthValue(evidence)
    }
    static var abduction: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let positive = and(f1, c1, f2, c2) // w+
        let total = and(f1, c1, c2) // w
        let evidence = Evidence(positive, total)
        return TruthValue(evidence)
    }
    static var exemplification: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let positive = and(f1, c1, f2, c2) // w+
        let total = and(f1, c1, f2, c2) // w
        let evidence = Evidence(positive, total)
        return TruthValue(evidence)
    }
}

extension TruthValue {
    static var comparison: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let positive = and(f1, c1, f2, c2) // w+
        let total = and(or(f1, f2), c1, c2) // w
        let evidence = Evidence(positive, total)
        return TruthValue(evidence)
    }
    static var analogy: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, f2)
        let c = and(f2, c1, c2)
        return TruthValue(f, c)
    }
    static var resemblance: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, f2)
        let c = and(or(f1, f2), c1, c2)
        return TruthValue(f, c)
    }
}

extension TruthValue {
    static var intersection: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, f2)
        let c = and(c1, c2)
        return TruthValue(f, c)
    }
    static var union: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = or(f1, f2)
        let c = and(c1, c2)
        return TruthValue(f, c)
    }
    static var difference: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, not(f2))
        let c = and(c1, c2)
        return TruthValue(f, c)
    }
}


/// Extended Boolean operators
/// bounded by the range from 0 to 1
public func not(_ x: Double) -> Double {
    1 - x
}
public func and(_ xs: Double...) -> Double {
    xs.reduce(1, { $0 * $1 })
}
public func or(_ xs: Double...) -> Double {
    1 - xs.reduce(1, { $0 * (1 - $1)})
}
/// Statement is a fundamental type
public typealias Rule = (Statement, Statement, Statement, TruthFunction)
public typealias Apply = (_ judgements: (Judgement, Judgement)) -> Judgement? // reduce operation

public typealias Infer = (Judgement) -> Judgement? /// Single-premise rules

public enum Rules: String, CaseIterable {
    // NAL-1
    case deduction
    case induction
    case abduction
    case exemplification
    // NAL-2
    case comparison
    case analogy
    case resemblance
    // Compositional
    case intersection
    case union
    case difference
    // Local
    case negation
    case conversion
    case contraposition
    case revision
}

public extension Rules {
    static let strong: [Rules] = [.deduction, .analogy, .resemblance]
    //TODO: should we add intersection, difference and union to the list?
    
    static func immediate(_ j: Judgement) -> [Judgement] {
        let immediate: [Infer] = [/*negation(j1:),*/ conversion(j1:), contraposition(j1:)]
        return immediate.compactMap { $0(j) }
    }
}

extension Rules {
    var tf: TruthFunction {
        TruthValue.truthFunction(self, false)
    }
    var tfi: TruthFunction { // inverse
        TruthValue.truthFunction(self, true)
    }
    public var apply: (_ judgements: (Judgement, Judgement)) -> [Judgement?] {
        { j in
            var (j1, j2) = j
//            print("\n>>>", j)
            
            var t1 = j1.statement // test
            var t2 = j2.statement // test
//            print(p1, p2, j1, j2)
    //        print("=", commonTerms)
    //        return nil
            
            if case .compound(let conn, let ts1) = t1, conn == .n {
//                print("1.", t1, t2)
                if ts1[0] == t2 { // TODO: use similarity helper to account for symmetrical connectors and copulas
                    return [] // no conclusion can be reached if premises are just opposite of each other
                }
            }
            if case .compound(let conn, let ts2) = t2, conn == .n {
//                print("2.", t1, t2)
                if ts2[0] == t1 { // TODO: use similarity helper to account for symmetrical connectors and copulas
                    return [] // no conclusion can be reached if premises are just opposite of each other
                }
            }
             
            /// temporal
            
//            if j1.truthValue.rule == nil && j2.truthValue.rule == nil {
//                
//            }
            
            /// variable elimination
//            print("before")
//            print("t1", t1)
//           print("t2", t2)
//           print("")
            /// independent
            t1 = variableEliminationIndependent(t1, t2)
            t2 = variableEliminationIndependent(t2, t1)
//            print("after")
//             print("t1", t1)
//            print("t2", t2)
//            print("")
            /// dependent
            if let result = variableEliminationDependent(t1, t2, j1, j2, self) {
                return result
            } else if let result = variableEliminationDependent(t2, t1, j2, j1, self) {
                return result
            }
            
            /// original code
            
            j1 = Judgement(t1, j1.truthValue, j1.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
            j2 = Judgement(t2, j2.truthValue, j2.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
           
            var x: [Judgement?] = []
                        
            // apply rules
            self.allRules.forEach { r in
                x.append(rule_generator(r)((j1, j2)))
            }
            // switch order of premises
            self.allRules.forEach { r in
                x.append(rule_generator(r)((j2, j1)))
            }
            
            // MARK: Variable introduction
            // “In all these rules a dependent variable is only introduced into a conjunction or intersection, and an independent variable into (both sides of) an implication or equivalence.”
            
            /// independent-variable introduction
            ///
            // TODO: introVarInner from CompositionalRules in OpenNARS
            ///
            if self == .induction || self == .comparison {
                x.append(contentsOf: variableIntroductionIndependent(t1, t2, j1, j2, self))
            }
            
            /// dependent-variable introduction
            
            if self == .intersection {
                x.append(contentsOf: variableIntroductionDependent(t1, t2, j1, j2, self))
            }
            
            ///
            // TODO: multi-variable introduction rules
            ///
            
            let unique = x.compactMap({$0}).removeDuplicates()
//            print("+++", x)
//            print("===", unique)
            return unique
        }
    }
}

// MARK: Rule application

private var checkOverlap = false // TODO: dirty trick to get dependent-variable introduction to work

public var rule_generator: (_ rule: Rule) -> Apply {
    { (arg) -> ((Judgement, Judgement)) -> Judgement? in
        // premise (p1) premise (p2) conclusion (c) truth-function (tf)
        var (p1, p2, c, tf) = arg
        
        return { (arg) in
            let (j1, j2) = arg
            
            if let j1t = j1.tense, let j2t = j2.tense, j1t != j2t {
                return nil // temporal order cannot be determined
            }
            
            /*
             * MARK: do temporal
             */
            
            // TODO: need to check copulas to ensure temporal order can be established
            
            guard let temporal = temporalReasoning(c) else {
                return nil // outside temporal window
            }
            
            c = temporal // introduce temporal copulas
            
            /*
             * MARK: appply logic
             */
            
            guard var result = logicReasoning(c) else {
                return nil // no conclusion
            }
            
            // TODO: come up with a better way
            result = determineOrder()
            result = accountForExemplification()
            
            //        print("here", result)
            
            // TODO: handle temporal compounds
            // TODO: check that compounds do not contain each other
            
            /*
             * MARK: check results
             */
            
            if let statement = validate(result), !statement.isTautology {
                //            print("accepted", result)
                let truthValue = tf(j1.truthValue, j2.truthValue)
                let derivationPath = Judgement.mergeEvidence(j1, j2)
                //            print("--")
                //            print(j1, j2)
                //            print("accepted", statement, truthValue, c)
                return Judgement(statement, truthValue, derivationPath, tense: j1.tense ?? j2.tense)
            }
            
            return nil // PROGRAM END
            
            
            // MARK: - helpers
            
            func temporalReasoning(_ t: Term) -> Term? {
                if j1.timestamp != ETERNAL, j2.timestamp != ETERNAL,
                   case .statement(var cs, var cc, var cp) = t,
                   cc == .implication || cc == .equivalence {
                    let forward = j1.timestamp < j2.timestamp
                    let delta = forward ? j2.timestamp - j1.timestamp : j1.timestamp - j2.timestamp
                    
                    let window = 50 * 1000000 // 50 ms
                    let distance = 10
                    // delta should be less than some param
                    // otherwise rules should not apply
                    // here we choose 1 second computed as
                    // distance factor on either side of the window
                    
                    guard delta < window * 2 * distance else {
                        return nil
                    }
                    //                print("N", delta, window, forward, j1, j2)
                    //                print(j1.timestamp, j2.timestamp)
                    
                    if delta < window {
                        //                    print("||", t)
                        cc = cc.concurrent
                    } else if forward {
                        //                    print(">>", t)
                        cc = cc.predictive
                    } else {
                        //                    print("<<", t)
                        cc = cc.retrospective
                    }
                    
                    // use temporal conclusion
                    return .statement(cs, cc, cp)
                }
                
                return t // use original conclusion
            }
            
            func logicReasoning(_ t: Term) -> Term? {
                var result = t
                var map: [String: Term] = [:]
                let test1: LogicGoal = (p1.logic() === j1.statement.logic())
                let test2: LogicGoal = (p2.logic() === j2.statement.logic())
                
                let substitution = solve(test1 && test2).makeIterator().next()
                
                // TODO: use LogicVariableFactory to avoid collision with terms
                // i.e. terms names "S" and "P" will fail a check below and produce no conclusion
                
                if let sol = substitution {
                    //            print("\n---SOL---\n", sol, "\n")
                    let ts = (p1.terms + p2.terms + c.terms).flatMap({ $0.terms.map({ $0.logic() }) })
                    let valid = sol.allSatisfy { (v, _) in
                        ts.contains { $0.equals(v) }
                    }
                    
                    if valid {
                        for item in sol {
                            //                print(item.LogicVariable.name)
                            //                print(type(of: item.LogicTerm))
                            result = result.replace(termName: item.LogicVariable.name, term: .from(logic: item.LogicTerm))
                            //                print("result\n", result)
                        }
                    }
                }
                
                //            print("}}}}", j1, j2, result)
                
                return (result == t) ? nil : result
            }
            
            func validate(_ term: Term) -> Term? {
                switch term {
                    
                    // TODO: difference connectors take exactly 2 terms
                    
                case .compound(let connector, let terms):
                    if terms.count == 0 {
                        return nil // empty compound
                    }
                    if terms.count == 1 {
                        if connector == .intSet || connector == .extSet {
                            if case .compound(let c, let ts) = terms[0] {
                                if ts.count == 1, c == .intSet || c == .extSet {
                                    return nil // prevent nesting i.e. [{x}], {{x}}, [[x]], {[x]}
                                }
                            }
                            return term // instances and properties are allowed one component
                        }
                        //                    print("here", term)
                        if connector == .x || connector == .i || connector == .e {
                            return term
                        }
                        return nil
                    }
                    if checkOverlap && j1.evidenceOverlap(j2) {
                        return nil
                    }
                    if connector == .x || connector == .i || connector == .e {
                        return term
                    }
                    return connector.connect(terms)
                    
                case .statement(let subject, let cop, let predicate):
                    if let sub = validate(subject), let pre = validate(predicate) {
                        //                    if case .compound(let cs, _) = subject, case .compound(let cp, _) = predicate {
                        //                        if cs == .x && cp == .x {
                        //                            return nil
                        //                        }
                        //                    }
                        return .statement(sub, cop, pre)
                    }
                    return nil
                    
                default:
                    return term
                }
            }
            
            func determineOrder() -> Term {
                // TODO: get rid of this dirty trick and determine temporal order of the conclusion properly
                if case .statement(var cs, var cc, var cp) = result, cc == .equivalence || cc == .implication {
                    if case .statement(let j1s, let j1c, let j1p) = j1.statement,
                       case .statement(let j2s, let j2c, let j2p) = j2.statement {
                        
                        if (j1c.isConcurrent && j2c.isConcurrent) {
                            // both concurrent
                            return .statement(cs, cc.concurrent, cp)
                        }
                        
                        if (j1c.isPredictive && j2c.isPredictive) {
                            // both predicitve
                            return .statement(cs, cc.predictive, cp)
                        }
                        
                        if (j1c.isRetrospective && j2c.isRetrospective) {
                            // both retrospective
                            return .statement(cs, cc.retrospective, cp)
                        }
                        
                        if (j1c.isConcurrent && j2c.isPredictive)
                            || (j2c.isConcurrent && j1c.isPredictive) {
                            // one is concurrent, another is predictive
                            return .statement(cs, cc.predictive, cp)
                        }
                        
                        if (j1c.isConcurrent && j2c.isRetrospective)
                            || (j2c.isConcurrent && j1c.isRetrospective) {
                            // one is concurrent, another is retrospective
                            return .statement(cs, cc.retrospective, cp)
                        }
                        
                        // Complex
                        var list = [Term]()
                        
                        if j1c.isPredictive && j2c.isRetrospective {
                            list = [j1s, j1p]
                            if let idx = list.firstIndex(of: j2s) {
                                list.insert(j2p, at: idx)
                            }
                            if let idx = list.firstIndex(of: j2p) {
                                list.insert(j2s, at: idx+1)
                            }
                        } else if j2c.isPredictive && j1c.isRetrospective {
                            list = [j2s, j2p]
                            if let idx = list.firstIndex(of: j1s) {
                                list.insert(j1p, at: idx)
                            }
                            if let idx = list.firstIndex(of: j1p) {
                                list.insert(j1s, at: idx+1)
                            }
                        }
                        
                        if let cpi = list.firstIndex(of: cp), let csi = list.firstIndex(of: cs) {
                            if cpi > csi {
                                // predicate after subject
                                return .statement(cs, cc.predictive, cp)
                            } else {
                                // predicate before subject
                                return .statement(cs, cc.retrospective, cp)
                            }
                        }                    
                    }
                }
                //            if result == (("John" * "key_101") --> "hold") {
                //                // set tense for the conclusion
                //            }
                return result
            }
            
            func accountForExemplification() -> Term {
                let rule = tf(j1.truthValue, j2.truthValue).rule
                
                if rule == .exemplification {
                    if case .statement(let rs, let rc, let rp) = result {
                        if rc.isPredictive {
                            return .statement(rs, rc.atemporal.retrospective, rp)
                        } else if rc.isRetrospective {
                            return .statement(rs, rc.atemporal.predictive, rp)
                        }
                    }
                }
                
                return result
            }
        }
    }
}


// MARK: - Variable introduction and elimination

private func variableEliminationIndependent(_ t1: Statement, _ t2: Statement) -> Statement {
    if case .statement(_, let cop1, _) = t1, cop1 == .implication || cop1 == .equivalence {
        return Term.match(t: t1, s: t2) ?? t1
    }
    return t1
}

private func variableEliminationDependent(_ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?]? {
    if case .compound(let conn, _) = t1, conn == .c || conn == .U || conn == .Ω {
        var x: [Judgement?] = []
        
        if let h = Term.match(t: t1, s: t2) {
            let tv = TruthValue.deduction(j1.truthValue, TruthValue(1, reliance))

            let res = r.allRules.flatMap { r in
                h.terms.flatMap { (t: Term) -> [Judgement?] in
                    let j = Judgement(t, tv, j1.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
                    return [rule_generator(r)((j, j2)),
                            rule_generator(r)((j2, j))]
                }
            }
            
            for rs in res.compactMap({ $0 }) {
                x.append(contentsOf: Rules.allCases.flatMap { r in
                    r.apply((rs, j2))
                })
            }
        }
        
        return x.isEmpty ? nil : x
    }
    return nil
}

private func variableIntroductionIndependent(_ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?] {
    variableIntroduction(dependent: false, t1, t2, j1, j2, r)
}

private func variableIntroductionDependent(_ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?] {
    variableIntroduction(dependent: true, t1, t2, j1, j2, r)
}

private func variableIntroduction(dependent: Bool, _ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?] {
    var x: [Judgement?] = []
    if case .statement(_, let cop1, _) = t1, cop1 == .inheritance,
       case .statement(_, let cop2, _) = t2, cop2 == .inheritance {
        
        let common = Set(t1.terms).intersection(t2.terms)
        
        if !common.isEmpty {
            
            if dependent { checkOverlap = false }
            
            var vari = r.conditional.flatMap { r in
                [rule_generator(r)((j1, j2)),
                 rule_generator(r)((j2, j1))] // switch order of premises
            }.compactMap { $0 }
            
            if dependent == false { // Table 10.3
                vari.append(contentsOf: r.variable_and_temporal.flatMap { r in
                    [rule_generator(r)((j1, j2)),
                     rule_generator(r)((j2, j1))] // switch order of premises
                }.compactMap { $0 })
            }
            
            if dependent { checkOverlap = true }
            
            let rep: [Judgement] = vari.compactMap { j in
                var r = j.statement
                for (i, c) in common.enumerated() {
                    if dependent {
                        r = r.replace(termName: c.description, depVarName: "x\(i)")
                    } else {
                        r = r.replace(termName: c.description, indepVarName: "x\(i)")
                    }
                }
                if j.statement.description == r.description {
                    return nil // variable substitution was not successful
                }
                return Judgement(r, j.truthValue, j.derivationPath)
            }
            
            x.append(contentsOf: rep)
        }
    }
    return x
}
//  “The systems show different “personalities” when predicting the future,
//   and larger k corresponds to more conservative and risk-averse behavior.”

public let evidentialHorizon: Double = 1 // "personality parameter"

public let occamsRazor: Int = 1
public let reliance: Double = 0.9


public var k: Double { evidentialHorizon }
public var r: Int { occamsRazor }


public let ETERNAL: UInt32 = UInt32.max

precedencegroup Copula { // priority
    higherThan: ComparisonPrecedence
}

infix operator -->    : Copula // "->"
infix operator <->    : Copula
infix operator  =>    : Copula
infix operator <=>    : Copula

infix operator •->    : Copula
infix operator  ->•   : Copula
infix operator •->•   : Copula

infix operator >>|=>  : Copula //  "/=>"  future
infix operator <<|=>  : Copula //  "\=>"   past
infix operator   |=>  : Copula //  "|=>"  present
infix operator >>|<=> : Copula //  "/<=>"
infix operator <<|<=> : Copula //  "\<=>"
infix operator   |<=> : Copula //  "|<=>"

// convenience initializer for Judgement
public func + (_ s: Statement, fc: (Double, Double, UInt32)) -> Judgement {
    Judgement(s, TruthValue(fc.0, fc.1), timestamp: fc.2)
}

postfix operator -*
public extension Statement {
    static postfix func -* (_ s: Statement) -> Judgement {
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

infix operator -* : Copula
public func -* (_ s: Statement, _ fc: (Double, Double, UInt32)) -> Judgement {
    s + fc
}

postfix operator •->
prefix  operator ->•
public extension Term {
    static postfix func •-> (_ t: Term) -> Term { instance(t) }
    static prefix  func ->• (_ t: Term) -> Term { property(t) }
}

// NAL-1
public func -->  (_ s: Term, p: Term) -> Statement { .statement( s    , .inheritance ,    p ) }
// NAL-2
public func <->  (_ s: Term, p: Term) -> Statement { .statement( s    , .similarity  ,    p ) }
public func •->  (_ s: Term, p: Term) -> Statement { .statement( s•-> , .inheritance ,    p ) }
public func ->•  (_ s: Term, p: Term) -> Statement { .statement( s    , .inheritance , ->•p ) }
public func •->• (_ s: Term, p: Term) -> Statement { .statement( s•-> , .inheritance , ->•p ) }
// NAL-5
public func =>   (_ s: Term, p: Term) -> Statement { .statement( s    , .implication ,    p ) }
public func <=>  (_ s: Term, p: Term) -> Statement { .statement( s    , .equivalence ,    p ) }
// NAL-7
public func >>|=>   (_ s: Term, p: Term) -> Statement { .statement( s    , .predictiveImp    ,    p ) }
public func <<|=>   (_ s: Term, p: Term) -> Statement { .statement( s    , .retrospectiveImp ,    p ) }
public func   |=>   (_ s: Term, p: Term) -> Statement { .statement( s    , .concurrentImp    ,    p ) }

prefix operator >> //  "/=>"  future
prefix operator << //  "\=>"   past
prefix operator || //  "|=>"  present

public prefix func >> (_ s: Term) -> Statement { .NULL >>|=> s } /// it will rain
public prefix func << (_ s: Term) -> Statement { .NULL <<|=> s } /// it rained
public prefix func || (_ s: Term) -> Statement { .NULL   |=> s } /// it's raining

extension Statement {
    static prefix func - (_ s: Statement) -> Statement { .compound(.n, [s]) }
}

public func  & (_ lhs: Statement, _ rhs: Statement) -> Statement {  +[lhs, rhs] }
public func  | (_ lhs: Statement, _ rhs: Statement) -> Statement {  %[lhs, rhs] }
public func  - (_ lhs: Statement, _ rhs: Statement) -> Statement {  -[lhs, rhs] }
public func  ~ (_ lhs: Statement, _ rhs: Statement) -> Statement {  ~[lhs, rhs] }
public func  * (_ lhs: Statement, _ rhs: Statement) -> Statement {  *[lhs, rhs] }
public func && (_ lhs: Statement, _ rhs: Statement) -> Statement { &&[lhs, rhs] }
public func || (_ lhs: Statement, _ rhs: Statement) -> Statement { ||[lhs, rhs] }

prefix operator  * // product
prefix operator && // conjunction
prefix operator  % // intensional intersection
/* -- leave commented out --
prefix operator  + // extensional intersection
prefix operator  - // extensional difference
prefix operator  ~ // intensional difference
prefix operator || disjunction
-- already declared elsewhere -- */

extension Array where Element == Statement {
    public static prefix func  + (_ s: Array<Statement>) -> Statement { ç.Ω.connect(s) }
    public static prefix func  % (_ s: Array<Statement>) -> Statement { ç.U.connect(s) }
    public static prefix func  - (_ s: Array<Statement>) -> Statement { ç.l.connect(s) }
    public static prefix func  ~ (_ s: Array<Statement>) -> Statement { ç.ø.connect(s) }
    public static prefix func  * (_ s: Array<Statement>) -> Statement { ç.x.connect(s) }
    public static prefix func && (_ s: Array<Statement>) -> Statement { ç.c.connect(s) }
    public static prefix func || (_ s: Array<Statement>) -> Statement { ç.d.connect(s) }
}

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






public extension Array where Element == Judgement {
    func removeDuplicates() -> [Judgement] {
        let unique = Dictionary(grouping: self) {
            $0.identifier
        }.values.compactMap {
            $0.max { j1, j2 in
                let j = choice(j1: j1, j2: j2)
                return j.statement == j2.statement
            }
        }
        return unique
    }
}

extension Judgement {
    public init(_ statement: Statement, _ truthValue: TruthValue, _ derivationPath: [String] = [], tense: Tense? = nil, timestamp: UInt32 = 0) {
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
    
    public func evidenceOverlap(_ j2: Judgement) -> Bool {
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

extension Copula {
    var atemporal: Copula {
        switch self {
        case .predictiveImp: fallthrough
        case .retrospectiveImp: fallthrough
        case .concurrentImp: return .implication
        case .predictiveEq: fallthrough
        case .retrospectiveEq: fallthrough
        case .concurrentEq: return .equivalence
        default: return self
        }
    }
    var concurrent: Copula {
        switch self {
        case .implication: return .concurrentImp
        case .equivalence: return .concurrentEq
        default: return self
        }
    }
    var predictive: Copula {
        switch self {
        case .implication: return .predictiveImp
        case .equivalence: return .predictiveEq
        default: return self
        }
    }
    var retrospective: Copula {
        switch self {
        case .implication: return .retrospectiveImp
        case .equivalence: return .retrospectiveEq
        default: return self
        }
    }
    var isConcurrent: Bool {
        self == .concurrentEq || self == .concurrentImp
    }
    var isPredictive: Bool {
        self == .predictiveEq || self == .predictiveImp
    }
    var isRetrospective: Bool {
        self == .retrospectiveImp || self == .retrospectiveEq
    }
}

public typealias ç = Connector /// shorthand

extension Connector {
    var term: Term { Term.symbol(rawValue) }
    
    public static func e_(_ r: Term, _ t1: Term, _ t2: Term) -> Term { connect(.compound(.x, [r]), .e, (t1 * t2)) }
    public static func i_(_ r: Term, _ t1: Term, _ t2: Term) -> Term { connect(.compound(.x, [r]), .i, (t1 * t2)) }

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



extension Sequence where Element == Term {
    func toList() -> List {
        var list: List = .empty
        for term in self.reversed() {
            list = List.cons(term.logic(), list)
        }
        return list
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
            return List.cons(LogicValue(c.atemporal), List.cons(s.logic(), List.cons(p.logic(), List.empty)))
            
        case .variable:
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
//
//  LogicKit.swift
//  LogicKit
//
//  Created by Dimitri Racordon on 07.02.17.
//  Copyright © 2017 University of Geneva. All rights reserved.
//
//  https://github.com/kyouko-taiga/SwiftKanren

protocol LogicTerm {

    // We can't make the LogicTerm conform to Equatable, as we need to use within
    // heterogeneous collections. Hence we can't have a safe requirements
    // (see WWDC 2015 - session 408). Similarly, we can't require conforming
    // types to implement the global equality operator (==), as the various
    // overloads would become ambiguous without a self requirement.
    func equals(_ other: LogicTerm) -> Bool

}

extension LogicTerm where Self: Equatable {

    func equals(_ other: LogicTerm) -> Bool {
        if other is Self {
            return (other as! Self) == self
        }

        return false
    }

}



struct LogicVariable: LogicTerm {

    let name: String

    init(named name: String) {
        self.name = name
    }

}

extension LogicVariable: Hashable {

    func hash(into hasher: inout Hasher) {
        hasher.combine(name)
    }

    static func == (left: LogicVariable, right: LogicVariable) -> Bool {
        return left.name == right.name
    }

}

extension LogicVariable: CustomStringConvertible {

    var description: String {
        return self.name
    }

}


class LogicVariableFactory {

    fileprivate var state: State
    private var LogicVariables = [String: LogicVariable]()

    fileprivate init(_ state: State) {
        self.state = state
    }

    subscript(name: String) -> LogicVariable {
        if let LogicVariable = self.LogicVariables[name] {
            return LogicVariable
        }

        self.LogicVariables[name] = LogicVariable(named: self.state.nextUnusedName)
        self.state = self.state.withNextNewName()
        return self.LogicVariables[name]!
    }

}


struct LogicValue<T: Equatable>: LogicTerm {

    fileprivate let wrapped: T

    init(_ val: T) {
        self.wrapped = val
    }

    func equals(_ other: LogicTerm) -> Bool {
        if let rhs = (other as? LogicValue<T>) {
            return rhs.wrapped == self.wrapped
        }

        return false
    }

    func extract() -> T {
        return wrapped
    }
}

extension LogicValue: Equatable {

    static func ==(lhs: LogicValue, rhs: LogicValue) -> Bool {
        return lhs.wrapped == rhs.wrapped
    }

}

extension LogicValue: CustomStringConvertible {

    var description: String {
        return String(describing: self.wrapped)
    }

}


struct Unassigned: LogicTerm, CustomStringConvertible {

    private static var LogicVariables = [LogicVariable: Int]()
    private static let unicodeSubscripts = [
        "\u{2080}", "\u{2081}", "\u{2082}", "\u{2083}", "\u{2084}",
        "\u{2085}", "\u{2086}", "\u{2087}", "\u{2088}", "\u{2089}"]

    private var id: Int

    fileprivate init(_ LogicVariable: LogicVariable) {
        if Unassigned.LogicVariables[LogicVariable] == nil {
            Unassigned.LogicVariables[LogicVariable] = Unassigned.LogicVariables.count
        }
        self.id = Unassigned.LogicVariables[LogicVariable]!
    }

    func equals(_ other: LogicTerm) -> Bool {
        return false
    }

    var description: String {
        var suffix = ""
        if self.id == 0 {
            suffix = Unassigned.unicodeSubscripts[0]
        } else {
            var number = self.id
            while number > 0 {
                suffix = Unassigned.unicodeSubscripts[number % 10] + suffix
                number /= 10
            }
        }

        return "_" + suffix
    }

}


enum List: LogicTerm {

    case empty, cons(LogicTerm, LogicTerm)

    func equals(_ other: LogicTerm) -> Bool {
        guard let rhs = other as? List else {
            return false
        }

        switch (self, rhs) {
        case (.empty, .empty):
            return true
        case (.cons(let lh, let lt), .cons(let rh, let rt)):
            return lh.equals(rh) && lt.equals(rt)
        default:
            return false
        }
    }

}


struct Map: LogicTerm {

    typealias StorageType = [String: LogicTerm]

    fileprivate let storage: StorageType

    init() {
        self.storage = [:]
    }

    init<S: Sequence>(_ items: S) where S.Iterator.Element == (key: String, value: LogicTerm) {
        var storage = StorageType()
        for (key, value) in items {
            storage[key] = value
        }
        self.storage = storage
    }

    var keys: Dictionary<String, LogicTerm>.Keys {//LazyMapCollection<StorageType, String> {
        return self.storage.keys
    }

    var values: Dictionary<String, LogicTerm>.Values {//LazyMapCollection<StorageType, LogicTerm> {
        return self.storage.values
    }

    subscript(key: String) -> LogicTerm? {
        return self.storage[key]
    }

    func with(key: String, value: LogicTerm) -> Map {
        var newStorage = self.storage
        newStorage[key] = value
        return Map(newStorage)
    }

}

extension Map: Equatable {

    static func == (left: Map, right: Map) -> Bool {
        let leftKeys = left.storage.keys.sorted()
        let rightKeys = right.storage.keys.sorted()

        guard leftKeys == rightKeys else {
            return false
        }

        for (leftKey, rightKey) in zip(leftKeys, rightKeys) {
            guard left.storage[leftKey]!.equals(right.storage[rightKey]!) else {
                return false
            }
        }

        return true
    }

}

extension Map: Sequence {

    func makeIterator() -> StorageType.Iterator {
        return self.storage.makeIterator()
    }

}

extension Map: Collection {

    var startIndex: StorageType.Index {
        return self.storage.startIndex
    }

    var endIndex: StorageType.Index {
        return self.storage.endIndex
    }

    func index(after: StorageType.Index) -> StorageType.Index {
        return self.storage.index(after: after)
    }

    subscript(index: StorageType.Index) -> StorageType.Element {
        return self.storage[index]
    }

}

extension Map: ExpressibleByDictionaryLiteral {

    init(dictionaryLiteral elements: (String, LogicTerm)...) {
        self.init(elements.map { (key: $0.0, value: $0.1) })
    }

}

extension Map: CustomStringConvertible {

    var description: String {
        return String(describing: self.storage)
    }

}


struct Substitution {

    init() {
        
    }
    
    fileprivate var storage = [LogicVariable: LogicTerm]()

    typealias Association = (LogicVariable: LogicVariable, LogicTerm: LogicTerm)

    subscript(_ key: LogicTerm) -> LogicTerm {
        // If the the given key isn't a LogicVariable, we can just give it back.
        guard let k = key as? LogicVariable else {
            return key
        }

        if let rhs = self.storage[k] {
            // Continue walking in case the rhs is another LogicVariable.
            return self[rhs]
        }

        // We give back the LogicVariable if is not associated.
        return key
    }

    func extended(with association: Association) -> Substitution {

        // NOTE: William Byrd's PhD thesis doesn't specify what is the
        // expected behaviour when extending a substitution map with an
        // already existing key.

        // TODO: Check for introduced circularity.

        var result = self
        result.storage[association.LogicVariable] = association.LogicTerm
        return result
    }

    func unifying(_ u: LogicTerm, _ v: LogicTerm) -> Substitution? {
        let walkedU = self[u]
        let walkedV = self[v]

        // LogicTerms that walk to equal values always unify, but add nothing to
        // the substitution.
        if walkedU.equals(walkedV) {
            return self
        }

        // Unifying a logic LogicVariable with some other LogicTerm creates a new entry
        // in the substitution.
        if walkedU is LogicVariable {
            return self.extended(with: (LogicVariable: walkedU as! LogicVariable, LogicTerm: walkedV))
        } else if walkedV is LogicVariable {
            return self.extended(with: (LogicVariable: walkedV as! LogicVariable, LogicTerm: walkedU))
        }

        // If the walked values of u and of v are lists, then unifying them
        // boils down to unifying their elements.
        if (walkedU is List) && (walkedV is List) {
            return self.unifyingLists(walkedU as! List, walkedV as! List)
        }

        // If the walked values of u and of v are maps, then unifying them
        // boils down to unifying their elements.
        if (walkedU is Map) && (walkedV is Map) {
            return self.unifyingMaps(walkedU as! Map, walkedV as! Map)
        }

        return nil
    }

    private func unifyingLists(_ u: List, _ v: List) -> Substitution? {
        switch (u, v) {
        case (.empty, .empty):
            // Empty lists always unify, but add nothing to the substitution.
            return self

        case (.cons(let uh, let ut), .cons(let vh, let vt)):
            // Unifying non-empty lists boils down to unifying their head,
            // before recursively unifying their tails.
            return self.unifying(uh, vh)?.unifying(ut, vt)

        default:
            // Unifying a non-empty list with an empty list always fail.
            return nil
        }
    }

    private func unifyingMaps(_ u: Map, _ v: Map) -> Substitution? {
        let leftKeys = u.keys.sorted()
        let rightKeys = v.keys.sorted()

        // Unifying dictionaries with different keys always fail.
        guard leftKeys == rightKeys else {
            return nil
        }

        // Unifying dictionaires boils down to unifying the values associated,
        // with each of their respective keys.
        var result: Substitution? = self
        for (leftKey, rightKey) in zip(leftKeys, rightKeys) {
            result = result?.unifying(u[leftKey]!, v[rightKey]!)
        }
        return result
    }

    func reified() -> Substitution {
        var result = Substitution()
        for LogicVariable in self.storage.keys {
            let walked = self.deepWalk(LogicVariable)
            if let v = walked as? LogicVariable {
                result = result.extended(with: (LogicVariable: LogicVariable, LogicTerm: Unassigned(v)))
            } else {
                result = result.extended(with: (LogicVariable: LogicVariable, LogicTerm: walked))
            }
        }
        return result
    }

    private func deepWalk(_ value: LogicTerm) -> LogicTerm {
        // If the given value is a list, we have to "deep" walk its elements.
        if let l = value as? List {
            switch l {
            case .empty:
                return l
            case .cons(let h, let t):
                return List.cons(self.deepWalk(h), self.deepWalk(t))
            }
        }

        // If the given value is a map, we have to "deep" walk its values.
        if let m = value as? Map {
            var reifiedMap = Map()
            for item in m {
                reifiedMap = reifiedMap.with(key: item.key, value: self.deepWalk(item.value))
            }
            return reifiedMap
        }

        // If the the given value isn't a LogicVariable, we can just give it back.
        guard let key = value as? LogicVariable else {
            return value
        }

        if let rhs = self.storage[key] {
            // Continue walking in case the rhs is another LogicVariable.
            return self.deepWalk(rhs)
        }

        // We give back the LogicVariable if is not associated.
        return value
    }

}

extension Substitution: Sequence {

    func makeIterator() -> AnyIterator<Association> {
        var it = self.storage.makeIterator()

        return AnyIterator {
            if let (LogicVariable, LogicTerm) = it.next() {
                return (LogicVariable: LogicVariable, LogicTerm: self[LogicTerm])
            }

            return nil
        }
    }

}


/// A struct containing a substitution and the name of the next unused logic
/// LogicVariable.
struct State {

    fileprivate let substitution: Substitution
    fileprivate var nextUnusedName: String {
        return "$" + String(describing: self.nextId)
    }

    private let nextId: Int

    init(substitution: Substitution = Substitution(), nextId: Int = 0) {
        self.substitution = substitution
        self.nextId = nextId
    }

    fileprivate func with(newSubstitution: Substitution) -> State {
        return State(substitution: newSubstitution, nextId: self.nextId)
    }

    fileprivate func withNextNewName() -> State {
        return State(substitution: self.substitution, nextId: self.nextId + 1)
    }

}


enum Stream {

    case empty
    indirect case mature(head: State, next: Stream)
    case immature(thunk: () -> Stream)

    // mplus
    fileprivate func merge(_ other: Stream) -> Stream {
        switch self {
        case .empty:
            return other

        case .mature(head: let state, next: let next):
            return .mature(head: state, next: next.merge(other))

        case .immature(thunk: let thunk):
            return .immature {
                return other.merge(thunk())
            }
        }
    }

    // bind
    fileprivate func map(_ LogicGoal: @escaping LogicGoal) -> Stream {
        switch self {
        case .empty:
            return .empty

        case .mature(head: let head, next: let next):
            return LogicGoal(head).merge(next.map(LogicGoal))

        case .immature(thunk: let thunk):
            return .immature {
                return thunk().map(LogicGoal)
            }
        }
    }

    // pull
    fileprivate func realize() -> Stream {
        switch self {
        case .empty:
            return .empty

        case .mature(head: _, next: _):
            return self

        case .immature(thunk: let thunk):
            return thunk().realize()
        }
    }

}

extension Stream: Sequence {

    func makeIterator() -> AnyIterator<Substitution> {
        var it = self

        return AnyIterator {

            // Realize the iterated stream here, so that we its state is
            // computed as lazily as possible (i.e. when the iterator's next()
            // method is called).

            switch it.realize() {
            case .empty:
                // Return nothing for empty stream, ending the sequence.
                return nil

            case .mature(head: let state, next: let successor):
                // Return the realized substitution and advance the iterator.
                it = successor
                return state.substitution

            case .immature(thunk: _):
                assertionFailure("realize shouldn't produce immature streams")
            }

            return nil
        }
    }

}


/// Represents a function that encapsulates a logic program and which, given a
/// state, returns a stream of states for each way the program can succeed.
typealias LogicGoal = (State) -> Stream


infix operator ≡   : ComparisonPrecedence
infix operator === : ComparisonPrecedence

/// Creates a LogicGoal that unify two LogicTerms.
///
/// The LogicGoal takes an existing state and returns (as a lazy stream) either a
/// state with bindings for the LogicVariables in u and v (using unification), or
/// nothing at all if u and v cannot be unified.
func ≡ (u: LogicTerm, v: LogicTerm) -> LogicGoal {
    return { state in
        if let s = state.substitution.unifying(u, v) {
            return .mature(head: state.with(newSubstitution: s), next: .empty)
        }

        return .empty
    }
}

/// Alternative for ≡(_:_:)
func === (u: LogicTerm, v: LogicTerm) -> LogicGoal {
    return u ≡ v
}


/// Takes a LogicGoal constructor and returns a LogicGoal with fresh LogicVariables.
///
/// This function takes a *LogicGoal constructor* (i.e. a function), which accepts
/// a single LogicVariable as parameter, and returns a new LogicGoal for which the
/// LogicVariable is fresh.
func fresh(_ constructor: @escaping (LogicVariable) -> LogicGoal) -> LogicGoal {
    return { state in
        constructor(LogicVariable(named: state.nextUnusedName))(state.withNextNewName())
    }
}


/// Takes a LogicGoal constructor and returns a LogicGoal with fresh LogicVariables.
///
/// This function takes a *LogicGoal constructor* (i.e. a function), which accepts
/// a LogicVariable factory as parameter, and returns a new LogicGoal for which all the
/// LogicVariables generated by the factory are fresh.
func freshn(_ constructor: @escaping (LogicVariableFactory) -> LogicGoal) -> LogicGoal {
    return { state in
        let factory = LogicVariableFactory(state)
        return constructor(factory)(factory.state)
    }
}


/// Constructs a disjunction of LogicGoals.
func || (left: @escaping LogicGoal, right: @escaping LogicGoal) -> LogicGoal {
    return { state in
        left(state).merge(right(state))
    }
}


/// Constructs a conjunction of LogicGoals.
func && (left: @escaping LogicGoal, right: @escaping LogicGoal) -> LogicGoal {
    return { state in
        left(state).map(right)
    }
}


/// Takes a LogicGoal constructor and returns a LogicGoal with substitution.
///
/// This function takes a *LogicGoal constructor* (i.e. a function), which accepts
/// a substitution as parameter, and returns a new LogicGoal.
func inEnvironment (_ constructor: @escaping (Substitution) -> LogicGoal) -> LogicGoal {
    return { state in
        let reified = state.substitution.reified()
        return constructor(reified)(state)
    }
}


/// Takes a LogicGoal and returns a thunk that wraps it.
func delayed(_ LogicGoal: @escaping LogicGoal) -> LogicGoal {
    return { state in
        .immature { LogicGoal(state) }
    }
}


/// Executes a logic program (i.e. a LogicGoal) with an optional initial state.
func solve(withInitialState state: State? = nil, _ program: LogicGoal) -> Stream {
    return program(state ?? State())
}


/// A LogicGoal that always succeeds.
let success = (LogicValue(true) === LogicValue(true))


/// A LogicGoal that always fails.
let failure = (LogicValue(false) === LogicValue(true))


/// Creates a LogicGoal that tests if a LogicTerm is an instance of a `Value<T>`
/// in the current substitution.
func isValue<T : Equatable>(_ LogicTerm: LogicTerm, _ type: T.Type) -> LogicGoal {
    return inEnvironment { substitution in
        if substitution [LogicTerm] is LogicValue<T> {
          return success
        } else {
          return failure
        }
    }
}


/// Creates a LogicGoal that tests if a LogicTerm is an instance of a `LogicVariable`
/// in the current substitution.
func isLogicVariable(_ LogicTerm: LogicTerm) -> LogicGoal {
    return inEnvironment { substitution in
        if substitution [LogicTerm] is LogicVariable {
          return success
        } else {
          return failure
        }
    }
}


/// Creates a LogicGoal that tests if a LogicTerm is an instance of a `List`
/// in the current substitution.
func isList(_ LogicTerm: LogicTerm) -> LogicGoal {
    return inEnvironment { substitution in
        if substitution [LogicTerm] is List {
          return success
        } else {
          return failure
        }
    }
}


/// Creates a LogicGoal that tests if a LogicTerm is an instance of a `Map`
/// in the current substitution.
func isMap(_ LogicTerm: LogicTerm) -> LogicGoal {
    return inEnvironment { substitution in
        if substitution [LogicTerm] is Map {
          return success
        } else {
          return failure
        }
    }
}

extension Term {
    public var isTautology: Bool {
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

extension Term {
    public static let º = Term.symbol("º") // image placeholder
    public static let NULL = Term.symbol("NULL")
    public static let SELF = Term.symbol("SELF")
    
    public static func word(_ w: String) -> Term { .symbol(w) }
    public static func `var`(_ s: String) -> Term { .variable(.independent(s)) }
    public static func instance(_ t: Term) -> Term { .compound(ç.extSet, [t]) }
    public static func property(_ t: Term) -> Term { .compound(ç.intSet, [t]) }
    
    public var terms: [Term] {
        switch self {
        case .symbol:
            return [self]
        case .compound(let c, let terms):
            if terms.count == 1, c == .intSet || c == .extSet {
                return [self]
            }
            return terms
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
    
    public static func getTerms(_ t: Term) -> [Term] {
        if t.terms.count == 1 {
            return t.terms
        }
        return t.terms.flatMap { getTerms($0) }
    }
}


// MARK: Replace

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

// MARK: ExpressibleByStringLiteral

extension Term: ExpressibleByStringLiteral {
    /// handles simple cases for use in testing and playgrounds
    public init(stringLiteral value: String) {
        self = {
            if value.first == "{" {
                return .instance(.init(stringLiteral: value.word))
            }

            if value.first == "[" {
                return .property(.init(stringLiteral: value.word))
            }

            if value.first == "?" {
                let word = value.dropFirst()
                let name = (word.count == 0) ? nil : String(word)
                return .variable(.query(name))
            }

            let words = value.words

            if words.count == 1 {
                return .symbol(words[0])
            }

            return .NULL
        }()
    }
}


// MARK: Replacements for Foundation methods

extension String {
    var word: String {
        var word: String = ""
        for c in self {
            if !["{", "}", "[", "]"].contains(c) {
                word.append(c)
            }
        }
        return word
    }
    
    var words: [String] {
        var words: [String] = []
        var word: String = ""
        for c in self {
            if c == " " {
                if !word.isEmpty {
                    words.append(word)
                    word = ""
                }
            } else {
                word.append(c)
            }
        }
        if !word.isEmpty {
            words.append(word)
        }
        return words
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

extension Variable {
    public init?(_ string: String) {
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
*/
