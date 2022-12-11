
// convenience initializer for Judgement
public func + (_ s: Statement, fc: (Double, Double, UInt64)) -> Judgement {
    Judgement(s, TruthValue(fc.0, fc.1), timestamp: fc.2)
}

extension Statement {
    public static postfix func -* (_ s: Statement) -> Judgement {
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
public func -* (_ s: Statement, _ fc: (Double, Double, UInt64)) -> Judgement {
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
