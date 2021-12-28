
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
    case concurrentEq      =  "|<=>"     // 7
}

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
infix operator   |<=> : Copula //  "|<=>"

// NAL-1
public func -->  (_ s: Term, p: Term) -> Statement { Statement( s    , .inheritance ,    p ) }
// NAL-2
public func <->  (_ s: Term, p: Term) -> Statement { Statement( s    , .similarity  ,    p ) } 
public func •->  (_ s: Term, p: Term) -> Statement { Statement( s•-> , .inheritance ,    p ) }
public func ->•  (_ s: Term, p: Term) -> Statement { Statement( s    , .inheritance , ->•p ) }
public func •->• (_ s: Term, p: Term) -> Statement { Statement( s•-> , .inheritance , ->•p ) }


// Convenience overrides
public func -->  (_ s: String, p: Term  ) -> Statement { s• -->  p }
public func -->  (_ s: Term,   p: String) -> Statement { s  --> •p }
public func -->  (_ s: String, p: String) -> Statement { s• --> •p }

public func <->  (_ s: String, p: Term  ) -> Statement { s• <->  p }
public func <->  (_ s: Term,   p: String) -> Statement { s  <-> •p }
public func <->  (_ s: String, p: String) -> Statement { s• <-> •p }

public func •->  (_ s: String, p: Term  ) -> Statement { s• •->  p }
public func •->  (_ s: Term,   p: String) -> Statement { s  •-> •p }
public func •->  (_ s: String, p: String) -> Statement { s• •-> •p }

public func  ->• (_ s: String, p: Term  ) -> Statement { s• ->•  p }
public func  ->• (_ s: Term,   p: String) -> Statement { s  ->• •p }
public func  ->• (_ s: String, p: String) -> Statement { s• ->• •p }

public func •->• (_ s: String, p: Term  ) -> Statement { s• •->•  p }
public func •->• (_ s: Term,   p: String) -> Statement { s  •->• •p }
public func •->• (_ s: String, p: String) -> Statement { s• •->• •p }

