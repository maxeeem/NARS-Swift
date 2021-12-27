
public enum Copula: String {
    /// Primary
    case inheritance = "->"  // 1
    case similarity  = "<–>" // 2
    case implication = "=>"  // 5
    case equivalence = "<=>" // 5
    /// Secondary
    case instance          = "•–>"  // 2
    case property          = "–>•"  // 2
    case insProp           = "•->•" // 2
    /// Temporal
    case predictiveImp     = "/=>"  // 7
    case retrospectiveImp  = "\\=>" // 7 - note: second slash is bc escape char in Swift
    case concurrentImp     = "|=>"  // 7
    case predictiveEq      = "/<=>" // 7
    case concurrentEq      = "|<=>" // 7
}

precedencegroup Copula { // priority
    higherThan: ComparisonPrecedence
}

infix operator --> : Copula // "->"
infix operator <-> : Copula
infix operator =>  : Copula
infix operator <=> : Copula

infix operator •->  : Copula
infix operator ->•  : Copula
infix operator •->• : Copula

infix operator >>|=>  : Copula //  "/=>"  future
infix operator <<|=>  : Copula //  "\=>"   past
infix operator   |=>  : Copula //  "|=>"  present
infix operator >>|<=> : Copula //  "/<=>"
infix operator   |<=> : Copula //  "|<=>"

// NAL-1
public func --> (_ s: Term, p: Term) -> Statement { Statement(s, .inheritance, p) }
// NAL-2
public func <->  (_ s: Term, p: Term) -> Statement { Statement(s, .similarity , p) }
public func •->  (_ s: Term, p: Term) -> Statement { Statement(s, .instance   , p) }
public func ->•  (_ s: Term, p: Term) -> Statement { Statement(s, .property   , p) }
public func •->• (_ s: Term, p: Term) -> Statement { Statement(s, .insProp    , p) }


// Convenience overrides
public func --> (_ s: String, p: Term  ) -> Statement { Term.word(s) --> p            }
public func --> (_ s: Term,   p: String) -> Statement { s            --> Term.word(p) }
public func --> (_ s: String, p: String) -> Statement { Term.word(s) --> Term.word(p) }

public func <-> (_ s: String, p: Term  ) -> Statement { Term.word(s) <-> p            }
public func <-> (_ s: Term,   p: String) -> Statement { s            <-> Term.word(p) }
public func <-> (_ s: String, p: String) -> Statement { Term.word(s) <-> Term.word(p) }

public func •-> (_ s: String, p: Term  ) -> Statement { Term.word(s) •-> p            }
public func •-> (_ s: Term,   p: String) -> Statement { s            •-> Term.word(p) }
public func •-> (_ s: String, p: String) -> Statement { Term.word(s) •-> Term.word(p) }

public func ->• (_ s: String, p: Term  ) -> Statement { Term.word(s) ->• p            }
public func ->• (_ s: Term,   p: String) -> Statement { s            ->• Term.word(p) }
public func ->• (_ s: String, p: String) -> Statement { Term.word(s) ->• Term.word(p) }

public func •->• (_ s: String, p: Term  ) -> Statement { Term.word(s) •->• p            }
public func •->• (_ s: Term,   p: String) -> Statement { s            •->• Term.word(p) }
public func •->• (_ s: String, p: String) -> Statement { Term.word(s) •->• Term.word(p) }

