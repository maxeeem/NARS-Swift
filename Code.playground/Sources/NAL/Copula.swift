
public enum Copula: String, CaseIterable, Codable {
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
