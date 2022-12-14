import NAL

// convenience initializer for Belief
public func +(_ j: Judgement, p: Double) -> Belief {
    Belief(j, p)
}


public func -*(_ s: Statement, _ fc: (Double, Double)) -> Judgement {
    s -* (fc.0, fc.1, ETERNAL)
}
public func -*(_ s: Statement, _ fc: (Double, Double)) -> Sentence {
    Sentence(s -* fc)
}
public func -*(_ s: Statement, _ fct: (Double, Double, UInt32)) -> Sentence {
    Sentence(s -* fct)
}
public func -*(_ s: Statement, _ f: Double) -> Sentence {
    Sentence(s -* (f, 0.9))
}
public func -*(_ s: Statement, _ t: UInt32) -> Sentence {
    Sentence(s -* (1, 0.9, t))
}

extension Statement {
    public static postfix func -*(_ s: Statement) -> Sentence {
        Sentence(s-*)
    }
}


extension Sentence {
    public static prefix func <<(_ s: Sentence) -> Sentence {
        s.addTense(.past)
    }
    public static prefix func ||(_ s: Sentence) -> Sentence {
        s.addTense(.present)
    }
    public static prefix func >>(_ s: Sentence) -> Sentence {
        s.addTense(.future)
    }
    
    private func addTense(_ tense: Tense) -> Sentence {
        switch self {
        case .judgement(let j):
            return .judgement(Judgement(j.statement, j.truthValue, j.derivationPath,
                                        tense: tense,
                                        timestamp: j.timestamp == ETERNAL ? 0 : j.timestamp))
        case .question(let q):
            return .question(Question(q.statement, q.type, tense))
        default:
            return self
        }
    }
}

postfix operator -?
extension Statement {
    public static postfix func -?(_ s: Statement) -> Question { Question(s) }
    public static postfix func -?(_ s: Statement) -> Sentence { Sentence(s-?) }
}

postfix operator -!
extension Statement {
    public static postfix func -!(_ s: Statement) -> Goal { Goal(s) }
    public static postfix func -!(_ s: Statement) -> Sentence { Sentence(s-!) }
}
