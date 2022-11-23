
public func debugPrint(_ item: Any, _ separator: String = "-------") {
    print("\n"+separator+"\(type(of: item))"+separator+"\n")
    print("\(item)"+separator+"\n")
}

extension Question {
    public init(_ f: @autoclosure () -> Statement) {
        statement = f()
        type = .truth
        tense = nil
    }
    public var variableTerm: Term! {
        if case .statement(let s, _ , let p) = statement {
            if case .variable = s {
                return s
            } else if case .variable = p {
                return p
            } else {
                return nil
            }
        }
        return nil
    }
}

extension Goal {
    public init(_ f: @autoclosure () -> Statement) {
        statement = f()
        desireValue = DesireValue(1.0, 0.9) // TODO: what is the correct default?
    }
}

extension Sentence {
    public init(_ q: Question) {
        self = .question(q)
    }
    public init(_ j: Judgement) {
        self = .judgement(j)
    }
    public init(_ g: Goal) {
        self = .goal(g)
    }
}

extension TermLink {
    public init(_ term: Term, _ priority: Double) {
        self.term = term
        self.priority = priority
    }
}

extension Belief {
    public init(_ judgement: Judgement, _ priority: Double) {
        self.judgement = judgement
        self.priority = priority
    }
}

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
public func -*(_ s: Statement, _ fct: (Double, Double, UInt64)) -> Sentence {
    Sentence(s -* fct)
}
public func -*(_ s: Statement, _ f: Double) -> Sentence {
    Sentence(s -* (f, 0.9))
}
public func -*(_ s: Statement, _ t: UInt64) -> Sentence {
    Sentence(s -* (1, 0.9, t))
}

postfix operator -*
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
            return .judgement(Judgement(j.statement, j.truthValue, j.derivationPath, tense: tense, timestamp: j.timestamp))
        case .question(let q):
            return .question(Question(statement: q.statement, type: q.type, tense: tense))
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


// MARK: CustomStringConvertible

extension Concept: CustomStringConvertible {
    public var description: String {
        "\(term)".uppercased() + "\n.  \(termLinks)" + ".  \(beliefs)"
    }
}

extension TermLink: CustomStringConvertible {
    public var description: String {
        identifier + " \(priority)"
    }
}

extension Belief: CustomStringConvertible {
    public var description: String {
        "\(judgement)"
    }
}

extension Sentence: CustomStringConvertible {
    public var description: String {
        switch self {
        case .judgement(let judgement):
            return "\(judgement)"
        case .goal(let goal):
            return "\(goal)"
        case .question(let question):
            return "\(question)"
        case .pause(let t):
            return "💤 \(Double(t)/1000) seconds"
        case .cycle(let n):
            return "🔄 \(Double(n * Sentence.defaultPause)/1000) seconds"
        }
    }
}

extension Bag: CustomStringConvertible {
    public var description: String {
        let x = I.self == Concept.self ? "" : ".  "
        let o = items.values.reduce("", { $0 + "\($1)\n" + x })
        return String(o.dropLast(x.count))
    }
}

extension WrappedBag: CustomStringConvertible {
    public var description: String {
        let b = "\(bag)"
        let w = wrapped == nil ? "" : "\(wrapped!)"
        return b + "\n---\n" + w
    }
}

