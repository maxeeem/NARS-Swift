
extension Question {
    public init(_ f: @autoclosure () -> Statement) {
        self.init(f(), Quest.truth, nil)
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
        let desireValue = DesireValue(1.0, 0.9) // TODO: what is the correct default?
        self.init(f(), desireValue)
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

// MARK: Equatable

extension Bag: Equatable {
    public static func == (lhs: Bag<I>, rhs: Bag<I>) -> Bool {
        Set(lhs.items.keys) == Set(rhs.items.keys)
    }
}

extension WrappedBag: Equatable {
    public static func == (lhs: WrappedBag<I>, rhs: WrappedBag<I>) -> Bool {
        lhs.bag == rhs.bag && lhs.wrapped == rhs.wrapped
    }
}

extension Concept: Equatable {
    public static func == (lhs: Concept, rhs: Concept) -> Bool {
        lhs.term == rhs.term
        && lhs.termLinks == rhs.termLinks
        && lhs.beliefs == rhs.beliefs
    }
}

extension NARS: Equatable {
    public static func == (lhs: NARS, rhs: NARS) -> Bool {
        lhs.name == rhs.name
    }
}


/// Convenience

extension WrappedBag where I == Belief {
    /// convenience for iterating over both dictionaries
    var items: [String : I] { bag.items.merging(wrapped?.items ?? [:], uniquingKeysWith: max)}
}

extension Belief: Comparable {
    public static func < (lhs: Belief, rhs: Belief) -> Bool {
        let c = choice(j1: lhs.judgement, j2: rhs.judgement)
        return c.statement == rhs.judgement.statement
    }
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

