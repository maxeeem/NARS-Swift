
extension Question {
    public init(_ f: @autoclosure () -> Statement, _ s: Judgement? = nil) {
        self.init(f(), Quest.truth, nil, s)
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

extension Concept: Equatable {
    public static func == (lhs: Concept, rhs: Concept) -> Bool {
        lhs.term == rhs.term
        && lhs.beliefs == rhs.beliefs
    }
}

extension NARS: Equatable {
    public static func == (lhs: NARS, rhs: NARS) -> Bool {
        lhs.name == rhs.name
    }
}


/// Convenience

extension Belief: Comparable {
    public static func < (lhs: Belief, rhs: Belief) -> Bool {
        rhs.judgement == choice(j1: lhs.judgement, j2: rhs.judgement)
    }
}

// MARK: CustomStringConvertible

extension Concept: CustomStringConvertible {
    public var description: String {
        "\(term)".uppercased() + ".  \(tasks) -- \(beliefs)"
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
        case .cycle(let n):
            return "🔄 \(n) cycles"
        }
    }
}

extension Bag: CustomStringConvertible {
    public var description: String {
        let x = I.self == Concept.self ? "" : ".  "
        let o = items.values.reduce("\n\(x)", { $0 + "\($1)\n" + x })
        return String(o.dropLast(x.count))
    }
}


/// Utility

extension Array where Element == Sentence {
    mutating func enqueue(_ js: [Judgement]) {
        let js: [Sentence] = js.reversed().map({.judgement($0)})
        insert(contentsOf: js, at: 0)
    }
    mutating func cleanup(_ statement: Statement) {
        let idx = firstIndex(where: { s in
            if case .question(let q) = s {
                if q.statement == statement {
                    return true
                }
            }
            if case .goal(let g) = s {
                if g.statement == statement {
                    return true
                }
            }
            return false
        })
        if let i = idx {
            self = Array(prefix(upTo: i))
        }
    }
}

extension Array where Element == Judgement {
    func remove(matching sentence: Sentence, keepOrder: Bool = true) -> [Judgement] {
        //TODO: use choice to additionally resolve duplicates
        
        let filtered = filter { j in
            if j.truthValue.confidence == 0 {
                return false
            }
            if j.statement.isTautology {
                return false
            }
            if case .judgement(let judgement) = sentence,
                j == judgement || judgement.statement.isTautology {
                return false
            }
//            if case .question(let question) = sentence, question.statement.isTautology {
//                return false
//            }
//            if case .goal(let goal) = sentence, goal.statement.isTautology {
//                return false
//            }
            return true
        }
        if keepOrder {
            return filtered
        } else {
            return Array(Set(filtered)) // TODO: not a good way to do this
        }
    }
}
