
public func debugPrint(_ item: Any, _ separator: String = "-------") {
    print("\n"+separator+"\(type(of: item))"+separator+"\n")
    print("\(item)"+separator+"\n")
}

// TODO: remove all Copying conformances
// replace with a wrapper that is able to create
// a shallow clone of memory and pull in new things
// on demand via some sort of a wrapper interface

extension Bag: Copying where I: Copying {
    public func copy() -> Bag<I> {
        queue.sync { // TODO: create a wrapper instead
            let bag = Bag<I>(levels, capacity)
            bag.items = items.mapValues { $0.copy() }
            bag.buckets = buckets.compactMap { $0.map { $0.copy() } }
            bag.currentLevel = currentLevel // not needed?
            return bag
        }
    }
}

extension Concept: Copying {
    public func copy() -> Concept {
        var concept = Concept(priority: priority, term: term)
        concept.termLinks = termLinks.copy()
        concept.beliefs = beliefs.copy()
        return concept
    }
}

extension TermLink: Copying {
    public func copy() -> TermLink { self } // value type 
}
extension Belief: Copying {
    public func copy() -> Belief { self } // value type
}
extension Task: Copying {
    public func copy() -> Task { self } // value type
}

extension Question {
    public init(_ f: @autoclosure () -> Statement) {
        statement = f()
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

extension Sentence {
    public init(_ q: Question) {
        self = .question(q)
    }
    public init(_ j: Judgement) {
        self = .judgement(j)
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


infix operator -* : Copula
public func -*(_ s: Statement, _ tv: (Double, Double)) -> Sentence {
    Sentence(s -* tv)
}
public func -*(_ s: Statement, _ f: Double) -> Sentence {
    Sentence(s -* (f, 0.9))
}
public func -*(_ j: inout Judgement, _ tv: (Double, Double)) {
    j = j.statement -* tv
}

postfix operator -*
extension Statement {
    public static postfix func -*(_ s: Statement) -> Sentence {
        Sentence(s-*)
    }
}

postfix operator -?
extension Statement {
    public static postfix func -?(_ s: Statement) -> Question { Question(s) }
    public static postfix func -?(_ s: Statement) -> Sentence { Sentence(s-?) }
}


//postfix operator -!
//extension Statement {
//    public static postfix func -!(_ s: Statement) -> Sentence {
//        Sentence.question(Question(s)) // TODO: goal
//    }
//}

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
        case .question(let question):
            return "\(question)"
        case .pause(let t):
            return "💤 \(Double(t)/1000) seconds"
        }
    }
}

extension Bag: CustomStringConvertible {
    public var description: String {
        queue.sync {
            let x = I.self == Concept.self ? "" : ".  "
            let o = items.values.reduce("", { $0 + "\($1)\n" + x })
            return String(o.dropLast(x.count))
        }
    }
}



