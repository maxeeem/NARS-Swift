
public enum Sentence {
    case judgement(Judgement)
    case question(Question)
}

import Dispatch

public final class NARS {
    public let name: String
    public private(set) var memory = Bag<Concept>()
    public let output: (String) -> Void
    private var queue = DispatchQueue(label: "input", qos: .userInitiated)
    public init(_ name: String = "𝝥𝝠𝗟", _ output: @escaping (String) -> Void = { print($0) }) {
        self.name = name
        self.output = output
    }
    public func reset() {
        memory = Bag<Concept>()
    }
    public func perform(_ script: Sentence...) { // convenience
        perform(script)
    }
    public func perform(_ script: [Sentence]) {
        // TODO: add buffer
        DispatchQueue.global(qos: .userInitiated).async {
            script.forEach { s in
                self.queue.async {
                    self.process(s, userInitiated: true)
                }
            }
        }
    }
}

// MARK: Private

extension NARS {
    public static var derivationLevels = 1
    
    private func process(_ input: Sentence, recurse: Int = NARS.derivationLevels, userInitiated: Bool = false) {
        output((userInitiated ? "•" : ".") + (recurse == 1 && userInitiated ? "" : "  ⏱") + " \(input)")
        if case .judgement(let j) = input, j.statement.subject == j.statement.predicate {
            return // tautology
        }
        switch input {
        case .judgement(let judgement):
            let derivedJudgements = memory.consider(judgement)
            if recurse > 0 {
                derivedJudgements.forEach { j in
                    self.process(.judgement(j), recurse: recurse - 1) 
                }
            }
        case .question(let question):
            let derivedJudgements = memory.consider(question)
            if case .statement(let statement) = question {
                if let winner = derivedJudgements.first, winner.statement == statement {
                    output(".  💡 \(winner)")
                } else if recurse > 0 {
                    let levels = recurse > 1 ? recurse - 1 : 1
                    derivedJudgements.forEach { j in process(.judgement(j), recurse: levels) }
                    // re-process question
                    process(.question(question), recurse: 1)
                }
            } else if let winner = derivedJudgements.first {
                output(".  💡 \(winner)")
            } else {
                output("\tI don't know 🤷‍♂️")
            }
        }
    }
}
