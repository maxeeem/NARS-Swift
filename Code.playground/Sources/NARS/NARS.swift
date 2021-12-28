
public enum Sentence {
    case judgement(Judgement)
    case question(Question)
}

import Dispatch

public final class NARS {
    public let name: String
    public let memory = Bag<Concept>()
    public let output: (String) -> Void
    private var queue = DispatchQueue(label: "input", qos: .userInitiated)
    public init(_ name: String = "𝝥𝝠𝗟", _ output: @escaping (String) -> Void = { print($0) }) {
        self.name = name
        self.output = output
    }
    public func perform(_ script: Sentence...) { // convenience
        perform(script)
    }
    public func perform(_ script: [Sentence]) {
        // TODO: add buffer
        DispatchQueue.global(qos: .userInitiated).async {
            self.queue.async {
            script.forEach { s in
                self.process(s, userInitiated: true)
                }
            }
        }
    }
}

// MARK: Private

extension NARS {
    private func process(_ input: Sentence, recurse: Bool = true, userInitiated: Bool = false) {
        output((userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + " \(input)")
        if case .judgement(let j) = input, j.statement.subject == j.statement.predicate {
            return // tautology
        }
        switch input {
        case .judgement(let judgement):
            let derivedJudgements = memory.consider(judgement)
            if recurse { // TODO: add levels
                derivedJudgements.forEach { j in process(.judgement(j), recurse: false) }
            }
        case .question(let question):
            let derivedJudgements = memory.consider(question)
            if case .statement(let statement) = question {
                if let winner = derivedJudgements.first, winner.statement == statement {
                    output(".  💡 \(winner)")
                } else if recurse { // TODO: add levels
                    derivedJudgements.forEach { j in process(.judgement(j), recurse: true) }
                    // re-process question
                    process(.question(question), recurse: false)
                }
            } else if let winner = derivedJudgements.first {
                output(".  💡 \(winner)")
            } else {
                output("\tI don't know 🤷‍♂️")
            }
        }
    }
}
