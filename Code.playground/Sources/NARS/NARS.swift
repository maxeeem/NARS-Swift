import Dispatch

public enum Sentence {
    case pause(UInt32)
    case judgement(Judgement)
    case question(Question)
    /// default wait time in t * 0.1 of a second
    public static var pause: Sentence { .pause(100) }
}

public final class NARS {
    public let name: String
    public private(set) var memory = Bag<Concept>()
    public private(set) var imagination = Bag<Concept>()
    public let output: (String) -> Void
    private var queue = DispatchQueue(label: "input", qos: .userInitiated)
    private var iqueue = DispatchQueue(label: "imagination", qos: .background)
    private var dreaming = false // TODO: workaround to avoid using OperationQueue
    
//    public var pendingTasks = Bag<Task>()
    
    public init(_ name: String = "𝝥𝝠𝗟", _ output: @escaping (String) -> Void = { print($0) }) {
        self.name = name
        self.output = output
    }
    public func reset() {
        dreaming = false
        memory = Bag<Concept>()
        imagination = Bag<Concept>()
    }
    public func perform(_ script: Sentence...) { // convenience
        perform(script)
    }
    public func perform(_ script: [Sentence]) {
        // TODO: add buffer
        script.forEach { s in
            self.queue.async { // default processing queue
                self.process(s, userInitiated: true)
            }
            if case .pause(let t) = s {
                usleep(t * 100000) // 0.1 second
            }
        }
    }
}

// MARK: Private

extension NARS {
    private func process(_ input: Sentence, recurse: Bool = true, userInitiated: Bool = false) {
        output((userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + " \(input)")
        
        // memory or imagination
        var derivedJudgements = (userInitiated ? memory : imagination).consider(input)
        derivedJudgements = derivedJudgements.filter({ j in
            if case .judgement(let judgement) = input, (judgement.statement == j.statement) || judgement.statement.isTautology {
                return false
            }
            return true
        })
        
        derivedJudgements = Array(Set(derivedJudgements))
//        print(derivedJudgements)
        if derivedJudgements.isEmpty { 
            if case .question = input {
                output("\tI don't know 🤷‍♂️")
            } 
            return 
        }
        
        // helper
        func imagine(recurse r: Bool = true) {
            //print("dj \(derivedJudgements)")
            derivedJudgements.forEach { j in
                if dreaming {
                    process(.judgement(j), recurse: r)
                }
            }
        }
        
        switch input {
        
        case .judgement:
            //  consider a judgement
            if !recurse { break } // return if no recursion is needed
            
            if userInitiated {
                derivedJudgements.forEach { j in
                    process(.judgement(j), recurse: false, userInitiated: true)
                }
            } else {
                imagine(recurse: false)
            }
            
        case .question(let question):
            /// consider a question 
            if case .statement(let statement) = question {

                if let winner = derivedJudgements.first, winner.statement == statement {
                    
                    if !userInitiated {
                        // cancel all in-flight activities
                        dreaming = false
                        
                        // process winning judgement
                        process(.judgement(winner),
                                     recurse: false, // determines if derived judgements are inserted
                                     userInitiated: true) // will cause insertion into main memory
                    }
                    
                    output(".  💡 \(winner)")
                    
                } else if recurse { // switch to imagination flow
                    
                    if userInitiated {
                        iqueue.sync { // very inefficient but just a poc
                            imagination = memory.copy()
                            dreaming = true
                        }
                    }
                    
                    iqueue.async {
                        imagine()
                        // re-process question
                        self.process(.question(question))
                    }
                }
                
            } else if let winner = derivedJudgements.first {
                output(".  💡 \(winner)")
            } else {
                output("\tI don't know 🤷‍♂️")
            }
            
        case .pause: 
            break // do nothing
        }
    }
}
