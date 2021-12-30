
public enum Sentence {
    case pause(UInt32)
    case judgement(Judgement)
    case question(Question)
}

import Dispatch
import Foundation

public final class NARS {
    public let name: String
    public private(set) var memory = Bag<Concept>()
    public let output: (String) -> Void
    private var queue = DispatchQueue(label: "input", qos: .userInitiated)
    
//    public var pendingTasks = Bag<Task>()
    
    public init(_ name: String = "𝝥𝝠𝗟", _ output: @escaping (String) -> Void = { print($0) }) {
        self.name = name
        self.output = output
    }
    public func reset() {
        memory = Bag<Concept>()
        imagination = Bag<Concept>()
    }
    public func perform(_ script: Sentence...) { // convenience
        perform(script)
    }
    public func perform(_ script: [Sentence]) {
        // TODO: add buffer
        //DispatchQueue.global(qos: .userInitiated).async {
            script.forEach { s in
                self.queue.async {
                    self.process(s, in: self.memory, userInitiated: true)
                }
                if case .pause(let t) = s {
                    sleep(t)
                }
                
            }
                //}
    }
    
    private var iqueue: OperationQueue = {
        let queue = OperationQueue() // imagination
        queue.maxConcurrentOperationCount = 1
        return queue
    }()
    
    public private(set) var imagination = Bag<Concept>()
}

// MARK: Private

extension NARS {
    // TODO: remove
    public static var derivationLevels = 1
    public static var sleeping = 0
    
    private func process(_ input: Sentence, in conceptBag: Bag<Concept>, recurse: Bool = true, userInitiated: Bool = false) {
        output((userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + " \(input)")
        
        if case .judgement(let j) = input, j.statement.subject == j.statement.predicate {
            return // tautology is ignored
        }
        
        switch input {
        case .judgement(let judgement):
            let derivedJudgements = conceptBag.consider(judgement) // memory or imagination
            
            if recurse {
                derivedJudgements.forEach { j in
                    self.queue.async {
                        self.process(.judgement(j), in: conceptBag, recurse: false)
                    }
                }
            }
            
        case .question(let question):
            let derivedJudgements = memory.consider(question) // always consider in memory
            
            if case .statement(let statement) = question {
                
                if let winner = derivedJudgements.first, winner.statement == statement {
                    output(".  💡 \(winner)")
                    
                    if !userInitiated {
                        // cancel all in-flight activities
                        self.queue.async {
                            self.iqueue.isSuspended = true
                            self.iqueue.cancelAllOperations()
                            self.iqueue.isSuspended = false
                        }
                        // process winning judgement
                        self.queue.async {
                            self.process(.judgement(winner), in: self.memory, recurse: false)
                        }
                    }
                    return // interrupt the flow
                    
                } else if recurse {
                    // switch to imagination flow
                    
                    if userInitiated {
                        // very inefficient but just a poc
                        imagination = Bag<Concept>() // TODO: create wrapper
                        memory.items.forEach { (key: String, value: Concept) in
                            imagination.put(value)
                        }
                    }
                    
                    derivedJudgements.forEach { j in
                        self.iqueue.addOperation {
                            self.process(.judgement(j), in: self.imagination)
                        }
                    }
                
                    // re-process question
                    self.iqueue.addOperation {
                        self.process(.question(question), in: self.imagination)
                    }
                }
//            } else if let winner = derivedJudgements.first {
//                output(".  💡 \(winner)")
//                return
            } else {
                output("\tI don't know 🤷‍♂️")
                return
            }
            
        case .pause: break
        }
    }
}
