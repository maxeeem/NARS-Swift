
public enum Sentence {
    case pause(UInt32)
    case judgement(Judgement)
    case question(Question)
    /// default wait time in t * 0.1 of a second
    public static var pause: Sentence { .pause(10) }
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
                    usleep(t * 100000) // 0.1 second
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
    private func process(_ input: Sentence, in conceptBag: Bag<Concept>, recurse: Bool = true, userInitiated: Bool = false) {
        output((userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + " \(input)")
        
        if case .judgement(let j) = input, j.statement.subject == j.statement.predicate {
            // TODO: is there a better way?
            // is it needed for conversion? 
            return // tautology is ignored
        }
        
        switch input {
        case .judgement(let judgement):
            var derivedJudgements: [Judgement] = []
            if userInitiated { // memory or imagination
                derivedJudgements = memory.consider(judgement)
            } else {
                derivedJudgements = imagination.consider(judgement) 
            }
            //print(judgement, userInitiated, "\n", derivedJudgements)
            if recurse {
//                print("===", j)
                    if userInitiated {
                        
//                        self.queue.async {
                            derivedJudgements.forEach { j in
//                                print("===", j)
                                
                            self.process(.judgement(j), in: self.memory,
                                         recurse: false, 
                                         userInitiated: false)
                            }
//                        }
                    } else {
                    self.iqueue.addOperation {
                        derivedJudgements.forEach { j in
                            
                        self.process(.judgement(j), in: self.imagination,
                                     recurse: true, 
                                     userInitiated: false)
                            
                    }
                    }
                }
            }
            
        case .question(let question):
            //let derivedJudgements = memory.consider(question) // always consider in memory
            var derivedJudgements: [Judgement] = []
            if userInitiated { // memory or imagination
                derivedJudgements = memory.consider(question)
            } else {
                derivedJudgements = imagination.consider(question) 
            }
            if case .statement(let statement) = question {
                
                if let winner = derivedJudgements.first, winner.statement == statement {
                    output(".  💡 \(winner)")
                    
                    if !userInitiated {
                        // cancel all in-flight activities
                        //self.queue.async {
                            self.iqueue.isSuspended = true
                            self.iqueue.cancelAllOperations()
                            self.iqueue.isSuspended = false
                                }
                        // process winning judgement
                    //self.queue.async {
                    if !userInitiated {
                            self.process(.judgement(winner), in: self.memory, 
                                         recurse: false, // determines if derived judgements are inserted
                                         userInitiated: true) // will cause insertion into main memory
                                //}
                            }
                    //break // interrupt the flow
                    
                } else if recurse {
                    // switch to imagination flow
                    if userInitiated {
                        // very inefficient but just a poc
                        imagination = Bag<Concept>() // TODO: create wrapper
                        iqueue.addOperation { 
                            self.memory.items.forEach { (key: String, value: Concept) in
                                let c = Concept(term: value.term)
                                value.termLinks.items.forEach {
                                    c.termLinks.put($0.value)
                                }
                                value.beliefs.items.forEach {
                                    c.beliefs.put($0.value)
                                }
                                self.imagination.put(c)
                            }
                        }
                    }
                    
                    derivedJudgements.forEach { j in
                        self.iqueue.addOperation {
                            self.process(.judgement(j), in: self.imagination, recurse: true)
                            // re-process question
                            self.process(.question(question), in: self.imagination, recurse: true)
                        }
                    }
                
                    // re-process question
                    self.iqueue.addOperation { 
                        //self.process(.question(question), in: self.imagination, recurse: true)
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
