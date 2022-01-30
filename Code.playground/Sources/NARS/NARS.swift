import Foundation
import Dispatch

public enum Sentence {
    case pause(Int)
    case judgement(Judgement)
    case question(Question)
    
    /// default wait time in milliseconds (0.001s)
    /// neurons spike between 5ms and 1000ms
    public static var pause: Sentence { .pause(defaultPause) }
    public static var defaultPause = 10000
}

public final class NARS {
    public let name: String
    public internal(set) var memory = Bag<Concept>()
    public internal(set) var imagination = Bag<Concept>()
    public let output: (String) -> Void
    private var queue = DispatchQueue(label: "input", qos: .userInitiated)
    private var iqueue = OperationQueue()//(label: "imagination", qos: .background)
//    private var dreaming = false // TODO: workaround to avoid using OperationQueue
    
//    public var pendingTasks = Bag<Task>()
    
    public init(_ name: String = "𝝥𝝠𝗟", _ output: @escaping (String) -> Void = { print($0) }) {
        self.name = name
        self.output = output
        self.iqueue.maxConcurrentOperationCount = 1
    }
    public func reset() {
//        dreaming = false
        iqueue.isSuspended = true
        iqueue.cancelAllOperations()
        iqueue.isSuspended = false
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
                let ms = 1000 // millisecond
                usleep(useconds_t(t * ms))
            }
//            iqueue.isSuspended = true
//            iqueue.cancelAllOperations()
//            iqueue.isSuspended = false
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
            if j.truthValue.confidence == 0 {
                return false
            }
            if case .judgement(let judgement) = input, (judgement.statement == j.statement) || judgement.statement.isTautology {
                return false
            }
            return true
        })
        
        derivedJudgements = Array(Set(derivedJudgements))
//        print(derivedJudgements)
        if derivedJudgements.isEmpty { 
            if case .question = input {
//                output("\t(1)I don't know 🤷‍♂️")
                output("thinking...")
                // iqueue.addOperations([MemCopy(self)], waitUntilFinished: true)
                iqueue.addOperation {
                    self.imagination = self.memory.copy()
                    //imagine()
                    // re-process question
                    self.process(input)
                }
            } else {
                return
            }
        }
        
        // helper
        func imagine(recurse r: Bool = true) {
            //print("dj \(derivedJudgements)")
            derivedJudgements.forEach { j in
//                if dreaming {
                    process(.judgement(j), recurse: r)
//                }
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
                    
//                    if !userInitiated {
                        // cancel all in-flight activities
//                        dreaming = false
                        iqueue.isSuspended = true
                        iqueue.cancelAllOperations()
                        iqueue.isSuspended = false
                        
                        // process winning judgement
                        process(.judgement(winner),
                                     recurse: false, // determines if derived judgements are inserted
                                     userInitiated: true) // will cause insertion into main memory
//                    }
                    
                    output(".  💡 \(winner)")
                    
                } else if recurse { // switch to imagination flow
                    
                    if userInitiated {
                        // very inefficient but just a poc
                        iqueue.addOperations([MemCopy(self)], waitUntilFinished: true)
//                            self.imagination = self.memory.copy()
//                            self.dreaming = true
//                        }
                    }
                    
                    iqueue.addOperation {
                        imagine()
                        // re-process question
                        self.process(.question(question))
                    }
                }
                
            } else if let winner = derivedJudgements.first {
                output(".  💡 \(winner)")
            } else {
                output("\t(2)I don't know 🤷‍♂️")
            }
            
        case .pause: 
            break // do nothing
        }
    }
}

class MemCopy: Operation {
    weak var nars: NARS!
    init(_ nars: NARS) {
        self.nars = nars
    }
    override func main() {
        nars!.imagination = nars!.memory.copy()
    }
}
