import Foundation
import Dispatch

public enum Sentence {
    case pause(Int)
    case judgement(Judgement)
    case question(Question)
    
    case cycle(Int)
    
    /// default wait time in milliseconds (0.001s)
    /// neurons spike between 5ms and 1000ms
    public static var pause: Sentence { .pause(defaultPause) }
    public static var defaultPause = 1000
    
    public static var cycle: Sentence { .cycle(1) }
}

public final class NARS {
    public internal(set) var memory = Bag<Concept>()
    public internal(set) var imagination = Bag<Concept>()
    public let output: (String) -> Void
    private var queue = DispatchQueue(label: "input", qos: .userInitiated)
    private var iqueue = DispatchQueue(label: "imagination", qos: .userInitiated)
    private var cycleQueue = DispatchQueue(label: "cycle", qos: .utility)
    private var dreaming = false
    
//    public var pendingTasks = Bag<Task>()
    private var lastQuestion: Statement?

    lazy var item = DispatchWorkItem { [weak self] in
        while true {
            if let s = self, s.cycle {
                let quietTime = s.lastPerformance.rawValue - DispatchWallTime.now().rawValue
                if quietTime > s.cycleLength, let c = s.memory.get(), let b = c.beliefs.get() {
                    c.beliefs.put(b)
                    s.memory.put(c)
                    s.process(.judgement(b.judgement), userInitiated: true)
                }
            }
        }
    }
    
    public var cycle = false
    
    fileprivate var cycleLength = 1000000 // 0.001 second
    
    fileprivate var lastPerformance = DispatchWallTime.now()
    
    public init(cycle: Bool = true, _ output: @escaping (String) -> Void = { print($0) }) {
        self.output = output
        self.cycle = cycle

        cycleQueue.async(execute: item)
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
                snooze(t)
            }
            if case .cycle(let n) = s {
                cycle = true
                snooze(n * Sentence.defaultPause)
                cycle = false
            }
        }
            
        func snooze(_ t: Int) {
            let ms = 1000 // millisecond
            usleep(useconds_t(t * ms))
        }
    }
}

// MARK: Private

extension NARS {
    fileprivate func process(_ input: Sentence, recurse: Bool = true, userInitiated: Bool = false) {
        lastPerformance = DispatchWallTime.now()

        output((userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + " \(input)")
        
        if userInitiated, case .question(let q) = input, case .statement(let s, _, let p) = q.statement {
            if case .variable = s {
                // nothing
            } else if case .variable = p {
                // nothing
            } else {
                lastQuestion = q.statement
            }
        }
        /*
        if case .judgement(let j) = input, lastQuestion == j.statement {
            iqueue.isSuspended = true
            iqueue.cancelAllOperations()
            iqueue.isSuspended = false
                /*
            // process winning judgement
            process(.judgement(j),
                    recurse: false, // determines if derived judgements are inserted
                    userInitiated: true) // will cause insertion into main memory
            //                    }
 */
            recurse = false
            userInitiated = true
            output(".  💡 \(j)")
            return
        }
 */
        // memory or imagination
        var derivedJudgements: [Judgement] = {
            var derivedJudgements = (userInitiated ? memory : imagination).consider(input, derive: recurse)
            derivedJudgements = derivedJudgements.filter({ j in
                if j.truthValue.confidence == 0 {
                    return false
                }
                if case .judgement(let judgement) = input, j == judgement || judgement.statement.isTautology {
                    return false
                }
                return true
            })
            derivedJudgements = Array(Set(derivedJudgements)) //TODO: use choice to additionally resolve duplicates
//        print(derivedJudgements)
            return derivedJudgements
        }()
        
        if derivedJudgements.isEmpty { 
            if case .question = input {
//                output("\t(1)I don't know 🤷‍♂️")
//                if dreaming {
                output("thinking... \(input)")
                // iqueue.addOperations([MemCopy(self)], waitUntilFinished: true)
                iqueue.async {
                    self.imagination = self.memory.copy()
                    //imagine()
                    // re-process question
                    self.process(input)
                }
//                }
            } else {
                return
            }
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
                    process(.judgement(j),
                            recurse: false, // determines if derived judgements are inserted
                            userInitiated: true) // will cause insertion into main memory
                }
            } else {
                imagine(recurse: false)
            }
            
        case .question(let question):
            /// consider a question 
            if case .statement(let s, _, let p) = question.statement {
                if case .variable = s {
                    if let winner = derivedJudgements.first {
                        output(".  💡 \(winner)")
                    } else {
                        output("\t(2)I don't know 🤷‍♂️")
                    }
                } else if case .variable = p {
                    if let winner = derivedJudgements.first {
                        output(".  💡 \(winner)")
                    } else {
                        output("\t(2)I don't know 🤷‍♂️")
                    }
                } else {
                if let winner = derivedJudgements.first, winner.statement == question.statement {
                    
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
                        // very inefficient but just a poc
                        iqueue.sync {
                            self.imagination = self.memory.copy()
                            self.dreaming = true
                        }
                    }
                    
                    iqueue.async {
                        imagine()
                        // re-process question
                        self.process(.question(question))
                    }
                } else {
                    output("\t(2)I don't know 🤷‍♂️")
                }
                }
            }
        case .pause, .cycle: 
            break // do nothing
        }
    }
}
