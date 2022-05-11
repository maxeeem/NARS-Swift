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

public final class NARS: Item {
    public var identifier: String { name.description }
    public var priority: Double = 0.9
    
    public var name = Term.word("SELF") // TODO: inject via init
    
    public internal(set) var recent = Bag<Belief>(4,20)
    public internal(set) var memory = Bag<Concept>()
    public internal(set) lazy var imagination = WrappedBag(memory)
    
    public let output: (String) -> Void
    private var queue = DispatchQueue(label: "input", qos: .userInitiated)
    private var iqueue = DispatchQueue(label: "imagination", qos: .userInitiated)
    private var cycleQueue = DispatchQueue(label: "cycle", qos: .utility)
    private var dreaming = false
    
//    public var pendingTasks = Bag<Task>()
//    private var lastQuestion: Statement?

    lazy var item = DispatchWorkItem { [weak self] in
        while true {
            guard let s = self, s.cycle else { continue }
            
            let quietTime = s.lastPerformance.rawValue - DispatchWallTime.now().rawValue
            
            if quietTime > s.cycleLength, let c = s.imagination.get(), let b = c.beliefs.get() {
                
                c.beliefs.put(b)
                s.imagination.put(c)
                
                var results = [b.judgement] + Theorems.apply(b.judgement)
                
                /// conversion is special
                if let conv = conversion(j1: b.judgement) {
                    if s.imagination.peek(conv.statement.description) == nil {
                        results.append(conv)
                    } else if case .statement(let sub, let cop, let pre) = conv.statement {
                        if s.imagination.peek(sub.description)?.beliefs.peek(conv.statement.description) == nil
                        && s.imagination.peek(pre.description)?.beliefs.peek(conv.statement.description) == nil {
                            results.append(conv)
                        }
                    }
                }
                
                results.forEach { j in
                    s.process(.judgement(j))
                }
            }
        }
    }
    
    public var cycle = false {
        didSet {
            if cycle {
                cycleQueue.resume()
            } else {
                cycleQueue.suspend()
            }
        }
    }
    
    fileprivate var cycleLength = 1000000 // 0.001 second
    
    fileprivate var lastPerformance = DispatchWallTime.now()
    
    public init(cycle: Bool = true, _ output: @escaping (String) -> Void = { print($0) }) {
        self.output = output
        self.cycle = cycle

        if !cycle {
            cycleQueue.suspend()
        }
        cycleQueue.async(execute: item)
    }
    
    public func reset() {
        dreaming = false
        memory = Bag<Concept>()
        imagination = WrappedBag(memory)
    }
    
    public func perform(_ script: Sentence...) { // convenience
        perform(script)
    }
    
    public func perform(_ script: [Sentence]) {
        // TODO: add buffer
        script.forEach { s in
            
            /// PROCESS
            self.queue.async { // default processing queue
                
                /// JUDGEMENT
                if case .judgement(let j) = s {
                    // process in recent memory
                    self.process(recent: j)
                
                /// QUESTION
                } else if case .question(let q) = s, case .statement(let sub, _, _) = q.statement {
                    // check recent memory, then imagination
                    if let answer = self.recent.peek(q.statement.description)?.judgement // OR
                        ?? self.imagination.consider(q, derive: false).first(where: { $0.statement == q.statement }),
                        // check main memory if the answer is already present
                        let c = self.memory.items[sub.description], c.beliefs.items.contains(where: { $0.value.judgement.statement == answer.statement }) == false {
                        
                        /// ANSWER
                        self.process(.judgement(answer), recurse: false, userInitiated: true)
                    }
                }
                
                /// SENTENCE
                self.process(s, userInitiated: true) // process in main memory
            }
            
            /// PAUSE
            if case .pause(let t) = s {
                snooze(t)
            }
            
            /// CYCLE
            if case .cycle(let n) = s {
                cycle = true
                think(n * Sentence.defaultPause)
                cycle = false
            }
        }
            
        func snooze(_ t: Int) {
            thinking = true
            let ms = 1000 // millisecond
            usleep(useconds_t(t * ms))
            thinking = false
        }
        
        func think(_ t: Int) {
            thinking = true
            let deadline = dispatch_time_t(t) * 1000 * 1000
            let start = DispatchWallTime.now().rawValue
            while thinking, deadline > (start - DispatchWallTime.now().rawValue) {
                /// cycle
            }
            thinking = false
        }
    }
    
    private var thinking = false
}

// MARK: Private

extension NARS {
    fileprivate func process(recent j: Judgement) {
        guard recent.peek(j.description) == nil else {
            return // no need to process what we already know
        }
        
        var derived: [Belief] = [j + 0.9]
        
        Theorems.apply(j).forEach {
            if recent.peek($0.statement.description) == nil {
                recent.put($0 + 0.9)
            }
        }
        
        while let b = recent.get() {
            derived.append(b)
        
            Rules.allCases.flatMap { r in
                r.apply((b.judgement, j))
            }.forEach {
                if let el = $0 {
                    if let d = derived.first(where: { $0.judgement.statement == el.statement }) {
                        derived.append(choice(j1: d.judgement, j2: el) + 0.9)
                    } else {
                        derived.append(el + 0.9)
                    }
                }
            }
        }
        
        derived.forEach {
            recent.put($0)
        }
    }
    
    fileprivate func process(_ input: Sentence, recurse: Bool = true, userInitiated: Bool = false) {
        lastPerformance = DispatchWallTime.now()

        output((userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + " \(input)")
        
        // memory or imagination
        let derivedJudgements: [Judgement] = {
            var derivedJudgements: [Judgement]
            if userInitiated {
                derivedJudgements = memory.consider(input, derive: recurse)
            } else {
                derivedJudgements = imagination.consider(input, derive: recurse)
            }
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
                if thinking {
                    output("thinking... \(input)")

                    iqueue.async {
//                    self.imagination.reset() //= self.memory.copy()
//                    self.dreaming = true
                    // re-process question
                        self.process(input)
                    }
                }
            }
            return
        }
        
        // helper
        func imagine(recurse r: Bool = true) {
            //print("dj \(derivedJudgements)")
            derivedJudgements.forEach { j in
                if dreaming || cycle {
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
                } else if let winner = derivedJudgements.first, winner.statement == question.statement {
                    
                    if !userInitiated {
                        // cancel all in-flight activities
                        dreaming = false
                        thinking = false
                        
                        // process winning judgement
                        process(.judgement(winner),
                                     recurse: false, // determines if derived judgements are inserted
                                     userInitiated: true) // will cause insertion into main memory
                    }
                    
                    output(".  💡 \(winner)")
//                    print("here", dreaming, cycle)
                    break
                    
                } else if recurse { // switch to imagination flow
//                    print("there", dreaming)
                    if userInitiated {
//                        iqueue.sync {
//                            self.imagination.reset() //= self.memory.copy()
//                        }
                        self.dreaming = true
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
        case .pause, .cycle: 
            break // do nothing
        }
    }
}
