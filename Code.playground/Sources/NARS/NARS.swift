import Dispatch

public enum Sentence {
    case judgement(Judgement)
    case goal(Goal)
    case question(Question)
    
    case pause(Int)
    case cycle(Int)
}

extension Sentence {
    /// default wait time in milliseconds (0.001s)
    /// neurons spike between 5ms and 1000ms
    public static var pause: Sentence { .pause(defaultPause) }
    public static var defaultPause = 1000
    
    public static var cycle: Sentence { .cycle(1) }
}

public final class NARS: Item {
    public var identifier: String { name.description }
    public var priority: Double = 0.9
    
    public var name = Term.symbol("SELF") // TODO: inject via init
    
    public internal(set) var recent = Bag<Belief>(4,40) // TODO: use tense and therefore identifier for indexing
    public internal(set) var memory = Bag<Concept>()
    public internal(set) lazy var imagination = WrappedBag(memory)
    
    public var output: (String) -> Void
    
    private var queue = DispatchQueue(label: "input", qos: .userInitiated)
    private var iqueue = DispatchQueue(label: "imagination", qos: .userInitiated)
    private var cycleQueue = DispatchQueue(label: "cycle", qos: .utility)
    private var dreaming = false
    
//    public var pendingTasks = Bag<Task>()
//    private var lastQuestion: Statement?

    lazy var cycleItem = DispatchWorkItem { [weak self] in
        while true {
            guard let s = self, s.cycle else { continue }
            
            let quietTime = s.lastPerformance.rawValue - DispatchWallTime.now().rawValue
            
            // TODO: potentially get items from recent memory and process them in imagination  
            if quietTime > s.cycleLength, let c = s.imagination.get(), let b = c.beliefs.get() {
                
                c.beliefs.put(b)
                s.imagination.put(c)
                
                let immediate = Rules.immediate(b.judgement)
                let structural = Theorems.apply(b.judgement)
                
                let results = (immediate + structural).filter { !s.imagination.contains($0) }

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
    
    private var factor = 4 // cycles per pause // TODO: dynamically adjust per system
    fileprivate lazy var cycleLength = (Sentence.defaultPause / factor) * 1000000 // 0.001 second
    
    fileprivate var lastPerformance = DispatchWallTime.now()
    
    public init(cycle: Bool = true, _ output: @escaping (String) -> Void = { print($0) }) {
        self.output = output
        self.cycle = cycle

        if !cycle {
            cycleQueue.suspend()
        }
        cycleQueue.async(execute: cycleItem)
    }
    
    public func reset() {
        dreaming = false
        thinking = false
        cycleQueue.suspend()
        memory = Bag<Concept>()
        imagination = WrappedBag(memory)
        recent = Bag<Belief>(4,40)
        cycleQueue.resume()
        sleep(2)
    }
    
    public func perform(_ script: Sentence...) { // convenience
        perform(script)
    }
    
    public func perform(_ script: [Sentence]) {
        // TODO: add buffer
        script.forEach { s in
            
            /// PROCESS
            self.queue.async { // default processing queue
                
                var s = s // for updating timstamp
                
//                var recent: [Judgement] = []
                
                /// JUDGEMENT
                if case .judgement(let j) = s {
                    // set time stamp if not yet set
                    if j.timestamp == 0 {
                        s = .judgement(.updateTimestamp(j))
                    }
                    // process in recent memory
                    DispatchQueue.global().async {
                        let recent = self.process(recent: j)
                        for el in recent { // add stable patterns from recent memory
                            self.process(.judgement(el), recurse: false, userInitiated: true)
                        }
                    }
                
                //
                // TODO: account for tense in question answering
                //

                /// QUESTION
                } else if case .question(let q) = s, case .statement(let sub, _, _) = q.statement {
                    // check recent memory, then imagination
                    if let answer = self.recent.peek(q.identifier)?.judgement // OR
                        ?? self.imagination.consider(q, derive: false).first(where: { $0.statement == q.statement }) {
                        // check main memory if the answer is already present
                        if let c = self.memory.items[sub.description] {
                            if c.beliefs.items.contains(where: { $0.value.judgement.statement == answer.statement }) == false {
                                /// ANSWER
                                self.process(.judgement(answer), recurse: false, userInitiated: true)
                            }
                        } else {
                            /// ANSWER
                            self.process(.judgement(answer), recurse: false, userInitiated: true)
                       }
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
                if cycle == false {
                    cycle = true
                    think(n * Sentence.defaultPause)
                    cycle = false
                } else {
                    think(n * Sentence.defaultPause)
                }
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
    public var lastCycle: [(UInt64,String)] = []
    private var lastInput: Sentence!
}

// MARK: Private

extension NARS {
    fileprivate func process(recent j: Judgement) -> [Judgement] {
        guard recent.peek(j.identifier) == nil else {
//            print("}}", j)
            return []// no need to process what we already know
        }
//        print(">", j)
        
        var derived: [Belief] = [j + 0.9]
        
        Theorems.apply(j).forEach {
            if recent.peek($0.identifier) == nil {
                recent.put($0 + 0.9)
            }
        }
        
//        print("D", derived)
//        print("R", recent)
        var stable: [Judgement] = []
        
        while let b = recent.get() {
            derived.append(b)
//            print("L", b, j)
            // process temporal
            if b.judgement.truthValue.rule == nil, b.judgement.timestamp != ETERNAL, j.timestamp != ETERNAL {
//                print("K", b.judgement, j)
                // only process direct experiences
                Rules.allCases.flatMap { rs in
                    rs.variable_and_temporal.flatMap { r in
                        [rule_generator(r)((j, b.judgement)),
                         rule_generator(r)((b.judgement, j))] // switch order of premises
                    }
                }.forEach {
                    if var el = $0 {
//                        print(">>--", el)
//                        // set time stamp if not yet set
//                        if el.timestamp == 0 {
//                            let now = DispatchWallTime.now()
//                            if el.derivationPath.count == 1 { // also update derivationPath
//                                el = el.statement + (el.truthValue.f, el.truthValue.c, now.rawValue)
//                            } else {
//                                el.timestamp = now.rawValue
//                            }
//                        }
                        foo()
                        if el.tense != nil {
                            let tv = el.truthValue
                            let elc = tv.c / (tv.c + k)
//                            print("KK", el, el.derivationPath)
                            el = Judgement(el.statement, TruthValue(tv.f, elc, el.truthValue.rule), el.derivationPath, tense: nil, timestamp: ETERNAL)
                            foo()
                            
                            // add to main memory
                            // TODO: figure out how to accomplish evidence accumulation
                            // because as it stands, there is evidence overlap
                            // so choice rule will be used instead of revision
//                            process(.judgement(el), recurse: false, userInitiated: true)
                            stable.append(el)
                        }
                        
                        func foo() {
                            if let d = derived.first(where: { $0.judgement.identifier == el.identifier }) {
                                el = choice(j1: d.judgement, j2: el)
                            }
                            derived.append(el + 0.9)
                        }
                    }
                }
            }
            
            if b.judgement.statement != j.statement {
                
                Rules.allCases.flatMap { r in
                    r.apply((b.judgement, j))
                }.forEach {
                    if let el = $0 {
                        //                    if el.timestamp == 0 {
                        //                        let now = DispatchWallTime.now()
                        //                        if el.derivationPath.count == 1 { // also update derivationPath
                        //                            el = el.statement + (el.truthValue.f, el.truthValue.c, now.rawValue)
                        //                        } else {
                        //                            el.timestamp = now.rawValue
                        //                        }
                        //                    }
                        if let d = derived.first(where: { $0.judgement.identifier == el.identifier }) {
                            derived.append(choice(j1: d.judgement, j2: el) + 0.9)
                        } else {
                            derived.append(el + 0.9)
                        }
                    }
                }
                
            }
        }
        
        derived.forEach {
            recent.put($0)
        }
        
        return stable
    }
    
    fileprivate func process(_ input: Sentence, recurse: Bool = true, userInitiated: Bool = false) {
//        var recurse = recurse; recurse = true
        let now = DispatchWallTime.now()
//        if lastCycle.count == 1 && lastCycle[0] == (0,"") {
//            lastCycle.remove(at: 0) // first cycle
//        }
        let duration = lastInput == nil ? 0 : lastPerformance.rawValue - now.rawValue
        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + " \(input)"
        lastCycle.append((duration, label))
        
        lastInput = input
        lastPerformance = now
            
        var input = input // set time stamp if not yet set
        if case .judgement(let j) = input, j.timestamp == 0 {
            input = .judgement(.updateTimestamp(j))
        }

        output(label)
        
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
//            print("processed \(input)\n\tderived \(derivedJudgements)")
            return derivedJudgements
        }()
        
        if derivedJudgements.isEmpty { 
            if case .question = input {
//                output("\t(1)I don't know 🤷‍♂️")
                if userInitiated == false {
                    self.imagination.reset() //= self.memory.copy()
                }

                if thinking {
                    output("thinking... \(input)")

                    iqueue.async {
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
            
        case .goal:
            break // TODO: finish implementation 
        
        case .question(let question):
            /// consider a question 
            if case .statement(let s, _, let p) = question.statement {
                if case .variable = s {
                    if let winner = derivedJudgements.first {
                        let duration = lastPerformance.rawValue - DispatchWallTime.now().rawValue
                        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + "💡 \(winner)"
                        lastCycle.append((duration, label))
                        output(".  💡 \(winner)")
                    } else {
                        output("\t(2)I don't know 🤷‍♂️")
                    }
                } else if case .variable = p {
                    if let winner = derivedJudgements.first {
                        let duration = lastPerformance.rawValue - DispatchWallTime.now().rawValue
                        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + "💡 \(winner)"
                        lastCycle.append((duration, label))
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
                    } else {
                        let duration = lastPerformance.rawValue - DispatchWallTime.now().rawValue
                        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + "💡 \(winner)"
                        lastCycle.append((duration, label))
                    }
                    output(".  💡 \(winner)")
                    print("}}", winner.derivationPath)
//                    print("here", dreaming, cycle)
                    break
                    
                } else if recurse { // switch to imagination flow
//                    print("there", dreaming)
                    if userInitiated && !dreaming {
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
