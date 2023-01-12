//#if os(Linux)
//  import Glibc
//  typealias dispatch_time_t = UInt32
//#elseif os(Windows)
//  import WinSDK
//  typealias dispatch_time_t = UInt32
//  typealias useconds_t = UInt32
//#endif

//import Dispatch

public enum Sentence {
    case judgement(Judgement)
    case goal(Goal)
    case question(Question)
    
//    case pause(Int)
    case cycle(Int)
}

extension Sentence {
    /// default wait time in milliseconds (0.001s)
    /// neurons spike between 5ms and 1000ms
//    public static var pause: Sentence { .pause(defaultPause) }
    public static var defaultPause = 1000
    
    public static var cycle: Sentence { .cycle(1) }
}

@dynamicMemberLookup
public final class NARS: Item {
    public var identifier: String { name.description }
    public var priority: Double = 0.9
    
    public var name = Term.symbol("SELF") // TODO: inject via init
    
    //    subscript(dynamicMember dynamicMember: KeyPath<Term, Term>) -> Term {
    subscript(dynamicMember dynamicMember: String) -> Concept {
        imagination.get(dynamicMember.description)
        ??
        {
            let new = Concept(string: dynamicMember)
            imagination.put(new)
            return new
        }()
    }
    
    public internal(set) var recent = Bag<Belief>(4,40) // TODO: use tense and therefore identifier for indexing
    public internal(set) var memory = Bag<Concept>()
    public internal(set) lazy var imagination = WrappedBag(memory)
    
    public var output: (String) -> Void
    
//    private var queue = DispatchQueue(label: "input", qos: .userInitiated)
//    private var iqueue = DispatchQueue(label: "imagination", qos: .userInitiated)
//    private var cycleQueue = DispatchQueue(label: "cycle", qos: .utility)
    
//    private var thinking = false

//    public var pendingTasks = Bag<Task>()
//    private var lastQuestion: Statement?

//    lazy var cycleItem = DispatchWorkItem { [weak self] in
//        while true {
//            guard let s = self, s.cycle else { continue }
//
//            let quietTime = s.lastPerformance.rawValue - DispatchWallTime.now().rawValue
//
//            // TODO: potentially get items from recent memory and process them in imagination
//            if quietTime > s.cycleLength, let c = s.imagination.get(), let b = c.beliefs.get() {
//
//                c.beliefs.put(b)
//                s.imagination.put(c)
//
//                let immediate = Rules.immediate(b.judgement)
//                let structural = Theorems.apply(b.judgement)
//
//                let results = (immediate + structural).filter { !s.imagination.contains($0) }
//
//                results.forEach { j in
//                    s.process(.judgement(j))
//                }
//            }
//        }
//    }
    
//    public var cycle = false {
//        didSet {
//            if cycle {
//                cycleQueue.resume()
//            } else {
//                cycleQueue.suspend()
//            }
//        }
//    }
    
//    private var factor = 4 // cycles per pause // TODO: dynamically adjust per system
//    fileprivate lazy var cycleLength = (Sentence.defaultPause / factor) * 1000000 // 0.001 second
    
//    fileprivate var lastPerformance = DispatchWallTime.now()
    
    let timeProviderMs: () -> UInt32
    
    public init(timeProviderMs: @escaping () -> UInt32, _ output: @escaping (String) -> Void = { print($0) }) {
        self.output = output
        self.timeProviderMs = timeProviderMs
//        if !cycle {
//            cycleQueue.suspend()
//        }
//        cycleQueue.async(execute: cycleItem)
    }
    
    public func reset() {
//        thinking = false
//        cycleQueue.suspend()
        inputBuffer.removeAll()
        recentBuffer.removeAll()
        imaginationBuffer.removeAll()
        
        memory = Bag<Concept>()
        imagination = WrappedBag(memory)
        recent = Bag<Belief>(4,40)
//        cycleQueue.resume()
//        _sleep(2)
    }
    
    public func perform(_ script: Sentence...) { // convenience
//        perform(script)
        inputBuffer.insert(contentsOf: script.reversed(), at: 0)
        for _ in 0 ..< script.count {
            inputCycle()
            recentCycle()
        }
    }
    
    var inputBuffer: [Sentence] = []
    
    func inputCycle() {
        guard var s = inputBuffer.popLast() else {
            return
        }
        
        /// JUDGEMENT
        if case .judgement(let j) = s {
            // set time stamp if not yet set
            if j.timestamp == 0 {
                s = .judgement(.updateTimestamp(j, timeProviderMs))
            }
            // process in recent memory
            recentBuffer.insert(j, at: 0)
        }
        
        //
        // TODO: account for tense in question answering
        //
        
        /// QUESTION
        if case .question(let q) = s, case .statement(let sub, _, _) = q.statement {
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
        
        
        /// PAUSE
//        if case .pause(let t) = s {
//            snooze(t)
//        }
        
        /// CYCLE
        if case .cycle(let n) = s {
//            thinking = true
            for _ in 0 ..< n {
                if imaginationBuffer.isEmpty {
                    mainCycle()
                } else {
                    imaginationCycle()
                }
            }
//            thinking = false
//            if cycle == false {
//                cycle = true
//                think(n * Sentence.defaultPause)
//                cycle = false
//            } else {
//                think(n * Sentence.defaultPause)
//            }
        }
    }
    
    func mainCycle() {
        // TODO: potentially get items from recent memory and process them in imagination
        //        if let r = recent.get() {
        //            recent.put(r)
        //            self.process(.judgement(b.judgement))
        //        }

        if let c = self.imagination.get(), let b = c.beliefs.get() {
            c.beliefs.put(b)
            self.imagination.put(c)
            
            let immediate = Rules.immediate(b.judgement)
            let structural = Theorems.apply(b.judgement)
            
            let results = (immediate + structural).filter { !self.imagination.contains($0) }

            results.forEach { j in
                self.process(.judgement(j))
            }
        }
    }
    
    var recentBuffer: [Judgement] = []
    
    func recentCycle() {
        guard let j = recentBuffer.popLast() else {
            return
        }
        let recent = self.process(recent: j)
        for el in recent { // add stable patterns from recent memory
            self.process(.judgement(el), recurse: false, userInitiated: true)
        }
    }
    
    var imaginationBuffer: [Sentence] = []
    
    func imaginationCycle() {
        guard let s = imaginationBuffer.popLast() else {
            return
        }
        self.process(s)
    }
    
    
//    public func perform(_ script: [Sentence]) {
//        // TODO: add buffer
//        script.forEach { s in
//
//            /// PROCESS
//            self.queue.async { // default processing queue
//
//                var s = s // for updating timstamp
//
////                var recent: [Judgement] = []
//
//                /// JUDGEMENT
//                if case .judgement(let j) = s {
//                    // set time stamp if not yet set
//                    if j.timestamp == 0 {
//                        s = .judgement(.updateTimestamp(j))
//                    }
//                    // process in recent memory
//                    DispatchQueue.global().async {
//                        let recent = self.process(recent: j)
//                        for el in recent { // add stable patterns from recent memory
//                            self.process(.judgement(el), recurse: false, userInitiated: true)
//                        }
//                    }
//
//                //
//                // TODO: account for tense in question answering
//                //
//
//                /// QUESTION
//                } else if case .question(let q) = s, case .statement(let sub, _, _) = q.statement {
//                    // check recent memory, then imagination
//                    if let answer = self.recent.peek(q.identifier)?.judgement // OR
//                        ?? self.imagination.consider(q, derive: false).first(where: { $0.statement == q.statement }) {
//                        // check main memory if the answer is already present
//                        if let c = self.memory.items[sub.description] {
//                            if c.beliefs.items.contains(where: { $0.value.judgement.statement == answer.statement }) == false {
//                                /// ANSWER
//                                self.process(.judgement(answer), recurse: false, userInitiated: true)
//                            }
//                        } else {
//                            /// ANSWER
//                            self.process(.judgement(answer), recurse: false, userInitiated: true)
//                       }
//                    }
//                }
//
//                /// SENTENCE
//                self.process(s, userInitiated: true) // process in main memory
//            }
//
//            /// PAUSE
//            if case .pause(let t) = s {
//                snooze(t)
//            }
//
//            /// CYCLE
//            if case .cycle(let n) = s {
//                if cycle == false {
//                    cycle = true
//                    think(n * Sentence.defaultPause)
//                    cycle = false
//                } else {
//                    think(n * Sentence.defaultPause)
//                }
//            }
//        }
//
//    }
//    func snooze(_ t: Int) {
//        thinking = true
//        let ms = 1000 // millisecond
//        _usleep(useconds_t(t * ms))
//        thinking = false
//    }
    
//    func think(_ t: Int) {
//        thinking = true
//        let deadline = dispatch_time_t(t) * 1000 * 1000
//        let start = DispatchWallTime.now().rawValue
//        while thinking, deadline > (start - DispatchWallTime.now().rawValue) {
//            /// cycle
//        }
//        thinking = false
//    }
    
    // TODO: remove -- this was for temporary profiling
//    public var lastCycle: [(UInt32,String)] = []
//    private var lastInput: Sentence!
    
    private var derivedQuestions: [Statement: (Judgement, Rules)] = [:]
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
//        let now = DispatchWallTime.now()
//        if lastCycle.count == 1 && lastCycle[0] == (0,"") {
//            lastCycle.remove(at: 0) // first cycle
//        }
//        let duration = lastInput == nil ? 0 : lastPerformance.rawValue - now.rawValue
        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + " \(input)"
//        lastCycle.append((duration, label))
        
//        lastInput = input
//        lastPerformance = now
            
        var input = input // set time stamp if not yet set
        if case .judgement(let j) = input, j.timestamp == 0 {
            input = .judgement(.updateTimestamp(j, timeProviderMs))
        }

        output(label)
        
        // memory or imagination
        let derivedJudgements: [Judgement] = {
            var derivedJudgements: [Judgement] = []
//            // apply theorems
//            if case .judgement(let judgement) = input {
//                derivedJudgements.append(contentsOf: Theorems.apply(judgement))
//            }
            // process in memory or imagination
            if userInitiated {
                derivedJudgements = memory.consider(input, derive: recurse)
            } else {
                derivedJudgements = imagination.consider(input, derive: recurse)
            }
            // filter out duplicates
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

//                if thinking {
                    output("thinking... \(input)")

//                    iqueue.async {
//                        // re-process question
//                        self.process(input)
//                    }
                    imaginationBuffer.insert(input, at: 0)
//                }
            }
            return
        }
        
        // helper
//        func imagine(recurse r: Bool = true) {
//            //print("dj \(derivedJudgements)")
//            derivedJudgements.forEach { j in
////                if thinking || cycle {
//                    process(.judgement(j), recurse: r)
////                }
//            }
//        }
        
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
                let js: [Sentence] = derivedJudgements.reversed().map({.judgement($0)})
                imaginationBuffer.insert(contentsOf: js, at: 0)
//                imagine(recurse: false)
            }
            
        case .goal:
            break // TODO: finish implementation 
        
        case .question(let question):
            /// consider a question 
            if case .statement(let s, _, let p) = question.statement {
                if case .variable = s {
                    if let winner = derivedJudgements.first {
//                        let duration = lastPerformance.rawValue - DispatchWallTime.now().rawValue
                        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + "💡 \(winner)"
//                        lastCycle.append((duration, label))
                        output(".  💡 \(winner)")
                    } else {
                        output("\t(2)I don't know 🤷‍♂️")
                    }
                } else if case .variable = p {
                    if let winner = derivedJudgements.first {
//                        let duration = lastPerformance.rawValue - DispatchWallTime.now().rawValue
                        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + "💡 \(winner)"
//                        lastCycle.append((duration, label))
                        output(".  💡 \(winner)")
                    } else {
                        output("\t(2)I don't know 🤷‍♂️")
                    }
                } else if let winner = derivedJudgements.first, winner.statement == question.statement {
                    
                    if !userInitiated {
                        // cancel all in-flight activities
//                        thinking = false
//                        print(imaginationBuffer)
//                        imaginationBuffer.removeAll()
                        if let source = derivedQuestions[winner.statement] {
//                            print("___", winner, source)
                            
                            derivedQuestions.removeValue(forKey: winner.statement)
                            
                            let idx = imaginationBuffer.firstIndex(where: { s in
                                if case .question(let q) = s {
                                    if q.statement == winner.statement {
                                        return true
                                    }
                                }
                                return false                                
                            })
                            if let i = idx {
                                imaginationBuffer = Array(imaginationBuffer.prefix(through: i))
                            }
                            
                            let answer = source.1.apply((winner, source.0))
                            .compactMap { $0 }
                            .map { Sentence.judgement($0) }
                            
                            imaginationBuffer.insert(contentsOf: answer, at: 0)
                        }
                        
                        // process winning judgement
                        process(.judgement(winner),
                                     recurse: false, // determines if derived judgements are inserted
                                     userInitiated: true) // will cause insertion into main memory
                    } else {
//                        let duration = lastPerformance.rawValue - DispatchWallTime.now().rawValue
                        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + "💡 \(winner)"
//                        lastCycle.append((duration, label))
                    }
                    output(".  💡 \(winner)")
                    print("}}", winner.derivationPath)
                    break
                    
                } else if recurse { // switch to imagination flow
//                    if userInitiated && !thinking {
//                        iqueue.sync {
//                            self.imagination.reset() //= self.memory.copy()
//                        }
//                        self.thinking = true
//                    }
                    
//                    iqueue.async {
//                        imagine()
//                        // re-process question
//                        self.process(.question(question))
//                    }
                    
//                    let js: [Sentence] = derivedJudgements.reversed().flatMap({[.question(question), .judgement($0)]})
                    
                    let source = derivedJudgements.first!
//                    print("\n\n", derivedQuestions, "\n\n")
                    derivedJudgements.dropFirst().forEach { j in
                        derivedQuestions[j.statement] = (source, j.truthValue.rule ?? .deduction)
                    }
                    
                    let js: [Sentence] = derivedJudgements.dropFirst().reversed().flatMap({[
                        .question(question),
                        .question(.init($0.statement))
                    ]})
                    
                    imaginationBuffer.insert(contentsOf: js, at: 0)
                    
                } else {
                    output("\t(2)I don't know 🤷‍♂️")
                }
            }
        case .cycle:
            break // do nothing
        }
    }
}


// MARK: - Compatibility

//func _sleep(_ t: UInt32) {
//    #if os(Windows)
//      Sleep(t * 1000)
//    #else
//      sleep(t)
//    #endif
//}
//
//func _usleep(_ t: UInt32) {
//    #if os(Windows)
//      Sleep(t)
//    #else
//      usleep(t)
//    #endif
//}

