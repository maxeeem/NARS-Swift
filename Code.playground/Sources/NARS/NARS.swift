
public enum Sentence {
    case judgement(Judgement)
    case goal(Goal)
    case question(Question)
    case cycle(Int)
}

extension Sentence {
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
    
//    private var factor = 4 // cycles per pause // TODO: dynamically adjust per system
//    fileprivate lazy var cycleLength = (Sentence.defaultPause / factor) * 1000000 // 0.001 second
    
//    fileprivate var lastPerformance = DispatchWallTime.now()
    
    private let timeProviderMs: () -> UInt32
    
    public init(timeProviderMs: @escaping () -> UInt32, _ output: @escaping (String) -> Void = { print($0) }) {
        self.output = output
        self.timeProviderMs = timeProviderMs
    }
    
    public func reset() {
        imaginationBuffer.removeAll()
        derivedQuestions.removeAll()
        
        memory = Bag<Concept>()
        imagination = WrappedBag(memory)
        recent = Bag<Belief>(4,40)
    }
    
    public func perform(_ script: Sentence...) { // blocking
        for s in script {
            processRecent(s)
            
            process(anticipations: s) // TODO: should be done inside MEMORY to avoid extra read/write on concepts
            
            for j in processInput(s) {
                process(.judgement(j),
                        recurse: false, // determines if derived judgements are inserted
                        userInitiated: true) // will cause insertion into main memory
            }
        }
    }
    
    public func perform(_ statement: Statement) { // convenience
        perform(statement-*)
    }

    
    // stores sentences for subsequent processing in imagination
    private var imaginationBuffer = [Sentence]()
    
    // stores derived questions and their roots for backward inference
    private var derivedQuestions = [Statement: (judgement: Judgement, rule: Rules)]()
    
    
    // MARK: Recent
    
    private func processRecent(_ s: Sentence) {
        if case .judgement(let j) = s {
            for j in process(recent: j) {
                // add stable patterns from recent memory
                process(.judgement(j), recurse: false, userInitiated: true)
            }
        }
    }
    
    // MARK: Input
    
    private func processInput(_ s: Sentence) -> [Judgement] {
        //
        // TODO: account for tense in question answering
        //
        
        // set time stamp if not yet set
        let s = s.setTimestamp(timeProviderMs)
        
        /// JUDGEMENT
        if case .judgement(let judgement) = s {
            recentInput.append(judgement)
        }
        
        /// QUESTION
        if case .question(let q) = s, case .statement(let sub, _, _) = q.statement {
            // check recent memory, then imagination
            if let answer = recent.peek(q.identifier)?.judgement // OR
                ?? imagination.consider(q, derive: false).first(where: { $0.statement == q.statement }) {
                // check main memory if the answer is already present
                let c = memory.items[sub.description]
                if c == nil || c!.beliefs.items.contains(where: { $0.value.judgement.statement == answer.statement }) == false {
                    /// ANSWER
                    process(.judgement(answer), recurse: false, userInitiated: true)
                }
            }
        }
    
        /// SENTENCE
        return process(s, userInitiated: true) // process in main memory
    }
    
    private var recentInput: [Judgement] = []
}


// MARK: Private

extension NARS {
    fileprivate func process(anticipations for: Sentence) {
        if case .judgement(let judgement) = `for` {
            if let concept = memory.get(judgement.statement.description) {
                for ant in concept.anticipations {
                    //                    print("ANT1", ant)
                    let st = ant.value.0
                    let tv = ant.value.1
                    if case .statement(let s, _, let p) = st {
                        
                        let tail = recentInput.reversed()
                            .prefix(while: { $0.statement.description != judgement.statement.description }).map({ $0.statement })
                        if tail.count == recentInput.count {
                            break // no history of this term
                        }
                        if tail.contains(s) {
//                        if recent.items.contains(where: { $0.key == "|| " + s.description }) { // TODO: handle tense correctly
                            // TODO: do an appropriate check to make sure preconditions have been met
                            
                            // check if it applies based on the temporal window
                            // TODO: determine if it is =/>, <=/> or none
                            
                            let c = tv.c / (tv.c + k)
                            let observed = Judgement(st, TruthValue(tv.f, c), tense: judgement.tense, timestamp: judgement.timestamp)
                            
                            if concept.term == p { // isPredicate
                                if let subject = memory.get(s.description) { // get subject concept
//                                    print("DS",
                                          subject.accept(observed, derive: false)
//                                    )
                                    memory.put(subject)
                                }
//                                print("DP",
                                      concept.accept(observed, derive: false)
//                                )
                            }
                            
                        }
                    }
                }
                memory.put(concept)
            }
            //
            // store anticipations for later access
            for ant in memory.peek(judgement.statement.description)?.anticipations(for: judgement) ?? [:] {
                //                print("ANT2", ant)
                let st = ant.value.0
                //                let tv = ant.value.1
                if case .statement(_, _, let p) = st {
                    if var pc = memory.get(p.description) {
                        pc.anticipations[ant.key] = ant.value
                        memory.put(pc)
                    }
                    
                    output(".  ⏱ anticipate " + "<\(p)>.") // add deduction calculation
                }
            }
        }
    }
    
    fileprivate func process(recent j: Judgement) -> [Judgement] {
        guard recent.peek(j.identifier) == nil else {
            return []// no need to process what we already know
        }
        
        var derived: [Belief] = [j + 0.9]
        
        var variants = [Judgement]()
        
        Theorems.apply(j).forEach {
            if recent.peek($0.identifier) == nil {
                recent.put($0 + 0.9)
                variants.append($0)
            }
        }
        
//        print("D", derived)
//        print("R", recent)
        var stable: [Judgement] = []
        
        while let b = recent.get() {
            derived.append(b)
            // only process direct experiences
            if b.judgement.truthValue.rule == nil, b.judgement.timestamp != ETERNAL, j.timestamp != ETERNAL {
                // process temporal
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
                        
                        chooseBestAndAppend()
                        
//                        if el.tense != nil {
//                            let tv = el.truthValue
//                            let elc = tv.c / (tv.c + k)
////                            print("OK", el, el.derivationPath)
//                            el = Judgement(el.statement, TruthValue(tv.f, elc, el.truthValue.rule), el.derivationPath, tense: nil, timestamp: ETERNAL)
//                            chooseBestAndAppend()
//                            if derived.first(where: { $0.judgement.identifier == el.identifier }) == nil {
//                                stable.append(el)
//                            }

                            // add to main memory
                            // TODO: figure out how to accomplish evidence accumulation
                            // because as it stands, there is evidence overlap
                            // so choice rule will be used instead of revision
//                            stable.append(el)
//                        }
                        
                        func chooseBestAndAppend() {
                            if let d = derived.first(where: { $0.judgement.identifier == el.identifier }) {
                                el = choice(j1: d.judgement, j2: el)
                            }
                            derived.append(el + 0.9)
                        }
                    }
                }
            }
            
            if b.judgement.statement != j.statement && !variants.contains(where: { $0.statement == b.judgement.statement }) {
                
                Rules.allCases.flatMap { r in
                    r.apply((b.judgement, j)) +
                    variants.flatMap({ r.apply((b.judgement, $0)) })
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
    
    /// Main processign function
    /// - Parameters:
    ///   - input: input `Sentence`
    ///   - recurse: determines if derived judgements are produced
    ///   - userInitiated: `true` will cause insertion into main memory
    @discardableResult
    fileprivate func process(_ input: Sentence, recurse: Bool = true, userInitiated: Bool = false) -> [Judgement] {
        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + " "
            
        // set time stamp if not yet set
        let input = input.setTimestamp(timeProviderMs)

        output(label + "\(input)")

        // process in memory or imagination
        var derived = ((userInitiated ? memory : imagination) as MEM)
            .consider(input, derive: recurse)
            .remove(matching: input)
        
        
//        if derived.isEmpty == false {
//            print("processed \(input)\n\tderived \(derived)")
//
//        }
        
        // check inference results
        if case .question = input {
            if derived.isEmpty {
                if !userInitiated {
                    self.imagination.reset()
                }
                output(label + "thinking... \(input)")
                imaginationBuffer.insert(input, at: 0)
                
                return [] // BREAK THE FLOW
            }
        }
        
        // CONTINUE MAIN FLOW
        
        switch input {
            
        case .judgement:
            //  consider in imagination
            if recurse && !userInitiated {
//                print("D", derived)
//                print(imaginationBuffer)
                imaginationBuffer.enqueue(derived)
//                print(imaginationBuffer)
            }
            
        case .goal:
            break // TODO: finish implementation
            
        case .question(let question):
            /// consider a question
            if case .statement(let s, _, let p) = question.statement {
                if case .variable = s {
                    output(label + "💡 \(derived.first!)")
                } else if case .variable = p {
                    output(label + "💡 \(derived.first!)")

                } else if let winner = derived.first, winner.statement == question.statement {
                    
                    if !userInitiated {
                        // cancel in-flight activities
                        if let source = derivedQuestions[winner.statement] {
                            derivedQuestions.removeValue(forKey: winner.statement)
                            
                            imaginationBuffer.cleanup(winner.statement)
                            
                            let answer = source.rule.apply((source.judgement, winner))
                                .compactMap { $0 }
                                .map { Sentence.judgement($0) }
                            
                            imaginationBuffer.insert(contentsOf: answer, at: 0)
                        }
                        
                        // process winning judgement
                        process(.judgement(winner),
                                recurse: false, // determines if derived judgements are inserted
                                userInitiated: true) // will cause insertion into main memory
                    }
                    
                    output(label + "💡 \(winner)")
//                    print("}}", winner.derivationPath)
                    
                    break // question answered
                    
                /*
                 * IMAGINATION -- derived questions
                 */
                    
                } else if recurse { // switch to imagination flow
                    
                    let source = derived.removeFirst()

                    derived.forEach { j in
                        derivedQuestions[j.statement] = (source, j.truthValue.rule ?? .deduction)
                    }
                    
                    let qs: [Sentence] = derived.reversed().flatMap {
                        [.question(question), .question(.init($0.statement))]
                    }
                    
                    imaginationBuffer.insert(contentsOf: qs, at: 0)
                    
                    return []
                    
                } else {
                    
                    output("\t(2)I don't know 🤷‍♂️")
                }
            }
            
        /// CYCLE
        
        case .cycle(let n):
            for _ in 0 ..< n {
//                print("Count:", imaginationBuffer.count)
                if let s = imaginationBuffer.popLast() {
                    process(s, recurse: false)
                } else {
                    mainCycle()
                }
            }
        }
        
        /// DERIVED
        return derived
    }
    
    
    // MARK: Cycles
    
    private func mainCycle() {
        // TODO: potentially get items from recent memory and process them in imagination
        //        if let r = recent.get() {
        //            recent.put(r)
        //            self.process(.judgement(b.judgement))
        //        }

        if let c = imagination.get(), let b = c.beliefs.get() {
            c.beliefs.put(b)
            imagination.put(c)
            
            let immediate = Rules.immediate(b.judgement)
            let structural = Theorems.apply(b.judgement)
            
            let results = (immediate + structural).filter { !imagination.contains($0) }
//            print("J1", b.judgement)
//            print("R", results)
//            print(imagination)
//            print(results.contains(b.judgement))
            
            results.forEach { j in
                process(.judgement(j))
            }
        }
    }
}
