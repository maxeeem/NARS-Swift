
public enum Sentence: Hashable {
    case judgement(Judgement)
    case goal(Goal)
    case question(Question)

    case cycle(Int)
}

extension Sentence {
    public static var cycle: Sentence { .cycle(1) }
}

@dynamicCallable
@dynamicMemberLookup
public final class NARS: Item {
    public var identifier: String { name.description }
    public var priority: Double = 0.9
    
    public var name = Term.symbol("SELF") // TODO: inject via init
    
    //    subscript(dynamicMember dynamicMember: KeyPath<Term, Term>) -> Term {
    subscript(dynamicMember dynamicMember: String) -> Concept {
        memory.get(dynamicMember.description)
        ??
        {
            let new = Concept(string: dynamicMember)
            memory.put(new)
            return new
        }()
    }
    
    public func dynamicallyCall(withArguments args : [Term]) -> Term { // operations
        if let op = args.first?.description {
            let terms = args.count > 1 ? Array(args.suffix(from: 1)) : []
//            if let action = operations[op] {
//                return action(terms)
//            }
            return .operation(op, terms)
        }
            
        return .NULL
    }
    
    public internal(set) var recent = Bag<Belief>(4,40) // TODO: use tense and therefore identifier for indexing
    public internal(set) var memory = Bag<Concept>()
    
    // TODO: rename to stdout and add stderr
    public var output: (String) -> Void
    
    //    fileprivate var lastPerformance = DispatchWallTime.now()

    public typealias Op = ([Term]) -> Term
    public var operations: [String: Op] = [:]
    
    @discardableResult
    public func register(_ op: String, _ closure: @escaping Op) -> Op? {
        let existing = operations[op]
        operations[op] = closure
        return existing
    }
    
    @discardableResult
    public func unregister(_ op: String) -> Op? {
        let existing = operations[op]
        operations.removeValue(forKey: op)
        return existing
    }
    
    private let timeProviderMs: () -> UInt32
    
    public init(timeProviderMs: @escaping () -> UInt32, _ output: @escaping (String) -> Void = { print($0) }) {
        self.output = output
        self.timeProviderMs = timeProviderMs
        
        Theorems.cache.removeAll() // TODO: temporary workaround, needs better implementation
    }
    
    public func reset() {
        derivedBuffer.removeAll()
        derivedQuestions.removeAll()
        
        Theorems.cache.removeAll() // TODO: temporary workaround, needs better implementation
        
        memory = Bag<Concept>()
        recent = Bag<Belief>(4,40)
        buffer = Bag<Task>(4,40)
    }
    
    public func perform(_ script: Sentence...) { // blocking
        perform(script)
    }

    private func cycle() {
        if var concept = memory.get() {
//            print("cycle", concept.term.description)
            let results = concept.cycle()
                        
            if let derived = results["Judgement"] {
                concept.adjustPriority(derived)
                memory.put(concept)
                
                for j in derived {
                    output("--. ⏱ \(j)")

                    buffer.put(Task(sentence: .judgement(j)))

                    let revised = memory.consider(.judgement(j), derive: false)

                    for r in revised {
                        if r.statement == j.statement {
                            output("++. ⏱ \(r)")
                        } else {
                            // will not happen if derive=false
//                            buffer.put(Task(sentence: .judgement(r)))
                        }
                    }
                }
//                concept.adjustPriority(derived)
            }
            
            if let answers = results["Question"] {
                
                if answers.first?.statement == .NULL {
                    
                    for var chunk in answers.split(separator: .NULL-*) {
                        let source = chunk.removeFirst()
                        chunk.reversed().forEach { j in
                            let q1 = Sentence.question(Question(j.statement))
//                            output(". ⏱ \(q1)") // derived questions
//                            _ = memory.consider(q1, derive: false)
                        }
                    }
                    
                } else if let a = answers.first {
                    output(". 💡 \(a)")
                }

            }
            
            memory.put(concept)
        }
    }
    
    public var buffer = Bag<Task>(4,40)
    
    public func perform(_ script: [Sentence]) { // blocking
        for s in script {
            
            if case .cycle(let n) = s {
                for _ in 0..<n {
//                    print(memory)
                    cycle()
                    
                    if let t = buffer.get() {
                        output(". ⏱ \(t.sentence)")
                               
                        let revised = memory.consider(t.sentence, derive: false)

                        for r in revised {

                            if case .judgement(let judgement) = t.sentence,
                               r.statement == judgement.statement {
                                output(". ⏱ \(r)") // revision
                            } else {
                                // local inference
//                                buffer.put(Task(sentence: .judgement(r)))
                            }

        //                    memory.consider(.judgement(r), derive: false)
                        }

                    }
                }
            }
            
            if case .judgement(let j) = s {
                output("• \(s)")

//                for j in process(recent: j) {
//                    
//                    // add stable patterns from recent memory
//                    let revised = memory.consider(.judgement(j), derive: true)
//                    for r in revised {
//                        output(". ⏱ \(r)")
//
////                        memory.consider(.judgement(r), derive: false)
//                    }
//                }

                let revised = memory.consider(s, derive: true)
                
                for r in revised {
                    if r.statement == j.statement {
                        output(". ⏱ \(r)") // revision
                    } else {
                        // local inference
                        buffer.put(Task(sentence: .judgement(r)))
                    }

//                    memory.consider(.judgement(r), derive: false)
                }
            }
            
            if case .question(let question) = s {
                output("• \(s)")

//                if case .statement(let sub, _, _) = question.statement {
//                    // check recent memory first
//                    if let answer = recent.peek(question.identifier)?.judgement {
//                        // check main memory if the answer is already present
//                        let c = memory.items[sub.description]
//                        if c == nil || c!.beliefs.items.contains(where: { $0.value.judgement.statement == answer.statement }) == false {
//                            /// ANSWER
//                            process(.judgement(answer), recurse: false)
//                            let revised = memory.consider(.judgement(answer), derive: false)
//
//                            for r in revised {
//                                output(". ⏱ \(r)")
//                            }
//                        }
//                    }
//                }

                
                let answers = memory.consider(s, derive: true)
                
                if answers.first?.statement == .NULL {
                    
                    for var chunk in answers.split(separator: .NULL-*) {
                        let source = chunk.removeFirst()
                        chunk.reversed().forEach { j in
                            let q1 = Sentence.question(Question(j.statement))
                            output(". ⏱ \(q1)") // derived questions
                            _ = memory.consider(q1, derive: false)
                        }
                    }
                    
                } else if let a = answers.first {
                    output(". 💡 \(a)")
                }
            }
            
//            processInput(s)
//
//            process(anticipations: s)
        }
    }
    
    public func perform(_ statement: Statement) { // convenience
        perform(statement-*)
    }

    
    // stores sentences for subsequent processing in imagination
    private var derivedBuffer = [Sentence]()
    
    // stores derived questions and their roots for backward inference
    private var derivedQuestions = [Statement: (judgement: Judgement, rule: Rules)]()
    
    
    // MARK: Recent

    private func processRecent(_ s: Sentence) {
        if case .judgement(let j) = s {
            for j in process(recent: j) {
                // add stable patterns from recent memory
                process(.judgement(j), recurse: false)
            }
        }
    }
    
    // MARK: Input
    
    private func processInput(_ s: Sentence) {
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
            // check recent memory first
            if let answer = recent.peek(q.identifier)?.judgement {
                // check main memory if the answer is already present
                let c = memory.items[sub.description]
                if c == nil || c!.beliefs.items.contains(where: { $0.value.judgement.statement == answer.statement }) == false {
                    /// ANSWER
                    process(.judgement(answer), recurse: false)
                }
            }
        }
        
        /// SENTENCE
        process(s, label: "•") // process in main memory
    }
    
    private var recentInput: [Judgement] = []
}


// MARK: Private

extension NARS {
    // TODO: should be done inside MEMORY to avoid extra read/write on concepts
    fileprivate func process(anticipations for: Sentence) {
        if case .judgement(let judgement) = `for` {
            if let concept = memory.get(judgement.statement.description) {
                for ant in concept.anticipations {
                    let st = ant.value.0
                    let tv = ant.value.1
                    if case .statement(let s, _, let p) = st {
                        
                        let tail = recentInput.reversed()
                            .prefix(while: { $0.statement.description != judgement.statement.description }).map({ $0.statement })
                        if tail.count == recentInput.count {
                            break // no history of this term
                        }
                        if tail.contains(s) {
                            if recent.items.contains(where: { $0.key == "|| " + s.description }) { // TODO: handle tense correctly
                                // TODO: do an appropriate check to make sure preconditions have been met
                                
                                // check if it applies based on the temporal window
                                // TODO: determine if it is =/>, <=/> or none
                                
                                let c = tv.c / (tv.c + k) // new confidence score
                                let observed = Judgement(st, TruthValue(tv.f, c), tense: judgement.tense, timestamp: judgement.timestamp)
                                
                                if concept.term == p { // isPredicate
                                    if let subject = memory.get(s.description) { // get subject concept
                                        subject.accept(observed, derive: false)
                                        memory.put(subject)
                                    }
                                    concept.accept(observed, derive: false)
                                }
                            }
                        }
                    }
                }
                memory.put(concept)
            }
            //
            // store anticipations for later access
            for ant in memory.peek(judgement.statement.description)?.anticipations(for: judgement) ?? [:] {
                let st = ant.value.0
                if case .statement(_, _, let p) = st {
                    if var pc = memory.get(p.description) {
                        pc.anticipations[ant.key] = ant.value
                        memory.put(pc)
                    }
                    
                    output(".  ⏱ anticipate " + "<\(p)>.") // TODO: add deduction calculation
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
        
        var stable: [Judgement] = []
        
        while let b = recent.get() {
            derived.append(b)
            var j = j
            if j.timestamp == 0 {
                j = .updateTimestamp(j, timeProviderMs)
            }
            // only process direct experiences
            if b.judgement.truthValue.rule == nil, // TODO: MAYBE: remove condition?
               b.judgement.timestamp != ETERNAL, j.timestamp != ETERNAL {
                
                // process temporal
                Rules.allCases.flatMap { rs in
                    rs.variable_and_temporal.flatMap { r in
                        [rule_applicator(r)((j, b.judgement)),
                         rule_applicator(r)((b.judgement, j))] // switch order of premises
                    }
                }.forEach {
                    if var el = $0 {
                        // set time stamp if not yet set
                        if el.timestamp == 0 {
                            el = .updateTimestamp(el, timeProviderMs)
                        }
                        
                        // chooseBestAndAppend
                        if let d = derived.first(where: { $0.judgement.identifier == el.identifier }) {
                            el = choice(j1: d.judgement, j2: el)
                        }
                        derived.append(el + 0.9)
                        
                        // add to main memory
                        // TODO: figure out how to accomplish evidence accumulation
                        // because as it stands, there is evidence overlap
                        // so choice rule will be used instead of revision
                        // TODO: QUESTION: is this evidence overlap still happening?
                        stable.append(el)
                    }
                }
            }
            
            if b.judgement.statement != j.statement && !variants.contains(where: { $0.statement == b.judgement.statement }) {
                
                Rules.allCases.flatMap { r in
                    r.apply((b.judgement, j)) +
                    variants.flatMap({ r.apply((b.judgement, $0)) })
                }.forEach {
                    if var el = $0 {
                        if el.timestamp == 0 {
                            el = .updateTimestamp(el, timeProviderMs)
                        }
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
    ///   - label: `.` prefix to use in output
    fileprivate func process(_ input: Sentence, recurse: Bool = true, label: String = ".") {
        let label = label + (recurse ? "" : "  ⏱") + " "
        
        // set time stamp if not yet set
        let input = input.setTimestamp(timeProviderMs)
        
        output(label + "\(input)")
        
        // process in memory
        var derived = memory.consider(input, derive: recurse)
        
        //        print("processed \(input)\n\tderived \(derived)")
        
        /*
         * MAIN
         */
        switch input {
            
        case .judgement:
            // clean up duplicates and tautologies
            /// TODO: filter open compound terms: Definition 10.4
            derived = derived.remove(matching: input)
            derivedBuffer.enqueue(derived)
            
        case .goal(let g): // TODO: take desireValue into account
            var accomplished = false
            
            for d in derived { // process statements
                if case .statement(let s, let c, let p) = d.statement,
                   /*case .operation(let op, let args) = s,*/ c == .predictiveImp, p == g.statement {
                    if case .operation(let op, let args) = s {
                        output(label + "🤖 \(s)")
                        accomplished = true
                        if let operation = operations[op] {
                            let result = operation(args) // execute
                            output(result.description)
                        } else {
                            output("Unknown operation \(op)")
                        }
                    } else if case .statement(_, let opc, let opp) = s,
                              opc == .predictiveImp { // need to verify the the other term?
                        if case .operation(let op, let args) = opp {
                            output(label + "🤖 \(opp)")
                            accomplished = true
                            if let operation = operations[op] {
                                let result = operation(args) // execute
                                output(result.description)
                            } else {
                                output("Unknown operation \(op)")
                            }
                        }
                    } else {
                        let qs = Question("?" >>|=> s)
                        if derivedQuestions[qs.statement] == nil {
                            derivedQuestions[qs.statement] = (d, d.truthValue.rule ?? .deduction)
                            derivedBuffer.insert(contentsOf: [input, .question(qs)], at: 0)
                        }
                    }
                }
            }
            
            if !accomplished {
                // re-process goal
                derivedBuffer.insert(input, at: 0) // TODO: need to only do this if goal has not been achieved yet
            }
            
        case .question(let question):
            // consider a question
            if case .statement = question.statement, !derived.isEmpty {
                if let winner = derived.first(where: { $0.statement == question.statement }) {
                    
                    // cancel in-flight activities
                    derivedBuffer.cleanup(winner.statement)
                    
                    if let (source, rule) = derivedQuestions[winner.statement] {
                        derivedQuestions.removeValue(forKey: winner.statement)
                        
                        let answers = rule.apply((source, winner)) .compactMap {$0} .map {Sentence($0)}
                        derivedBuffer.append(contentsOf: answers)
                    }
                    
                    output("• 💡 \(winner)")
                    print("}}", winner.derivationPath)
                    
                    derivedBuffer.append(.judgement(winner))
                    
                } else if let winner = derived.first(where: {
                    let match = Term.logic_match(t1: $0.statement, t2: question.statement)
                    if match {
                        let request = Term.getTerms(question.statement).filter({ $0 != "º" && !$0.description.hasPrefix("?") })
                        let result = Term.getTerms($0.statement)
                        return request.allSatisfy({result.contains($0)})
                    }
                    return match
                }) {
                    // cancel in-flight activities
                    derivedBuffer.cleanup(question.statement)
                    
                    if let (source, rule) = derivedQuestions[question.statement] {
                        derivedQuestions.removeValue(forKey: question.statement)
                        
                        let answers = rule.apply((source, winner)) .compactMap {$0} .map {Sentence($0)}
                        
                        if case .goal(let goal) = derivedBuffer.last {
                            if case .judgement(let jud) = answers.first {
                                if case .statement(let sub, let cop, let pre) = jud.statement {
                                    if cop == .predictiveImp, pre == goal.statement {
                                        // subject is the subgoal
                                        if case .statement(_, let c, let p) = sub {
                                            // TODO: how to check preconditions?
                                            if c == .predictiveImp { // TODO: `s` precondition need to be checked
                                                if case .operation(let op, let ts) = p {
                                                    _ = derivedBuffer.removeLast()
                                                    
                                                    output(label + "🤖 \(p)")
                                                    if let operation = operations[op] {
                                                        let result = operation(ts) // execute
                                                        output(result.description)
                                                    } else {
                                                        output("Unknown operation \(op)")
                                                    }
                                                }
                                            }
                                        }
                                        
                                        return // EXIT
                                    }
                                }
                            }
                        }
                        
                        derivedBuffer.append(contentsOf: answers)
                    }
                    
                    output("• 💡 \(winner)")
                    print("}}", winner.derivationPath)
                    
                    derivedBuffer.append(.judgement(winner))
                    
                    if case .statement(let s, let c, let p) = winner.statement, c == .inheritance {
                        if case .compound(let con, let terms) = p, con == .e, terms.count > 2, terms.first == .represent {
                            let op = terms.filter {
                                if case .operation = $0 {
                                    return true
                                }
                                return false
                            }
                            
                            if op.isEmpty {
                                let relation = terms[2] // kiu -> [dormas]
                                let followUp = relation --> ç.e_(.represent, .º, "?")
                                derivedBuffer.append(.question(.init(followUp)))
                                
                            } else {
                                for o in op {
                                    if case .operation(let op, let args) = o {
                                        output(label + "🤖 \(s)")
                                        if let operation = operations[op] {
                                            let result = operation(args) // execute
                                            output(result.description)
                                        } else {
                                            output("Unknown operation \(op)")
                                        }
                                    }
                                }
                            }
                        }
                    }
                    
                } else {
                    /*
                     * IMAGINATION -- derived questions
                     */
                    // maybe?
                    var buff: [Sentence] = []
                    for var chunk in derived.split(separator: .NULL-*) {
                        let source = chunk.removeFirst()
                        let qs: [Sentence] = chunk.reversed().flatMap { j -> [Sentence] in
                            if derivedQuestions[j.statement] == nil {
                                derivedQuestions[j.statement] = (source, j.truthValue.rule ?? .deduction)
                                return [.question(question), .question(.init(j.statement))]
                            } else { // no idea; take a guess
                                let guess = Judgement(j.statement, .guess, j.derivationPath, tense: j.tense, timestamp: j.timestamp)
                                return [.question(question), .judgement(guess)]
                            }
                        }
                        buff.append(contentsOf: qs)
                    }
                    
                    derivedBuffer.insert(contentsOf: buff, at: 0)
                    
                    output("\tI don't know 🤷‍♂️")
                    // TODO: additionally process sentence *["SELF", question.statement] --> -("[know]")
                    
                }
            } else {
                //output("\t(3)I don't know 🤷‍♂️")
                derivedBuffer.insert(input, at: 0)
            }
            
            /// CYCLE
            
        case .cycle(let n):
            for _ in 0 ..< n {
                if let s = derivedBuffer.popLast() {
                    process(s, recurse: false)
                } else {
                    mainCycle()
                }
            }
            output("Completed \(n) cycles")
        }
    }
    
    
    // MARK: Cycles
    
    private func mainCycle() {
        // TODO: potentially get items from recent memory and process them in imagination
        //        if let r = recent.get() {
        //            recent.put(r)
        //            self.process(.judgement(b.judgement))
        //        }
        
        if let c = memory.peek(), let b = c.beliefs.peek() {
            
            let immediate = Rules.immediate(b.judgement)
            let structural = Theorems.apply(b.judgement)
            
            let results = (immediate + structural)
            
            results.forEach { j in
                process(.judgement(j), recurse: true)
            }
        }
    }
}
