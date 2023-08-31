
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
    
    public var recent = Bag<Belief>(4,40) // TODO: use tense and therefore identifier for indexing
    public internal(set) var memory = Bag<Concept>()
    
    public internal(set) var buffer = Bag<Task>(1,20) // TODO: need to use actual buffer not just a bag

    public var recentInput: [Judgement] = [] // TODO: needs to be handled properly

    
    // TODO: rename to stdout and add stderr
    public var output: (String) -> Void
    

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
    
    public let timeProviderMs: () -> UInt32
    
    public init(timeProviderMs: @escaping () -> UInt32, _ output: @escaping (String) -> Void = { print($0) }) {
        self.output = output
        self.timeProviderMs = timeProviderMs
        
        Theorems.cache.removeAll() // TODO: temporary workaround, needs better implementation
    }
    
    public func reset() {
        Theorems.cache.removeAll() // TODO: temporary workaround, needs better implementation
        
        memory = Bag<Concept>()
        recent = Bag<Belief>(4,40)
        buffer = Bag<Task>(4,40)
        
        recentInput.removeAll()
    }
    
    func forward() {
        if let t = buffer.get() {
            output("\(t.sentence)") // task
            
            if case .question(let q) = t.sentence {
                let answers = memory.consider(t.sentence, derive: true)

                if answers.first?.statement == .NULL {
                    for var chunk in answers.split(separator: .NULL-*) {
                        let source = chunk.removeFirst()
                        chunk.reversed().forEach { j in
                            let q1 = Sentence.question(Question(j.statement, source))
                            buffer.put(Task(sentence: q1))
                        }
                    }
                    
                } else if let a = answers.first {
                    output(". 💡 \(a)")
                }
            }
            
            if case .judgement(let j) = t.sentence {
                
                if case .operation(let op, let ts) = j.statement {
                    _ = operations[op]?(ts)
                }
                
                if case .statement(_, let p, let c) = j.statement {
                    if case .operation(let op, let ts) = c, p == .predictiveImp {
                        _ = operations[op]?(ts)
                    }
                }
                
                // pre-process task
                var results = memory.consider(t.sentence, derive: true)

//                    results += nars.process(recent: j)
                
                if let last = recentInput.last, last.timestamp != ETERNAL && j.timestamp != ETERNAL {
                    let prediction: Judgement = (last.statement >>|=> j.statement)-*
                    if prediction.statement.complexity < 20 {
                        results.append(prediction)
                    }
                    recentInput.append(j)
                }
                
                // apply rules
                results += Set(j.statement.terms)
                    .map {
                        memory.peek($0.description) ?? Concept(term: $0)
                    }
                    .compactMap {
                        $0.beliefs.peek() ?? (j + 0.9)
                    }
                    .filter {
                        $0.judgement.statement != j.statement
                    }
                    .map {
                        // forward pass
                        fforward((t, $0))
                    }
                    .flatMap {$0}
                
                // add derived to buffer
                results.removeDuplicates().filter { ($0.truthValue.confidence > 0.1) && $0.statement.complexity < 20 }.forEach {
                    buffer.put(Task(sentence: .judgement($0)))
                }
            }
        }
        else if let c = memory.peek(),
                var t = c.tasks.peek() {
            buffer.put(t)
        }
    }
    
    private func fforward(_ pair: (Task, Belief)) -> [Judgement] {
        var (t, b) = pair
        
        guard case .judgement(let j) = t.sentence,
              !b.judgement.evidenceOverlap(j) else {
            return []
        }
        
        var derived: [Judgement] = []
        // apply rules
        let results = Rules.allCases
            .flatMap { r in
                r.apply((b.judgement, j))
            }
            .compactMap { $0 }
            .removeDuplicates()
        
        derived.append(contentsOf: results)
        
        derived = derived.filter { $0.truthValue.confidence != 0 }
        
        return derived
    }

    
    public func perform(_ statement: Statement) { // convenience
        perform(statement-*)
    }
    
    public func perform(_ script: Sentence...) { // blocking
        perform(script)
    }

    public func perform(_ script: [Sentence]) { // blocking
        for s in script {
            
            if case .cycle(let n) = s {
                for _ in 0..<n {
//                    forward()
                    cycle()

                    if let t = buffer.get() {
                        output(". ⏱ \(t.sentence)") // task

                        memory.consider(t.sentence).forEach { revised in
                            output(". ⏱ \(revised)") // revision
                        }
                    }
                }
                
                //                output(". ⏱ \(s)")
                continue
                
//            } else {
//
//                buffer.put(Task(sentence: s))
//                //            print(||s)
//                continue
            }
                        
            // set time stamp if not yet set
            let s = s.setTimestamp(timeProviderMs)

            output("• \(s)")

            if case .judgement(let j) = s {
                
                process(recent: j).forEach { stable in
                    // add stable patterns from recent memory
                    buffer.put(Task(sentence: .judgement(stable)))
                }
                
                process(anticipations: s)

                memory.consider(s, derive: true).forEach { derived in
                    if derived.statement == j.statement {
                        output(". ⏱ \(derived)") // revision
                    } else {
                        // add to buffer for later processing
                        buffer.put(Task(sentence: .judgement(derived)))
                    }
                }
            }
            
            if case .question(let question) = s {

                if case .statement(let sub, _, _) = question.statement {
                    // check recent memory first
                    if let answer = recent.peek(question.identifier)?.judgement {
                        // check main memory if the answer is already present
                        let c = memory.items[sub.description]
                        if c == nil || c!.beliefs.items.contains(where: { $0.value.judgement.statement == answer.statement }) == false {
                            /// ANSWER
                            buffer.put(Task(sentence: .judgement(answer)))
                            output(". 💡 \(answer)")
                        }
                    }
                }

                let answers = memory.consider(s, derive: true)

                if answers.first?.statement == .NULL {
                    for var chunk in answers.split(separator: .NULL-*) {
                        let source = chunk.removeFirst()
                        chunk.reversed().forEach { j in
                            let q1 = Sentence.question(Question(j.statement, source))
                            buffer.put(Task(sentence: q1))
                        }
                    }
                    
                } else if let a = answers.first {
                    output(". 💡 \(a)")
                }
            }
            
            if case .goal(let g) = s { // TODO: take desireValue into account
                let derived = memory.consider(s, derive: true)
                print(derived)
                for d in derived { // process statements
                    if case .statement(let s, let c, let p) = d.statement,
                       /*case .operation(let op, let args) = s,*/ c == .predictiveImp, p == g.statement {
                        if case .operation(let op, let args) = s {
                            if recentInput.suffix(20).contains(where: { $0.statement == s} ) {
                                output(". 🤖 \(s)")
                                if let operation = operations[op] {
                                    let result = operation(args) // execute
                                    output(result.description)
                                } else {
                                    output("Unknown operation \(op)")
                                }
                            }
                        } else if case .statement(let ops, let opc, let opp) = s,
                                  opc == .predictiveImp { // need to verify the the other term?
                            if case .operation(let op, let args) = opp {
                                if recentInput.suffix(10).contains(where: {$0.statement == ops}) {
                                    output(". 🤖 \(opp)")
                                    if let operation = operations[op] {
                                        let result = operation(args) // execute
                                        output(result.description)
                                    } else {
                                        output("Unknown operation \(op)")
                                    }
                                }
                            }
                        } else {
//                            let qs = Question("?" >>|=> s)
//                            buffer.put(Task(sentence: .question(qs)))
                        }
                    }
                }
            }
            
        }
    }
    
    // MARK: - CYCLE
    
    private func cycle() {
        guard var concept = memory.get() else {
            return // no concepts in memory
        }
        
        
        var results: [String: [Judgement]] = concept.cycle()
        
//        let inputs = concept.pickOut()
//
//        let outputs = concept.forward(inputs) // GPT-N
//
//        outputs.forEach({ buffer.put(Task(sentence: .judgement($0))) })
//
//        results["Judgement"] = outputs
        
        if let derived = results["Judgement"] {            
            for j in derived {
                buffer.put(Task(sentence: .judgement(j)))
            }
        }
        
        if let answers = results["Question"] {

            if answers.first?.statement == .NULL {

                // TODO: finish this !!!
                
                for var chunk in answers.split(separator: .NULL-*) {
                    let source = chunk.removeFirst()
                    chunk.reversed().forEach { j in
                        let q1 = Sentence.question(Question(j.statement, source))
                        buffer.put(Task(sentence: q1))
                    }
                }
                
            } else {
                for a in answers {
                    output(". 💡 \(a)")
                }
            }

        }
        
        if let answers = results["Goal"] {

//            print("A\n", answers, "\n")
            if let a = answers.first {
                if case .statement(let sub, let cop, _) = a.statement {
                    if cop == .predictiveImp { // TODO: needs to be done properly
                        // subject is the subgoal
                        if case .statement(let s, let c, let p) = sub {
                            // TODO: how to check preconditions?
                            if c == .predictiveImp { // TODO: `s` precondition need to be checked
                                if recentInput.suffix(20).contains(where: { $0.statement == s } ) {
                                    if case .operation(let op, let ts) = p {
                                        output(". 🤖 \(p)")
                                        if let operation = operations[op] {
                                            let result = operation(ts) // execute
                                            output(result.description)
                                        } else {
                                            output("Unknown operation \(op)")
                                        }
                                    }
                                }
                            } else {
                                let gs = Goal(sub)
                                buffer.put(Task(sentence: .goal(gs)))
                            }
                        }
                    }
                }
            }
        }
        
        if results.flatMap({$0.value}).isEmpty {
            // decrease priority
            concept.priority = max(concept.priority - 0.01, 0.01)
        } else {
            // increase priority
            concept.priority = min(concept.priority + 0.01, 0.99)
        }

        // storage
        memory.put(concept)
    }
        
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
                                        if let revised = subject.accept(belief: observed) {
                                            output(". ⏱ \(revised)")
                                        }
                                        memory.put(subject)
                                    }
                                    if let revised = concept.accept(belief: observed) {
                                        output(". ⏱ \(revised)")
                                    }
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
    
    public func process(recent j: Judgement) -> [Judgement] {
        guard recent.peek(j.identifier) == nil else {
            return []// no need to process what we already know
        }
        
        recentInput.append(j)
        
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
}
