
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
    
    public internal(set) var buffer = Bag<Task>(4,40) // TODO: need to use actual buffer not just a bag

    private var recentInput: [Judgement] = []

    
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
    
    private let timeProviderMs: () -> UInt32
    
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
    
    public func perform(_ script: Sentence...) { // blocking
        perform(script)
    }

    private func cycle() {
        guard var concept = memory.get() else {
            return // no concepts in memory
        }
        
        // print("cycle", concept.term.description)
        let results = concept.cycle()
                    
        if let derived = results["Judgement"] {
            concept.adjustPriority(derived)
            
            for j in derived {
                buffer.put(Task(sentence: .judgement(j)))
            }
        }
        
        if let answers = results["Question"] {
            
            if answers.first?.statement == .NULL {

                // TODO: finish this !!!
                
//                for chunk in answers.split(separator: .NULL-*) {
//                    chunk.reversed().forEach { j in
//                        let q1 = Sentence.question(Question(j.statement))
//                        buffer.put(Task(sentence: q1))
//                    }
//                }
                
            } else if let a = answers.first {
                output(". 💡 \(a)")
                
                if case .statement(let sub, let cop, let pre) = a.statement {
                    if cop == .predictiveImp, pre == "G" { // TODO: needs to be done properly
                        // subject is the subgoal
                        if case .statement(_, let c, let p) = sub {
                            // TODO: how to check preconditions?
                            if c == .predictiveImp { // TODO: `s` precondition need to be checked
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
                        }
                    }
                }
            }

        }
        
        memory.put(concept)
    }
        
    public func perform(_ script: [Sentence]) { // blocking
        for s in script {
            
            if case .cycle(let n) = s {
                for _ in 0..<n {

                    cycle()
                    
                    if let t = buffer.get() {
                        output(". ⏱ \(t.sentence)") // task
                               
                        let revised = memory.consider(t.sentence, derive: false)

                        for r in revised {
                            output(". ⏱ \(r)") // revision
                        }
                    }
                }
            }
                        
            // set time stamp if not yet set
            let s = s.setTimestamp(timeProviderMs)

            
            if case .judgement(let j) = s {
                output("• \(s)")

                recentInput.append(j)
                
                for j in process(recent: j) {
                    // add stable patterns from recent memory
                    buffer.put(Task(sentence: .judgement(j)))
                }

                let revised = memory.consider(s, derive: true)
                
                for r in revised {
                    if r.statement == j.statement {
                        output(". ⏱ \(r)") // revision
                    } else {
                        // local inference
                        buffer.put(Task(sentence: .judgement(r)))
                    }
                }
            }
            
            if case .question(let question) = s {
                output("• \(s)")

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
                    
                    for chunk in answers.split(separator: .NULL-*) {
                        chunk.reversed().forEach { j in
                            let q1 = Sentence.question(Question(j.statement))
                            buffer.put(Task(sentence: q1))
                        }
                    }
                    
                } else if let a = answers.first {
                    output(". 💡 \(a)")
                }
            }
            
            if case .goal(let g) = s { // TODO: take desireValue into account
                let derived = memory.consider(s, derive: true)
                
                for d in derived { // process statements
                    if case .statement(let s, let c, let p) = d.statement,
                       /*case .operation(let op, let args) = s,*/ c == .predictiveImp, p == g.statement {
                        if case .operation(let op, let args) = s {
                            output(". 🤖 \(s)")
                            if let operation = operations[op] {
                                let result = operation(args) // execute
                                output(result.description)
                            } else {
                                output("Unknown operation \(op)")
                            }
                        } else if case .statement(_, let opc, let opp) = s,
                                  opc == .predictiveImp { // need to verify the the other term?
                            if case .operation(let op, let args) = opp {
                                output(". 🤖 \(opp)")
                                if let operation = operations[op] {
                                    let result = operation(args) // execute
                                    output(result.description)
                                } else {
                                    output("Unknown operation \(op)")
                                }
                            }
                        } else {
                            let qs = Question("?" >>|=> s)
                            buffer.put(Task(sentence: .question(qs)))
                        }
                    }
                }
            }
            
            // ANTICIPATIONS
            
            process(anticipations: s)
        }
    }
    
    public func perform(_ statement: Statement) { // convenience
        perform(statement-*)
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
}
