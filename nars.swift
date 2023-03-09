/*
public protocol Item: Equatable {
    var identifier: String { get }
    var priority: Double { get set }
}

public struct TermLink: Item {
    public var identifier: String { term.description }
    public var priority: Double = 0.9
    public let term: Term
}

public struct Belief: Item {
    public var identifier: String { judgement.identifier }
    public var priority: Double = 0.9
    public let judgement: Judgement
}

//public struct Task: Item {
//    public var identifier: String { sentence.description }
//    public var priority: Double = 0.9
//    public let sentence: Sentence
//}

// MARK: Concept

public struct Concept: Item {
    public var identifier: String { term.description }
    public var priority: Double = 0.9
    
    let term: Term
    
    private var _termLinks = Bag<TermLink>()
    internal var termLinks: WrappedBag<TermLink>
    //let tasks = Bag<TermLink>() // sentences
    private var _beliefs = Bag<Belief>()
    internal var beliefs: WrappedBag<Belief>

    init(term: Term) {
        self.term = term
        self.termLinks = WrappedBag(_termLinks)
        self.beliefs = WrappedBag(_beliefs)
    }
    
    // TODO: how much should the input change
    // before it is considered different?
    // for how long should we keep the cache?
    // after n seconds or instances of the same input
    // should we still permit the signal to go through?
    // implementing a debounce of sorts
//    internal var lastInput: Judgement!
//    internal var lastAccepted: Set<Judgement> = []
//    internal var lastQuestion: Question!
//    internal var lastAnswered: Set<Judgement> = []
}

extension Concept {
    // returns derived judgements if any
    func accept(_ j: Judgement, isSubject: Bool = true, derive: Bool) -> [Judgement] {
//        if j == lastInput { return Array(lastAccepted) }
//        lastInput = j

        var originalPriority: Double?
        
        var derived: [Judgement] = []

        var j = j
        
        // revision goes first
        if let b = beliefs.get(j.identifier) {
            originalPriority = b.priority
            var judgement: Judgement
            if j.evidenceOverlap(b.judgement) {
                judgement = choice(j1: j, j2: b.judgement)
            } else {
                if j.truthValue.rule == .conversion {
                    judgement = b.judgement
                } else if b.judgement.truthValue.rule == .conversion {
                    judgement = j
                } else {
                    judgement = revision(j1: j, j2: b.judgement)
                }
            }
            // wait to put back original belief to process another one
            if j != judgement {
                j = judgement
                derived.append(judgement)
            }
        }
        
        var jflipped: Judgement = j
        // store symmetrical statement
        if case .statement(let sub, let cop, let pre) = j.statement, (cop == .equivalence || cop == .similarity) {
            let flipped: Statement = .statement(pre, cop, sub)
            jflipped = Judgement(flipped, j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp)
            if beliefs.peek(jflipped.identifier) == nil {
                derived.append(jflipped)
            }
        }
        
        // store symmetrical compound
        if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
            if terms.count == 2 { // TODO: handle compounds with multiple terms
                let flipped: Statement = .compound(conn, terms.reversed())
                jflipped = Judgement(flipped, j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp)
                if beliefs.peek(jflipped.identifier) == nil {
                    derived.append(jflipped)
                }
            }
        }
        
        defer {
            switch j.statement {
            case .symbol: // TODO: is this accurate?
                termLinks.put(TermLink(j.statement, 0.9))
            case .compound(let c, _):
                if ![.c, .d, .n].contains(c) {
                    termLinks.put(TermLink(j.statement, 0.9))
                }
            case .statement(let subject, _, let predicate):
                if !j.statement.isTautology {
                    let term = isSubject ? predicate : subject
                    termLinks.put(TermLink(term, 0.9))
                }
            case .variable:
                break // TODO: is this accurate?
            case .operation:
                break // TODO: is this accurate?
            }

            var b = j + (originalPriority ?? 0.9)
            b.adjustPriority(derived)
//            if j.truthValue.rule != .conversion {
                beliefs.put(b) // store new belief
//            }
        }
        
        // return if no recursion
        guard derive else { return derived }
        
        /// apply two-premise rules
        twoPremiseRules:
        if var b = beliefs.get() {
            
            if b.judgement.statement == jflipped.statement {
                if let b1 = beliefs.get() {
                    beliefs.put(b)
                    b = b1 // use another belief
                } else {
                    beliefs.put(b)
                    break twoPremiseRules
                }
            }
            
            // apply rules
            let results = Rules.allCases
                .flatMap { r in
                    r.apply((b.judgement, j))
                }
                .compactMap { $0 }
            
            derived.append(contentsOf: results)
            
            // TODO: wait to put back
            // modify its "usefullness" value
            b.adjustPriority(results)
            beliefs.put(b) // put back another belief

//            lastAccepted = Set(derived)
            if !derived.isEmpty {
//                print("because...")
//                print("+++", j, "\n", "&&", b)
//                print("it follows...")
            }
        }
        
        derived = derived.removeDuplicates().filter {
            beliefs.peek($0.identifier) == nil
        }//&& $0.statement != j.statement }

        // TODO: process `values`
        // like rules but modifiable by the system
        // statements using variables
        
        return derived
    }
    
    
    //
    // TODO: account for tense in question answering
    //
    
    
    // returns relevant belief or derived judgements if any
    func answer(_ q: Question) -> [Judgement] {
        var result: [Judgement] = []
        switch q.statement {
        case .statement(let subject, let copula, let predicate):
            if case .variable(let v) = subject {
                if case .query = v {
                    // special
                    result = answer { s in
                        switch s {
                        case .symbol: fallthrough // TODO: is this accurate?
                        case .compound:
                            return predicate == s
                        case .statement(_, let c, let p):
                            return copula == c && predicate == p
                        case .variable:
                            return false // TODO: is this accurate?
                        case .operation:
                            return false // TODO: is this accurate?
                        }
                    }
                } // TODO: handle other cases
            } else if case .variable(let v) = predicate {
                if case .query = v {
                    // general
                    result = answer { s in
                        switch s {
                        case .symbol: fallthrough // TODO: is this accurate?
                        case .compound:
                            return subject == s
                        case .statement(let s, let c, _):
                            return subject == s && copula == c
                        case .variable:
                            return false // TODO: is this accurate?
                        case .operation:
                            return false // TODO: is this accurate?
                        }
                    }
                }
            } else { // TODO: handle other cases 
                result = answer(q.statement)
                result = result.removeDuplicates()
            }
        default:
            return [] // TODO: handle other cases
        }
//        if q == lastQuestion &&
//            Set(result) == lastAnswered {
//            return []
//        }
//        lastQuestion = q
//        lastAnswered = Set(result)
        return result
    }
    
    // MARK: Private
    
    private func answer(_ s: Statement) -> [Judgement] {
        if let b = beliefs.get(s.description) {
            beliefs.put(b) // put back
            return [b.judgement]
        } else if let c = conversion(j1: s-*), let b = beliefs.get(c.statement.description) {
            beliefs.put(b) // put back
            let conv = conversion(j1: b.judgement)!
            beliefs.put(conv + 0.9)
            return [conv]
            
        } else if let b = beliefs.get() {
            beliefs.put(b) // put back
            // all other rules // backwards inference
            
            let r = Theorems.apply(b.judgement)
                .filter { beliefs.peek($0.description) == nil }

            if let answer = r.first(where: { $0.statement == s }) {
                return [answer]
            }
            
            return r +
             Rules.allCases
                .flatMap { r in
                    r.apply((s-*, b.judgement))
                }
                .compactMap { $0 }
        }
        return [] // no results found
    }
    
    private func answer(_ f: (Statement) -> Bool) -> [Judgement] {
        let winner = beliefs.items
            .filter { b in
                f(b.value.judgement.statement)
            }.map { b in
                b.value.judgement
            }.max { j1, j2 in
                let c = choice(j1: j1, j2: j2)
                return c.statement == j2.statement
            }
        return winner == nil ? [] : [winner!]
    }
}

extension AbstractBag where I == Concept {
    func consider(_ s: Sentence, derive: Bool) -> [Judgement] {
        switch s {
        case .judgement(let j): return consider(j, derive: derive)
        case .goal: return [] // TODO: finish implementation
        case .question(let q): return consider(q, derive: derive)
        case .cycle: return []
        }
    }
    func consider(_ j: Judgement, derive: Bool) -> [Judgement] {
        consider(j.statement, derive: derive) { c in
            switch j.statement {
            case .symbol: fallthrough // TODO: is this accurate?
            case .compound:
                return c.accept(j, isSubject: c.term == j.statement, derive: derive)
            case .statement(let subject, _, _):
                return c.accept(j, isSubject: c.term == subject, derive: derive)
            case .variable:
                return [] // TODO: is this accurate?
            case .operation:
                return [] // TODO: is this accurate?
            }
        }
    }
    func consider(_ q: Question, derive: Bool) -> [Judgement] {
        if case .statement = q.statement {
            return consider(q.statement, derive: derive) { c in c.answer(q) }
        } else {
            return considerVar(q.variableTerm, derive: derive) { c in c.answer(q) }
        }
    }
}

// MARK: Private

extension AbstractBag where I == Concept {
    private func consider(_ s: Statement, derive: Bool, _ f: (inout Concept) -> [Judgement]) -> [Judgement] {
        var derivedJudgements = [Judgement]()
        // TODO: consider overall concept
        // let overallConcept = get(s.description) ?? Concept(term: s)
        switch s {
        case .symbol: // TODO: is this accurate?
            var concept = get(s.description) ?? Concept(term: s)
            derivedJudgements.append(contentsOf: f(&concept))
            concept.adjustPriority(derivedJudgements)
            put(concept)
            return derivedJudgements
        case .compound(let c, let ts):
//            if c == .n, ts.count == 1 { // TODO: is this correct?
//                return consider(ts[0], derive: derive, f)
//            }
            if [.c, .d].contains(c) {
                let terms = Set(ts.flatMap{$0.terms})
                for t in terms {
                    if var concept = get(t.description) {
                        derivedJudgements.append(contentsOf: f(&concept))
                        concept.adjustPriority(derivedJudgements)
                        put(concept)
                    }
                }
                return derivedJudgements
            }
            var concept = get(s.description) ?? Concept(term: s)
            derivedJudgements.append(contentsOf: f(&concept))
            concept.adjustPriority(derivedJudgements)
            put(concept)
            return derivedJudgements
        case .statement(let subject, _, let predicate):
            var subjectConcept = get(subject.description) ?? Concept(term: subject)
            var predicateConcept = get(predicate.description) ?? Concept(term: predicate)
            derivedJudgements.append(contentsOf: f(&subjectConcept))
            subjectConcept.adjustPriority(derivedJudgements)
            derivedJudgements.append(contentsOf: f(&predicateConcept))
            predicateConcept.adjustPriority(derivedJudgements)
            if case .statement = subject {
                derivedJudgements.append(contentsOf: consider(subject, derive: derive, f))
            }
            if case .statement = predicate {
                derivedJudgements.append(contentsOf: consider(predicate, derive: derive, f))
            }
            put(subjectConcept)
            put(predicateConcept)
            return derivedJudgements
        case .variable:
            return [] // TODO: is this accurate?
        case .operation:
            return [] // TODO: is this accurate?
        }
    }

    private func considerVar(_ t: Term, derive: Bool, _ f: (inout Concept) -> [Judgement]) -> [Judgement] {
        guard var concept = get(t.description) else { return [] }
        defer { put(concept) } // put back
        return f(&concept)
    }
    
    func contains(_ j: Judgement) -> Bool {
        let identifier = j.identifier
        switch j.statement {
        case .symbol(let word):
            if let c = peek(word) {
                return c.beliefs.peek(identifier) != nil
            }
        case .compound(let c, let ts):
            if c == .n, ts.count == 1 {
                return contains(ts[0]-*)
            }
//            return !ts.map { contains($0-*) }.contains(false) // TODO: finish implementation
        case .statement(let s, _, let p):
            if let sc = peek(s.description), let pc = peek(p.description) {
                return sc.beliefs.peek(identifier) != nil && pc.beliefs.peek(identifier) != nil
            }
        case .variable(_):
            return false // TODO: finish implementation
        case .operation(_, _):
            return false // TODO: finish implementation
        }
        return false
    }
}

//import Dispatch

public protocol AbstractBag {
    associatedtype I: Item
    @discardableResult
    func put(_ item: I) -> I?
    func get() -> I?
    func get(_ identifier: String) -> I?
    func peek() -> I?
    func peek(_ identifier: String) -> I?
}


//class SyncQueue {
//    var writing = false
//    func sync<I>(_ block: () -> I) -> I {
//        while writing { /*wait*/ }
//        writing = true
//        defer { writing = false }
//        return block()
//    }
//    func sync(_ block: () -> Void) {
//        while writing { /*wait*/ }
//        writing = true
//        defer { writing = false }
//        block()
//    }
//}

public final class Bag<I: Item>: AbstractBag {
    var buckets: [[I]]
    var items: [String: I] = [:]
    
    internal let levels: Int
    internal let capacity: Int
    internal var currentLevel: Int
    
//    internal let queue = SyncQueue()//DispatchQueue(label: "ioqueue", qos: .background)
    
    public init(_ levels: Int = 100, _ capacity: Int = 10000) {
        buckets = Array(repeating: [], count: levels)
        self.levels = levels
        self.capacity = capacity
        self.currentLevel = levels - 1
    }
    
    @discardableResult
    public func put(_ item: I) -> I? {
//        queue.sync {
            var item = item
            let oldItem = items[item.identifier]
            if let oldItem = oldItem {
                item.priority = max(oldItem.priority, item.priority)
                removeFromBucket(oldItem)
            }
            items[item.identifier] = item
            return addToBucket(item)
//        }
    }
    
    public func get() -> I? {
//        queue.sync {
            if items.isEmpty {
                return nil
            }
            currentLevel = selectNonEmptyLevel()
            if buckets[currentLevel].isEmpty {
                return nil
            }
            let item = buckets[currentLevel].removeFirst()
            items.removeValue(forKey: item.identifier)
            return item
//        }
    }
    
    public func get(_ identifier: String) -> I? {
//        queue.sync {
            if let item = items[identifier] {
                removeFromBucket(item)
                items.removeValue(forKey: item.identifier)
                return item
            }
            return nil
//        }
    }
    
    public func peek() -> I? {
//        queue.sync {
            if items.isEmpty {
                return nil
            }
            currentLevel = selectNonEmptyLevel()
            return buckets[currentLevel].first
//        }
    }
    
    public func peek(_ identifier: String) -> I? {
//        queue.sync {
            return items[identifier]
//        }
    }

    private func getLevel(_ item: I) -> Int {
        let fl = item.priority * Double(buckets.count)
        let level = Int(fl.rounded(.down) - 1)
        return max(level, 0)
    }
    
    // TODO: add probabilistic distributor from OpenNARS
    private func selectNonEmptyLevel() -> Int {
        var selectedLevel = currentLevel
        var cache = Array(0..<levels)
        while buckets[selectedLevel].isEmpty && cache.count > 0 {
            /// https://stackoverflow.com/a/27541537
            let randomKey = Int.random(in: 0..<cache.count)
            selectedLevel = cache[randomKey]
            cache.swapAt(randomKey, cache.count - 1)
            cache.removeLast()
        }
        return selectedLevel
    }
    
    private func addToBucket(_ item: I) -> I? {
        var oldItem: I?
        if items.count > capacity {
            var level = 0
            while buckets[level].isEmpty, level < levels {
                level += 1
            }
            oldItem = buckets[level].removeFirst()
            if let removed = oldItem {
                items.removeValue(forKey: removed.identifier)
            }
        }
        let level = getLevel(item)
        buckets[level].append(item)
        return oldItem
    }
    
    private func removeFromBucket(_ item: I) {
        let level = getLevel(item)
        var items = buckets[level]
        items.removeAll(where: { $0.identifier == item.identifier })
        buckets[level] = items
    }
}


// MARK: - WrappedBag

/// Read access to wrapped Bag with writes to internal bag
public final class WrappedBag<I: Item>: AbstractBag {
    weak var wrapped: Bag<I>?
    var bag = Bag<I>()
    
//    internal var queue = SyncQueue()//DispatchQueue(label: "wrappedqueue-\(I.self)", qos: .background)

    init(_ bag: Bag<I>) {
        wrapped = bag
    }
    
    public func reset() {
//        queue.sync {
            bag = Bag<I>()
//        }
    }
    
    @discardableResult
    public func put(_ item: I) -> I? {
//        queue.sync {
            if item != wrapped?.peek(item.identifier) {
                bag.put(item) // items have diverged
            }
            return nil
//        }
    }
    
    public func get() -> I? {
//        queue.sync {
            bag.get() ?? wrapped?.peek()
//        }
    }
    
    public func get(_ identifier: String) -> I? {
//        queue.sync {
            bag.get(identifier) ?? wrapped?.peek(identifier)
//        }
    }
    
    public func peek() -> I? {
//        queue.sync {
            bag.peek() ?? wrapped?.peek()
//        }
    }
    
    public func peek(_ identifier: String) -> I? {
//        queue.sync {
            bag.peek(identifier) ?? wrapped?.peek(identifier)
//        }
    }
}
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

public final class NARS: Item {
    public var identifier: String { name.description }
    public var priority: Double = 0.9
    
    public var name = Term.symbol("SELF") // TODO: inject via init
    
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
                        
                        imaginationBuffer.removeAll()
                        
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
                    
                    let js: [Sentence] = derivedJudgements.reversed().flatMap({[.question(question), .judgement($0)]})
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


// convenience initializer for Belief
public func +(_ j: Judgement, p: Double) -> Belief {
    Belief(j, p)
}


public func -*(_ s: Statement, _ fc: (Double, Double)) -> Judgement {
    s -* (fc.0, fc.1, ETERNAL)
}
public func -*(_ s: Statement, _ fc: (Double, Double)) -> Sentence {
    Sentence(s -* fc)
}
public func -*(_ s: Statement, _ fct: (Double, Double, UInt32)) -> Sentence {
    Sentence(s -* fct)
}
public func -*(_ s: Statement, _ f: Double) -> Sentence {
    Sentence(s -* (f, 0.9))
}
public func -*(_ s: Statement, _ t: UInt32) -> Sentence {
    Sentence(s -* (1, 0.9, t))
}

extension Statement {
    public static postfix func -*(_ s: Statement) -> Sentence {
        Sentence(s-*)
    }
}


extension Sentence {
    public static prefix func <<(_ s: Sentence) -> Sentence {
        s.addTense(.past)
    }
    public static prefix func ||(_ s: Sentence) -> Sentence {
        s.addTense(.present)
    }
    public static prefix func >>(_ s: Sentence) -> Sentence {
        s.addTense(.future)
    }
    
    private func addTense(_ tense: Tense) -> Sentence {
        switch self {
        case .judgement(let j):
            return .judgement(Judgement(j.statement, j.truthValue, j.derivationPath, tense: tense, timestamp: j.timestamp))
        case .question(let q):
            return .question(Question(q.statement, q.type, tense))
        default:
            return self
        }
    }
}

postfix operator -?
extension Statement {
    public static postfix func -?(_ s: Statement) -> Question { Question(s) }
    public static postfix func -?(_ s: Statement) -> Sentence { Sentence(s-?) }
}

postfix operator -!
extension Statement {
    public static postfix func -!(_ s: Statement) -> Goal { Goal(s) }
    public static postfix func -!(_ s: Statement) -> Sentence { Sentence(s-!) }
}

extension Item {
    // TODO: this needs to be handled properly
    mutating func adjustPriority(_ derivedJudgements: [Judgement]) {
        if let maxPriority = derivedJudgements.map({$0.truthValue.confidence}).max() {
            let newPriority = (priority + maxPriority) / 2
            priority = min(newPriority, 0.9)
        }
    }
}
//import Dispatch

extension Judgement {
    static func updateTimestamp(_ j: Judgement, _ timeProvider: () -> UInt32) -> Judgement {
        var j = j
        let now = timeProvider()
        if j.derivationPath.count == 1 { // also update derivationPath
            return Judgement(j.statement, j.truthValue, tense: j.tense, timestamp: now)
        } else {
            j.timestamp = now
            return j
        }
    }
}


extension Question {
    public init(_ f: @autoclosure () -> Statement) {
        self.init(f(), Quest.truth, nil)
    }
    public var variableTerm: Term! {
        if case .statement(let s, _ , let p) = statement {
            if case .variable = s {
                return s
            } else if case .variable = p {
                return p
            } else {
                return nil
            }
        }
        return nil
    }
}

extension Goal {
    public init(_ f: @autoclosure () -> Statement) {
        let desireValue = DesireValue(1.0, 0.9) // TODO: what is the correct default?
        self.init(f(), desireValue)
    }
}

extension Sentence {
    public init(_ q: Question) {
        self = .question(q)
    }
    public init(_ j: Judgement) {
        self = .judgement(j)
    }
    public init(_ g: Goal) {
        self = .goal(g)
    }
}

extension TermLink {
    public init(_ term: Term, _ priority: Double) {
        self.term = term
        self.priority = priority
    }
}

extension Belief {
    public init(_ judgement: Judgement, _ priority: Double) {
        self.judgement = judgement
        self.priority = priority
    }
}

// MARK: Equatable

extension Bag: Equatable {
    public static func == (lhs: Bag<I>, rhs: Bag<I>) -> Bool {
        Set(lhs.items.keys) == Set(rhs.items.keys)
    }
}

extension WrappedBag: Equatable {
    public static func == (lhs: WrappedBag<I>, rhs: WrappedBag<I>) -> Bool {
        lhs.bag == rhs.bag && lhs.wrapped == rhs.wrapped
    }
}

extension Concept: Equatable {
    public static func == (lhs: Concept, rhs: Concept) -> Bool {
        lhs.term == rhs.term
        && lhs.termLinks == rhs.termLinks
        && lhs.beliefs == rhs.beliefs
    }
}

extension NARS: Equatable {
    public static func == (lhs: NARS, rhs: NARS) -> Bool {
        lhs.name == rhs.name
    }
}


/// Convenience

extension WrappedBag where I == Belief {
    /// convenience for iterating over both dictionaries
    var items: [String : I] { bag.items.merging(wrapped?.items ?? [:], uniquingKeysWith: max)}
}

extension Belief: Comparable {
    public static func < (lhs: Belief, rhs: Belief) -> Bool {
        let c = choice(j1: lhs.judgement, j2: rhs.judgement)
        return c.statement == rhs.judgement.statement
    }
}

// MARK: CustomStringConvertible

extension Concept: CustomStringConvertible {
    public var description: String {
        "\(term)".uppercased() + "\n.  \(termLinks)" + ".  \(beliefs)"
    }
}

extension TermLink: CustomStringConvertible {
    public var description: String {
        identifier + " \(priority)"
    }
}

extension Belief: CustomStringConvertible {
    public var description: String {
        "\(judgement)"
    }
}

extension Sentence: CustomStringConvertible {
    public var description: String {
        switch self {
        case .judgement(let judgement):
            return "\(judgement)"
        case .goal(let goal):
            return "\(goal)"
        case .question(let question):
            return "\(question)"
//        case .pause(let t):
//            return "💤 \(Double(t)/1000) seconds"
        case .cycle(let n):
            return "🔄 \(n) cycles"
        }
    }
}

extension Bag: CustomStringConvertible {
    public var description: String {
        let x = I.self == Concept.self ? "" : ".  "
        let o = items.values.reduce("", { $0 + "\($1)\n" + x })
        return String(o.dropLast(x.count))
    }
}

extension WrappedBag: CustomStringConvertible {
    public var description: String {
        let b = "\(bag)"
        let w = wrapped == nil ? "" : "\(wrapped!)"
        return b + "\n---\n" + w
    }
}

*/
