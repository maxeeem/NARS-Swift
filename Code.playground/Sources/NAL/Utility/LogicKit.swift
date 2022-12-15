//
//  LogicKit.swift
//  LogicKit
//
//  Created by Dimitri Racordon on 07.02.17.
//  Copyright © 2017 University of Geneva. All rights reserved.
//
//  https://github.com/kyouko-taiga/SwiftKanren

protocol LogicTerm {

    // We can't make the LogicTerm conform to Equatable, as we need to use within
    // heterogeneous collections. Hence we can't have a safe requirements
    // (see WWDC 2015 - session 408). Similarly, we can't require conforming
    // types to implement the global equality operator (==), as the various
    // overloads would become ambiguous without a self requirement.
    func equals(_ other: LogicTerm) -> Bool

}

extension LogicTerm where Self: Equatable {

    func equals(_ other: LogicTerm) -> Bool {
        if other is Self {
            return (other as! Self) == self
        }

        return false
    }

}



struct LogicVariable: LogicTerm {

    let name: String

    init(named name: String) {
        self.name = name
    }

}

extension LogicVariable: Hashable {

    func hash(into hasher: inout Hasher) {
        hasher.combine(name)
    }

    static func == (left: LogicVariable, right: LogicVariable) -> Bool {
        return left.name == right.name
    }

}

extension LogicVariable: CustomStringConvertible {

    var description: String {
        return self.name
    }

}


class LogicVariableFactory {

    fileprivate var state: State
    private var LogicVariables = [String: LogicVariable]()

    fileprivate init(_ state: State) {
        self.state = state
    }

    subscript(name: String) -> LogicVariable {
        if let LogicVariable = self.LogicVariables[name] {
            return LogicVariable
        }

        self.LogicVariables[name] = LogicVariable(named: self.state.nextUnusedName)
        self.state = self.state.withNextNewName()
        return self.LogicVariables[name]!
    }

}


struct LogicValue<T: Equatable>: LogicTerm {

    internal let wrapped: T

    init(_ val: T) {
        self.wrapped = val
    }

    func equals(_ other: LogicTerm) -> Bool {
        if let rhs = (other as? LogicValue<T>) {
            return rhs.wrapped == self.wrapped
        }

        return false
    }

}

extension LogicValue: Equatable {

    static func ==(lhs: LogicValue, rhs: LogicValue) -> Bool {
        return lhs.wrapped == rhs.wrapped
    }

}

extension LogicValue: CustomStringConvertible {

    var description: String {
        return String(describing: self.wrapped)
    }

}

/*
struct Unassigned: LogicTerm, CustomStringConvertible {

    private static var LogicVariables = [LogicVariable: Int]()
    private static let unicodeSubscripts = [
        "\u{2080}", "\u{2081}", "\u{2082}", "\u{2083}", "\u{2084}",
        "\u{2085}", "\u{2086}", "\u{2087}", "\u{2088}", "\u{2089}"]

    private var id: Int

    fileprivate init(_ LogicVariable: LogicVariable) {
        if Unassigned.LogicVariables[LogicVariable] == nil {
            Unassigned.LogicVariables[LogicVariable] = Unassigned.LogicVariables.count
        }
        self.id = Unassigned.LogicVariables[LogicVariable]!
    }

    func equals(_ other: LogicTerm) -> Bool {
        return false
    }

    var description: String {
        var suffix = ""
        if self.id == 0 {
            suffix = Unassigned.unicodeSubscripts[0]
        } else {
            var number = self.id
            while number > 0 {
                suffix = Unassigned.unicodeSubscripts[number % 10] + suffix
                number /= 10
            }
        }

        return "_" + suffix
    }

}
*/

enum List: LogicTerm {

    case empty, cons(LogicTerm, LogicTerm)

    func equals(_ other: LogicTerm) -> Bool {
        guard let rhs = other as? List else {
            return false
        }

        switch (self, rhs) {
        case (.empty, .empty):
            return true
        case (.cons(let lh, let lt), .cons(let rh, let rt)):
            return lh.equals(rh) && lt.equals(rt)
        default:
            return false
        }
    }

}

/*
struct Map: LogicTerm {

    typealias StorageType = [String: LogicTerm]

    fileprivate let storage: StorageType

    init() {
        self.storage = [:]
    }

    init<S: Sequence>(_ items: S) where S.Iterator.Element == (key: String, value: LogicTerm) {
        var storage = StorageType()
        for (key, value) in items {
            storage[key] = value
        }
        self.storage = storage
    }

    var keys: Dictionary<String, LogicTerm>.Keys {//LazyMapCollection<StorageType, String> {
        return self.storage.keys
    }

    var values: Dictionary<String, LogicTerm>.Values {//LazyMapCollection<StorageType, LogicTerm> {
        return self.storage.values
    }

    subscript(key: String) -> LogicTerm? {
        return self.storage[key]
    }

    func with(key: String, value: LogicTerm) -> Map {
        var newStorage = self.storage
        newStorage[key] = value
        return Map(newStorage)
    }

}

extension Map: Equatable {

    static func == (left: Map, right: Map) -> Bool {
        let leftKeys = left.storage.keys.sorted()
        let rightKeys = right.storage.keys.sorted()

        guard leftKeys == rightKeys else {
            return false
        }

        for (leftKey, rightKey) in zip(leftKeys, rightKeys) {
            guard left.storage[leftKey]!.equals(right.storage[rightKey]!) else {
                return false
            }
        }

        return true
    }

}

extension Map: Sequence {

    func makeIterator() -> StorageType.Iterator {
        return self.storage.makeIterator()
    }

}

extension Map: Collection {

    var startIndex: StorageType.Index {
        return self.storage.startIndex
    }

    var endIndex: StorageType.Index {
        return self.storage.endIndex
    }

    func index(after: StorageType.Index) -> StorageType.Index {
        return self.storage.index(after: after)
    }

    subscript(index: StorageType.Index) -> StorageType.Element {
        return self.storage[index]
    }

}

extension Map: ExpressibleByDictionaryLiteral {

    init(dictionaryLiteral elements: (String, LogicTerm)...) {
        self.init(elements.map { (key: $0.0, value: $0.1) })
    }

}

extension Map: CustomStringConvertible {

    var description: String {
        return String(describing: self.storage)
    }

}
*/

struct Substitution {

    init() {
        
    }
    
    fileprivate var storage = [LogicVariable: LogicTerm]()

    typealias Association = (LogicVariable: LogicVariable, LogicTerm: LogicTerm)

    subscript(_ key: LogicTerm) -> LogicTerm {
        // If the the given key isn't a LogicVariable, we can just give it back.
        guard let k = key as? LogicVariable else {
            return key
        }

        if let rhs = self.storage[k] {
            // Continue walking in case the rhs is another LogicVariable.
            return self[rhs]
        }

        // We give back the LogicVariable if is not associated.
        return key
    }

    func extended(with association: Association) -> Substitution {

        // NOTE: William Byrd's PhD thesis doesn't specify what is the
        // expected behaviour when extending a substitution map with an
        // already existing key.

        // TODO: Check for introduced circularity.

        var result = self
        result.storage[association.LogicVariable] = association.LogicTerm
        return result
    }

    func unifying(_ u: LogicTerm, _ v: LogicTerm) -> Substitution? {
        let walkedU = self[u]
        let walkedV = self[v]

        // LogicTerms that walk to equal values always unify, but add nothing to
        // the substitution.
        if walkedU.equals(walkedV) {
            return self
        }

        // Unifying a logic LogicVariable with some other LogicTerm creates a new entry
        // in the substitution.
        if walkedU is LogicVariable {
            return self.extended(with: (LogicVariable: walkedU as! LogicVariable, LogicTerm: walkedV))
        } else if walkedV is LogicVariable {
            return self.extended(with: (LogicVariable: walkedV as! LogicVariable, LogicTerm: walkedU))
        }

        // If the walked values of u and of v are lists, then unifying them
        // boils down to unifying their elements.
        if (walkedU is List) && (walkedV is List) {
            return self.unifyingLists(walkedU as! List, walkedV as! List)
        }

        // If the walked values of u and of v are maps, then unifying them
        // boils down to unifying their elements.
//        if (walkedU is Map) && (walkedV is Map) {
//            return self.unifyingMaps(walkedU as! Map, walkedV as! Map)
//        }

        return nil
    }

    private func unifyingLists(_ u: List, _ v: List) -> Substitution? {
        switch (u, v) {
        case (.empty, .empty):
            // Empty lists always unify, but add nothing to the substitution.
            return self

        case (.cons(let uh, let ut), .cons(let vh, let vt)):
            // Unifying non-empty lists boils down to unifying their head,
            // before recursively unifying their tails.
            return self.unifying(uh, vh)?.unifying(ut, vt)

        default:
            // Unifying a non-empty list with an empty list always fail.
            return nil
        }
    }
/*
    private func unifyingMaps(_ u: Map, _ v: Map) -> Substitution? {
        let leftKeys = u.keys.sorted()
        let rightKeys = v.keys.sorted()

        // Unifying dictionaries with different keys always fail.
        guard leftKeys == rightKeys else {
            return nil
        }

        // Unifying dictionaires boils down to unifying the values associated,
        // with each of their respective keys.
        var result: Substitution? = self
        for (leftKey, rightKey) in zip(leftKeys, rightKeys) {
            result = result?.unifying(u[leftKey]!, v[rightKey]!)
        }
        return result
    }
*/
/*
    func reified() -> Substitution {
        var result = Substitution()
        for LogicVariable in self.storage.keys {
            let walked = self.deepWalk(LogicVariable)
            if let v = walked as? LogicVariable {
                result = result.extended(with: (LogicVariable: LogicVariable, LogicTerm: Unassigned(v)))
            } else {
                result = result.extended(with: (LogicVariable: LogicVariable, LogicTerm: walked))
            }
        }
        return result
    }

    private func deepWalk(_ value: LogicTerm) -> LogicTerm {
        // If the given value is a list, we have to "deep" walk its elements.
        if let l = value as? List {
            switch l {
            case .empty:
                return l
            case .cons(let h, let t):
                return List.cons(self.deepWalk(h), self.deepWalk(t))
            }
        }

        // If the given value is a map, we have to "deep" walk its values.
        if let m = value as? Map {
            var reifiedMap = Map()
            for item in m {
                reifiedMap = reifiedMap.with(key: item.key, value: self.deepWalk(item.value))
            }
            return reifiedMap
        }

        // If the the given value isn't a LogicVariable, we can just give it back.
        guard let key = value as? LogicVariable else {
            return value
        }

        if let rhs = self.storage[key] {
            // Continue walking in case the rhs is another LogicVariable.
            return self.deepWalk(rhs)
        }

        // We give back the LogicVariable if is not associated.
        return value
    }
*/
}

extension Substitution: Sequence {

    func makeIterator() -> AnyIterator<Association> {
        var it = self.storage.makeIterator()

        return AnyIterator {
            if let (LogicVariable, LogicTerm) = it.next() {
                return (LogicVariable: LogicVariable, LogicTerm: self[LogicTerm])
            }

            return nil
        }
    }

}


/// A struct containing a substitution and the name of the next unused logic
/// LogicVariable.
struct State {

    fileprivate let substitution: Substitution
    fileprivate var nextUnusedName: String {
        return "$" + String(describing: self.nextId)
    }

    private let nextId: Int

    init(substitution: Substitution = Substitution(), nextId: Int = 0) {
        self.substitution = substitution
        self.nextId = nextId
    }

    fileprivate func with(newSubstitution: Substitution) -> State {
        return State(substitution: newSubstitution, nextId: self.nextId)
    }

    fileprivate func withNextNewName() -> State {
        return State(substitution: self.substitution, nextId: self.nextId + 1)
    }

}


enum Stream {

    case empty
    indirect case mature(head: State, next: Stream)
    case immature(thunk: () -> Stream)

    // mplus
    fileprivate func merge(_ other: Stream) -> Stream {
        switch self {
        case .empty:
            return other

        case .mature(head: let state, next: let next):
            return .mature(head: state, next: next.merge(other))

        case .immature(thunk: let thunk):
            return .immature {
                return other.merge(thunk())
            }
        }
    }

    // bind
    fileprivate func map(_ LogicGoal: @escaping LogicGoal) -> Stream {
        switch self {
        case .empty:
            return .empty

        case .mature(head: let head, next: let next):
            return LogicGoal(head).merge(next.map(LogicGoal))

        case .immature(thunk: let thunk):
            return .immature {
                return thunk().map(LogicGoal)
            }
        }
    }

    // pull
    fileprivate func realize() -> Stream {
        switch self {
        case .empty:
            return .empty

        case .mature(head: _, next: _):
            return self

        case .immature(thunk: let thunk):
            return thunk().realize()
        }
    }

}

extension Stream: Sequence {

    func makeIterator() -> AnyIterator<Substitution> {
        var it = self

        return AnyIterator {

            // Realize the iterated stream here, so that we its state is
            // computed as lazily as possible (i.e. when the iterator's next()
            // method is called).

            switch it.realize() {
            case .empty:
                // Return nothing for empty stream, ending the sequence.
                return nil

            case .mature(head: let state, next: let successor):
                // Return the realized substitution and advance the iterator.
                it = successor
                return state.substitution

            case .immature(thunk: _):
                assertionFailure("realize shouldn't produce immature streams")
            }

            return nil
        }
    }

}


/// Represents a function that encapsulates a logic program and which, given a
/// state, returns a stream of states for each way the program can succeed.
typealias LogicGoal = (State) -> Stream


infix operator ≡   : ComparisonPrecedence
infix operator === : ComparisonPrecedence

/// Creates a LogicGoal that unify two LogicTerms.
///
/// The LogicGoal takes an existing state and returns (as a lazy stream) either a
/// state with bindings for the LogicVariables in u and v (using unification), or
/// nothing at all if u and v cannot be unified.
func ≡ (u: LogicTerm, v: LogicTerm) -> LogicGoal {
    return { state in
        if let s = state.substitution.unifying(u, v) {
            return .mature(head: state.with(newSubstitution: s), next: .empty)
        }

        return .empty
    }
}

/// Alternative for ≡(_:_:)
func === (u: LogicTerm, v: LogicTerm) -> LogicGoal {
    return u ≡ v
}

let x = fresh({ x in
    x === LogicVariable(named: "a")
})

let v = freshn({ v in
    v["x"] === v["y"]
})

/// Takes a LogicGoal constructor and returns a LogicGoal with fresh LogicVariables.
///
/// This function takes a *LogicGoal constructor* (i.e. a function), which accepts
/// a single LogicVariable as parameter, and returns a new LogicGoal for which the
/// LogicVariable is fresh.
func fresh(_ constructor: @escaping (LogicVariable) -> LogicGoal) -> LogicGoal {
    return { state in
        constructor(LogicVariable(named: state.nextUnusedName))(state.withNextNewName())
    }
}


/// Takes a LogicGoal constructor and returns a LogicGoal with fresh LogicVariables.
///
/// This function takes a *LogicGoal constructor* (i.e. a function), which accepts
/// a LogicVariable factory as parameter, and returns a new LogicGoal for which all the
/// LogicVariables generated by the factory are fresh.
func freshn(_ constructor: @escaping (LogicVariableFactory) -> LogicGoal) -> LogicGoal {
    return { state in
        let factory = LogicVariableFactory(state)
        return constructor(factory)(factory.state)
    }
}


/// Constructs a disjunction of LogicGoals.
func || (left: @escaping LogicGoal, right: @escaping LogicGoal) -> LogicGoal {
    return { state in
        left(state).merge(right(state))
    }
}


/// Constructs a conjunction of LogicGoals.
func && (left: @escaping LogicGoal, right: @escaping LogicGoal) -> LogicGoal {
    return { state in
        left(state).map(right)
    }
}

/*
/// Takes a LogicGoal constructor and returns a LogicGoal with substitution.
///
/// This function takes a *LogicGoal constructor* (i.e. a function), which accepts
/// a substitution as parameter, and returns a new LogicGoal.
func inEnvironment (_ constructor: @escaping (Substitution) -> LogicGoal) -> LogicGoal {
    return { state in
        let reified = state.substitution.reified()
        return constructor(reified)(state)
    }
}
*/

/// Takes a LogicGoal and returns a thunk that wraps it.
func delayed(_ LogicGoal: @escaping LogicGoal) -> LogicGoal {
    return { state in
        .immature { LogicGoal(state) }
    }
}


/// Executes a logic program (i.e. a LogicGoal) with an optional initial state.
func solve(withInitialState state: State? = nil, _ program: LogicGoal) -> Stream {
    return program(state ?? State())
}


/// A LogicGoal that always succeeds.
let success = (LogicValue(true) === LogicValue(true))


/// A LogicGoal that always fails.
let failure = (LogicValue(false) === LogicValue(true))

/*
/// Creates a LogicGoal that tests if a LogicTerm is an instance of a `Value<T>`
/// in the current substitution.
func isValue<T : Equatable>(_ LogicTerm: LogicTerm, _ type: T.Type) -> LogicGoal {
    return inEnvironment { substitution in
        if substitution [LogicTerm] is LogicValue<T> {
          return success
        } else {
          return failure
        }
    }
}


/// Creates a LogicGoal that tests if a LogicTerm is an instance of a `LogicVariable`
/// in the current substitution.
func isLogicVariable(_ LogicTerm: LogicTerm) -> LogicGoal {
    return inEnvironment { substitution in
        if substitution [LogicTerm] is LogicVariable {
          return success
        } else {
          return failure
        }
    }
}


/// Creates a LogicGoal that tests if a LogicTerm is an instance of a `List`
/// in the current substitution.
func isList(_ LogicTerm: LogicTerm) -> LogicGoal {
    return inEnvironment { substitution in
        if substitution [LogicTerm] is List {
          return success
        } else {
          return failure
        }
    }
}


/// Creates a LogicGoal that tests if a LogicTerm is an instance of a `Map`
/// in the current substitution.
func isMap(_ LogicTerm: LogicTerm) -> LogicGoal {
    return inEnvironment { substitution in
        if substitution [LogicTerm] is Map {
          return success
        } else {
          return failure
        }
    }
}
*/
