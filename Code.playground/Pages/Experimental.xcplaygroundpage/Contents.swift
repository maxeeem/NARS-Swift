
let name = "𝝥𝝠𝗟"

//import PlaygroundSupport
//
//PlaygroundPage.current.needsIndefiniteExecution = true

//let SELF = Term("SELF")

actor SELF {
    let nars: NARS
    init(_ output: @escaping (String) -> () = { print($0) }) async {
        defer { output("\tHi, I'm \(nars.name) 👋") }
        nars = NARS(name, output)
    }
    nonisolated
    func perform(_ script: Sentence...) {
        nars.perform(script)
    }
}

let tweety = "Tweety"
let cartoon_bird = "cartoon_bird"
let yellow = "yellow"
print(tweety•->cartoon_bird)

var statement = "Bart Simpson" •-> "cartoon_character"
print(statement)
statement     = "Bart Simpson" •->• "yellow"
print(statement, "\n")

print(TruthValue.induction(TruthValue(1, 0.9), TruthValue(0, 0.9)))


// military uses made up language to teach their staff
// use it to teach nars

// pseudo code for Narsese -> Swift compiler
// transform narsese Sentence into a valid Swift string
// terms become variables: `car` -> `let car = Term.word("car")`
extension Term {
    public var swift: String {
        var id = ""
        switch self {
        case .word:
            id = ".word"
            case .instance:
                id = ".instance"
                case .property:
                    id = ".property"
        case .compound:
            id = ".compound"
        }
        return "let \(description) = \(type(of: self))\(id)(\"\(description)\")"
    }
    public var compoundStatement: Statement? {
        switch self {
        case .compound(let connector, let terms):
            if terms.count == 2, let copula = Copula(rawValue: connector.description) {
                return Statement(terms[0], copula, terms[1])
            } else {
                return nil
            }
        default: return nil
        }
    }
}

import Foundation
extension Term {
    public init?(_ s: String) {
        let words = s.components(separatedBy: " ")
        if words.count == 3, let copula = Copula(rawValue: words[1]), let t1 = Term(words[0]), let t2 = Term(words[2]) {
            self = .compound(copula.term, [t1, t2])
        } else if words.count > 1, let connector = Connector(rawValue: words[0]) {
            let terms = words.dropFirst().compactMap(Term.init(_:))
            if !terms.isEmpty {
                self = .compound(connector.term, terms)
            } else {
                return nil
            }
        } else if words.count == 1 {
            self = Term.word(words[0])
        } else {
            return nil
        }
    }
    var copula: Copula? { Copula(rawValue: description) }
}
extension Copula {
    var term: Term { Term.word(rawValue) }
}
extension Connector {
    var term: Term! { Term.word(rawValue) }
}


// English "I" refers to Narsese "SELF"
//
// Input string for a type that can be instantiated from a greeting template
// "Hi, I'm \(name) 👋"

// experimental

print("Hi, I'm \(name) 👋")
print() // ready for input
let bird = Term.word("bird")
let animal = Term.word("animal")
let mammal = Term.word("mammal")
print("Swift: ", mammal.swift)
let compound = Term.compound(.word(Copula.inheritance.rawValue), [bird, animal])
print("Swift: ", compound.swift)
let reproduced = Term(compound.description)
print("Swift: ", reproduced == compound)
print("Swift: ", reproduced?.compoundStatement ?? "")


// Quine ( translates between Swift and Narsese )

import Foundation
var narseseToEnglish: (String) -> String = { narsese in
    var english = narsese.replacingOccurrences(of: "->", with: "is a type of")
    english = english.replacingOccurrences(of: " ?", with: " what?")
    return english
}


//#if targetEnvironment(simulator)
//#sourceLocation(file: "Source.swift", line: 1)


/// https://docs.swift.org/swift-book/ReferenceManual/Declarations.html#//apple_ref/swift/grammar/typealias-declaration

//public typealias ab = (a, a, a, a)
/*
 /// https://stackoverflow.com/a/27084702
 struct Parser<A> {
 let f: (String) -> [(A, String)]
 }
 
 func parse<A>(stringToParse: String, parser: Parser)
 let parser = Parser<Term> { string in return [string, tail(string)] }
 */



/// rule as a matrix of optional booleans
/// true identifies common term
[
    false, true,
    true , nil ,
    nil  , false
]

/// test sequence is a dual
/// A -> B
/// B -> C
/// first symbol is `false`, second is `nil
/// if there is no common term term identified by `true`
/// then a conclusion could not be derived
[
    false, true,
    true, nil
]
/// P --> M
/// M --> S
[
    false, true,
    true, nil
]
/// M --> P
/// S --> M
[
    true, nil,
    false, true
]

/// Optional<Bool> is represented as a Triple
let common = true
let first = false
var second: Bool?
[
    false, nil, true
] // OR
[ // as a list of terms
    first, second, common
]

/// if there is a false then there is at least one type of symbol present
/// if there is a second then all are not repeats
/// if there is no common term then a rule could not be applied
/// otherwise this term is a common


/// Optional<Bool> quad where 0 & 1 are false & true
[
    nil, false, true, nil
]




/// opens a `var`iable channel that changes over time
/// synchronized to system clock
var channel: Optional<Bool> {
    true
}


/// open an immutable channel
/// can represent eternal statements or questions:
let immutableChannel: Judgement = ("A"-->"B")-* // -?


///
/// if narsese can be translated to swift
/// then a well-formed narsese sentence must compile
///

/// operations can be expressed as functions
/// assigned to a variable like above
/// variable name is the name of operation in nars
/// source code stored elsewhere
///



func validate(a: Term,
              b: Term,
              c: Term,
              d: Term) -> Bool {
    print(a --> b)
    print(b --> d)
    return false
}
//print("--")
//validate(a: test1.subject,
//         b: test1.predicate,
//         c: test2.subject,
//         d: test2.predicate)
//print(t1.copula == s1.copula)
//print(t2.copula == s2.copula)
//print(s1.copula.makeStatement(t1.subject, t1.predicate))
//print(s2.copula.makeStatement(t2.subject, t2.predicate))
