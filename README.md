# [NARS-Swift <sup>![gh](https://github.com/maxeeem/NARS-Swift/blob/main/docs/assets/mark-github.svg?raw=true&sanitize=true)</sup>](https://github.com/maxeeem/NARS-Swift)

![one](https://user-images.githubusercontent.com/1018034/208796237-f76a0028-a59d-4254-b3dd-f308f5622774.jpg)

Swift implementation of Pei Wang's [Non-Axiomatic Logic](https://books.apple.com/us/book/non-axiomatic-logic-a-model-of-intelligent-reasoning/id666735302).

> Intelligence is the ability for a system to adapt to its environment and to work with insufficient knowledge and resources.

Assumption of Insufficient Knowledge and Resources, or **AIKR**, is the fundamental feature of NARS — a Non-Axiomatic Reasoning System. 

# Quickstart
You need to install Swift for your [platform](https://www.swift.org/getting-started/). Supported platforms include macOS, Linux, and Windows. For ARM-based devices like Raspberry Pi, you can use the [Swiftlang.xyz](http://swiftlang.xyz) repo.

Once you have the Swift runtime installed, clone [this repo](https://github.com/maxeeem/NARS-Swift) and type 

```swift run``` 

This will launch the default `nar` executable.

### Use cases
There are three primary ways to use NARS-Swift.

#### Command-line tool

You can build `nar` with

```swift build```

Compiled binary is located in the `/.build` folder.

#### Playground

If you have a Mac or an iPad, the easiest way to get started is to run the included `Code.playground` in [Swift Playgrounds](https://www.apple.com/swift/playgrounds/) app. It will allow you to experiment with the system and explore its capabilities without needing to install anything.

#### Swift Package Manager

For more advanced uses, add the following dependency to your Package.swift file:

```
.package(url: "https://github.com/maxeeem/NARS-Swift", from: "0.1.0")
```

There are four modules to choose from depending on your use case.

`import NAL` if you want just the logic.

`import NARS` if you want the complete system.

`import Narsese` for converting strings of text into Narsese terms.

Finally, there is `nar` command line tool that puts all of the above modules together.


# Overview 
The system consists of two parts – the [logic part](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NAL) and the [control part](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NARS), with the latter dependent on the logic.

"The representation language of NARS is called *Narsese*, which serves both the roles of internal representation and external communication for NARS." In [NARS-Swift](https://github.com/maxeeem/NARS-Swift), we embed Narsese in the programming language of the system (Swift) as a DSL or Domain Specific Language, so statements in Swift Narsese dialect are both valid Narsese *and* valid Swift code. 

## Logic
In [Narsese](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NAL/Narsese.swift), statements represent relations between terms, and [inference rules](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NAL/Inference.swift#L107) are applied to statements when they share a common term. The simplest type of term is a `word`, a [Copula](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NAL/Copula.swift) connects two terms to form a `statement`, and you can use a [Connector](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NAL/Narsese.swift#L14) to create a `compound` containing two or more terms (there are certain cases where compounds consist of only one term). In addition to the types mentioned above, there are `variable` and `operation` terms.

<img src="https://github.com/maxeeem/NARS-Swift/blob/main/docs/assets/Narsese_nal1.png?raw=true" width="340">

```swift
public indirect enum Term {
    case symbol(String) /// <word>
    case statement(Term, Copula, Term)
    case compound(Connector, [Term])
    ...
}
```

<img src="https://github.com/maxeeem/NARS-Swift/blob/main/docs/assets/Narsese_symbols.png?raw=true" width="410">

```swift
public enum Copula {
    //// Primary
    case inheritance       =    "->" // NAL 1
    case similarity        =   "<–>"     // 2
    case implication       =    "=>"     // 5
    case equivalence       =   "<=>"     // 5
    ...
}
```

Several [extensions](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NAL/DSL.swift) to the language allow writing Narsese statements like `(bird --> animal)`, which are simultaneously valid Swift code. Having embedded Narsese as a DSL in Swift, it is now possible to express the inference rules of NAL directly.

<img src="https://github.com/maxeeem/NARS-Swift/blob/main/docs/assets/Narsese_rules.jpeg?raw=true" width="450">

```swift
...
case .deduction:
    return [(M --> P,     S --> M, S --> P, tf),
            (P --> M,     M --> S, P --> S, tfi)]
case .induction:
    return [(M --> P,     M --> S, S --> P, tf),
            (M --> P,     M --> S, P --> S, tfi)]
...
```

During inference, several [extensions](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NAL/Utility/Logic.swift) transform Narsese into logic terms, and the solver produces a set of substitutions matching the rule’s pattern. Later, we reverse the process to obtain Narsese statements from logic terms. Another DSL called [miniKanren](https://github.com/kyouko-taiga/SwiftKanren) helps decide which rules apply to any two statements. It is a relational programming language designed to be small and embeddable, and in NARS-Swift, we use Dimitri Racordon’s implementation.

For external communication, it is often convenient to express Narsese as a string of text. While technically not part of the core system, that functionality is highly desirable and is implemented as part of NARS+, extending the system’s capabilities. A third-party library [Covfefe](https://github.com/palle-k/Covfefe) by Palle Klewitz translates Narsese [grammar](https://github.com/maxeeem/NARS-Swift/blob/main/Sources/Narsese/Narsese.swift) defined in Backus-Naur Form into an Abstract Syntax Tree (AST) which we then convert to Narsese data structures. 

<img src="https://github.com/maxeeem/NARS-Swift/blob/main/docs/assets/Narsese_grammar.jpeg?raw=true" width="500">

## Control
TBD
