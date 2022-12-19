# [NARS-Swift <sup>![gh](https://github.com/maxeeem/NARS-Swift/blob/main/docs/assets/mark-github.svg?raw=true&sanitize=true)</sup>](https://github.com/maxeeem/NARS-Swift)

Swift implementation of Pei Wang's [Non-Axiomatic Logic](https://books.apple.com/us/book/non-axiomatic-logic-a-model-of-intelligent-reasoning/id666735302).

> Intelligence is the ability for a system to adapt to its environment and to work with insufficient knowledge and resources.

Assumption of Insufficient Knowledge and Resources, or **AIKR**, is the fundamental feature of NARS — a Non-Axiomatic Reasoning System. 

# Quickstart
TBD

# Overview 
The system consists of two parts – the [logic part](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NAL) and the [control part](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NARS), with the latter dependent on the logic.

"The representation language of NARS is called *Narsese*, which serves both the roles of internal representation and external communication for NARS." In [NARS-Swift](https://github.com/maxeeem/NARS-Swift), we embed Narsese in the programming language of the system (Swift) as a DSL or Domain Specific Language, so statements in Swift Narsese dialect are both valid Narsese *and* valid Swift code. 

## Logic
Statements in Narsese represent relations between terms, and inference rules are applied to statements when they share a common term. The simplest type of term is a `word`, a [Copula](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NAL/Copula.swift) connects two terms to form a `statement`, and you can use a [Connector](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NAL/Narsese.swift#L14) to create a `compound` containing two or more terms (there are certain cases where compounds consist of only one term). In addition to the types mentioned above, there are `variable` and `operation` terms.

![NAL-1](https://github.com/maxeeem/NARS-Swift/blob/main/docs/assets/Narsese_nal1.png?raw=true)

```swift
public indirect enum Term {
    case symbol(String) /// <word>
    case compound(Connector, [Term])
    
    case statement(Term, Copula, Term)
    
    case variable(Variable)
    case operation(String, [Term])
}
```

![Narsese symbols](https://github.com/maxeeem/NARS-Swift/blob/main/docs/assets/Narsese_symbols.png?raw=true)

```swift
public enum Copula {
    //// Primary
    case inheritance       =    "->" // NAL 1
    case similarity        =   "<–>"     // 2
    case implication       =    "=>"     // 5
    case equivalence       =   "<=>"     // 5
    ...
}

public enum Connector: String {
    /// intensional set Ω
    case intSet = "[]"
    /// extensional set U
    case extSet = "{}"
    
    /// extensional intersection
    case Ω = "⋂" /// intensional set
    /// intensional intersection
    case U = "⋃" /// extensional set
    ...
}
```

Several [extensions](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NAL/DSL.swift) to the language allow writing Narsese statements like `(bird --> animal)`, which are simultaneously valid Swift code. Having embedded Narsese as a DSL in Swift, it is now possible to express the [inference rules](https://github.com/maxeeem/NARS-Swift/blob/main/Code.playground/Sources/NAL/Inference.swift#L107) of NAL directly. For example, one of the deduction rules is `(M --> P, S --> M |- S --> P)`.

![Narsese rules](https://github.com/maxeeem/NARS-Swift/blob/main/docs/assets/Narsese_rules.png?raw=true)
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

![Narsese grammar](https://github.com/maxeeem/NARS-Swift/blob/main/docs/assets/Narsese_grammar.png?raw=true)

## Control
TBD
