# [NARS-Swift <sup>![gh](https://octicons.glitch.me/mark-github.svg)</sup>](https://github.com/maxeeem/NARS-Swift)

Swift implementation of Pei Wang's [Non-Axiomatic Logic](https://books.apple.com/us/book/non-axiomatic-logic-a-model-of-intelligent-reasoning/id666735302).

> Intelligence is the ability for a system to adapt to its environment and to work with insufficient knowledge and resources.

Assumption of Insufficient Knowledge and Resources, or **AIKR**, is the fundamental feature of NARS — a Non-Axiomatic Reasoning System. 

# Quickstart
TBD

# Overview 
The system consists of two parts – the [logic part]() and the [control part](), with the latter dependent on the logic.

"The representation language of NARS is called *Narsese*, which serves both the roles of internal representation and external communication for NARS." In [NARS-Swift](), we embed Narsese in the programming language of the system (Swift) as a DSL or Domain Specific Language, so statements in Swift Narsese dialect are both valid Narsese *and* valid Swift code. 

## Logic
Statements in Narsese represent relations between terms, and inference rules are applied to statements when they share a common term. The simplest type of term is a `word`, a [Copula]() connects two terms to form a `statement`, and you can use a [Connector]() to create a `compound` containing two or more terms (there are certain cases where compounds consist of only one term). In addition to the types mentioned above, there are `variable` and `operation` terms.

```[narsese in swift]```

Several [extensions]() to the language allow writing Narsese statements like `(bird --> animal)`, which are simultaneously valid Swift code. Having embedded Narsese as a DSL in Swift, it is now possible to express the [inference rules]() of NAL directly. For example, one of the deduction rules is `(M --> P, S --> M, S --> P)`.

```[inference rules]```

During inference, several [extensions]() transform Narsese into logic terms, and the solver produces a set of substitutions matching the rule’s pattern. Later, we reverse the process to obtain Narsese statements from logic terms. Another DSL called [miniKanren]() helps decide which rules apply to any two statements. It is a relational programming language designed to be small and embeddable, and in NARS-Swift, we use Dimitri Racordon’s implementation.

For external communication, it is often convenient to express Narsese as a string of text. While technically not part of the core system, that functionality is highly desirable and is implemented as part of NARS+, extending the system’s capabilities. A third-party library [Covfefe]() by Palle Klewitz translates Narsese [grammar]() defined in Backus-Naur Form into an Abstract Syntax Tree (AST) which we then convert to Narsese data structures. 

## Control
TBD
