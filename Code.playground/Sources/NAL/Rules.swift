
/// Swift Tuple is a basic primitive
//typealias Triple = (Bool?, Bool?, Bool?)
typealias Quad<T: Equatable> = (T, T, T, T)

/// Statement is a fundamental type
public typealias Rule = (Statement, Statement, Statement, TruthFunction)
public typealias Apply = (_ judgements: (Judgement, Judgement)) -> Judgement? // reduce operation

public typealias Infer = (Judgement) -> Judgement?

public enum Rules: String, CaseIterable, Codable {
    // NAL-1
    case deduction
    case induction
    case abduction
    case exemplification
    // NAL-2
    case comparison
    case analogy
    case resemblance
    // Compositional
    case intersection
    case union
    case difference
    // Local
    case negation
    case conversion
    case contraposition
    case revision
}

extension Rules {
    static let strong: [Rules] = [.deduction, .analogy, .resemblance]
    //TODO: should we add intersection, difference and union to the list?
    
    static func immediate(_ j: Judgement) -> [Judgement] {
        let immediate: [Infer] = [/*negation(j1:),*/ conversion(j1:), contraposition(j1:)]
        return immediate.compactMap { $0(j) }
    }
}

extension Rules {
    var tf: TruthFunction {
        TruthValue.truthFunction(self, false)
    }
    var tfi: TruthFunction { // inverse
        TruthValue.truthFunction(self, true)
    }
    public var apply: (_ judgements: (Judgement, Judgement)) -> [Judgement?] {
        { j in
            var (j1, j2) = j
//            print("\n>>>", j)
            
            var t1 = j1.statement // test
            var t2 = j2.statement // test
//            print(p1, p2, j1, j2)
    //        print("=", commonTerms)
    //        return nil
            
            var x: [Judgement?] = []
            
            if case .compound(let conn, let ts1) = t1, conn == .n {
//                print("1.", t1, t2)
                if ts1[0] == t2 { // TODO: use similarity helper to account for symmetrical connectors and copulas
                    return x // no conclusion can be reached if premises are just opposite of each other
                }
            }
            if case .compound(let conn, let ts2) = t2, conn == .n {
//                print("2.", t1, t2)
                if ts2[0] == t1 { // TODO: use similarity helper to account for symmetrical connectors and copulas
                    return x // no conclusion can be reached if premises are just opposite of each other
                }
            }
             
            /// temporal
            
//            if j1.truthValue.rule == nil && j2.truthValue.rule == nil {
//                
//            }
            
            /// variable elimination
//            print("before")
//            print("t1", t1)
//           print("t2", t2)
//           print("")
            /// independent
            t1 = variableEliminationIndependent(t1, t2)
            t2 = variableEliminationIndependent(t2, t1)
//            print("after")
//             print("t1", t1)
//            print("t2", t2)
//            print("")
            /// dependent
            if let result = variableEliminationDependent(t1, t2, j1, j2, self) {
                return result
            } else if let result = variableEliminationDependent(t2, t1, j2, j1, self) {
                return result
            }
            
            /// original code
            
            j1 = Judgement(t1, j1.truthValue, j1.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
            j2 = Judgement(t2, j2.truthValue, j2.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
            
            x.append(contentsOf: self.allRules.flatMap { r in
                [rule_generator(r)((j1, j2)),
                 rule_generator(r)((j2, j1))] // switch order of premises
            })
            
            
            // MARK: Variable introduction
            // “In all these rules a dependent variable is only introduced into a conjunction or intersection, and an independent variable into (both sides of) an implication or equivalence.”
            
            /// independent-variable introduction
            ///
            // TODO: introVarInner from CompositionalRules in OpenNARS
            ///
            if self == .induction || self == .comparison {
                x.append(contentsOf: variableIntroductionIndependent(t1, t2, j1, j2, self))
            }
            
            /// dependent-variable introduction
            
            if self == .intersection {
                x.append(contentsOf: variableIntroductionDependent(t1, t2, j1, j2, self))
            }
            
            ///
            // TODO: multi-variable introduction rules
            ///
            
            let unique = Dictionary(grouping: x.compactMap({$0})) {
                $0.identifier
            }.values.compactMap {
                $0.max { j1, j2 in
                    let j = choice(j1: j1, j2: j2)
                    return j.statement == j2.statement
                }
            }
            
//            print("+++", x)
//            print("===", unique)
            return unique
        }
    }
}

// MARK: Rule application

private var checkOverlap = false // TODO: dirty trick to get dependent-variable introduction to work

let rule_generator: (_ rule: Rule) -> Apply = { (arg) -> ((Judgement, Judgement)) -> Judgement? in
    // premise (p1) premise (p2) conclusion (c) truth-function (tf)
    var (p1, p2, c, tf) = arg

    return { (arg) in
        let (j1, j2) = arg
        
        if let j1t = j1.tense, let j2t = j2.tense, j1t != j2t {
            return nil // temporal order cannot be determined
        }
        
        /*
         * MARK: do temporal
         */
        
        // TODO: need to check copulas to ensure temporal order can be established
        
        guard let temporal = temporalReasoning(c) else {
            return nil // outside temporal window
        }

        c = temporal // introduce temporal copulas

        /*
         * MARK: appply logic
         */
        
        guard let result = logicReasoning(c) else {
            return nil // no conclusion
        }
        
        //        print("here", result)

        // TODO: handle temporal compounds
        // TODO: check that compounds do not contain each other

        /*
         * MARK: check results
         */
        
        if let statement = validate(result), !statement.isTautology {
//            print("accepted", result)
            let truthValue = tf(j1.truthValue, j2.truthValue)
            let derivationPath = Judgement.mergeEvidence(j1, j2)
//            print("--")
//            print(j1, j2)
//            print("accepted", statement, truthValue, c)
            return Judgement(statement, truthValue, derivationPath, tense: j1.tense ?? j2.tense)
        }
        
        return nil // PROGRAM END
        
        
        // MARK: - helpers
        
        func temporalReasoning(_ t: Term) -> Term? {
            if j1.timestamp != ETERNAL, j2.timestamp != ETERNAL,
               case .statement(var cs, var cc, var cp) = t,
               cc == .implication || cc == .equivalence {
                let forward = j1.timestamp < j2.timestamp
                let delta = forward ? j2.timestamp - j1.timestamp : j1.timestamp - j2.timestamp
                
                let window = 50 * 1000000 // 50 ms
                let distance = 10
                // delta should be less than some param
                // otherwise rules should not apply
                // here we choose 1 second computed as
                // distance factor on either side of the window
                
                guard delta < window * 2 * distance else {
                    return nil
                }
                //                print("N", delta, window, forward, j1, j2)
                //                print(j1.timestamp, j2.timestamp)
                
                if delta < window {
                    //                    print("||", t)
                    if cc == .implication {
                        cc = .concurrentImp
                    } else if cc == .equivalence {
                        cc = .concurrentEq
                    }
                } else if forward {
                    //                    print(">>", t)
                    if cc == .implication {
                        cc = .predictiveImp
                    } else if cc == .equivalence {
                        cc = .predictiveEq
                    }
                } else {
                    //                    print("<<", t)
                    if cc == .implication {
                        cc = .retrospectiveImp
                    } else if cc == .equivalence {
                        let swap = cs
                        cs = cp
                        cc = .predictiveEq
                        cp = swap
                    }
                }
                
                // use temporal conclusion
                return .statement(cs, cc, cp)
            }
            return t // use original conclusion
        }
        
        func logicReasoning(_ t: Term) -> Term? {
            var result = t
            var map: [String: Term] = [:]
            let test1: LogicGoal = (p1.logic() === j1.statement.logic())
            let test2: LogicGoal = (p2.logic() === j2.statement.logic())
            
            let substitution = solve(test1 && test2).makeIterator().next()
            
            // TODO: use LogicVariableFactory to avoid collision with terms
            // i.e. terms names "S" and "P" will fail a check below and produce no conclusion
            
            if let sol = substitution {
                //            print("\n---SOL---\n", sol, "\n")
                let ts = (p1.terms + p2.terms + c.terms).flatMap({ $0.terms.map({ $0.logic() }) })
                let valid = sol.allSatisfy { (v, _) in
                    ts.contains { $0.equals(v) }
                }
                
                if valid {
                    for item in sol {
                        //                print(item.LogicVariable.name)
                        //                print(type(of: item.LogicTerm))
                        result = result.replace(termName: item.LogicVariable.name, term: .from(logic: item.LogicTerm))
                        //                print("result\n", result)
                    }
                }
            }
            
//            print("}}}}", j1, j2, result)
            
            return (result == t) ? nil : result
        }
        
        func validate(_ term: Term) -> Term? {
            switch term {

            // TODO: difference connectors take exactly 2 terms

            case .compound(let connector, let terms):
                if terms.count != 2 { // TODO: handle multiple components
                    if terms.count == 1, connector == .intSet || connector == .extSet {
                        if case .compound(let c, let ts) = terms[0] {
                            if ts.count == 1, c == .intSet || c == .extSet {
                                return nil // prevent nesting i.e. [{x}], {{x}}, [[x]], {[x]}
                            }
                        }
                        return term // instances and properties are allowed one component
                    }
//                    print("here", term)
                    if connector == .x || connector == .i || connector == .e {
                        return term
                    }
                    return nil
                }
                if checkOverlap && j1.evidenceOverlap(j2) {
                    return nil
                }
                if connector == .x || connector == .i || connector == .e {
                    return term
                }
                guard let compound = ç.connect(terms[0], connector, terms[1]) else {
                    return nil // invalid compound
                }
                return compound
                
            case .statement(let subject, let cop, let predicate):
                if let sub = validate(subject), let pre = validate(predicate) {
//                    if case .compound(let cs, _) = subject, case .compound(let cp, _) = predicate {
//                        if cs == .x && cp == .x {
//                            return nil
//                        }
//                    }
                    return .statement(sub, cop, pre)
                }
                return nil
                
            default:
                return term
            }
        }
    }
}



// MARK: - Variable introduction and elimination

private func variableEliminationIndependent(_ t1: Statement, _ t2: Statement) -> Statement {
    var t1 = t1
    if case .statement(let sub1, let cop1, let pre1) = t1, cop1 == .implication || cop1 == .equivalence {
        if false == sub1.terms.contains(where: { if case .variable = $0 { return true } ; return false }) {
            return t1
        }
        
        if let (rep1, rep2) = helper((sub1, pre1), t2) {
            
            t1 = .statement(rep1, cop1, rep2)
        }
    }
    return t1
}

private func variableEliminationDependent(_ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?]? {
    if case .compound(let conn, let terms) = t1, conn == .c || conn == .U || conn == .Ω {
        if terms.count == 2 { // TODO: handle compounds with more terms
            
            if let (rep1, rep2) = helper((terms[0], terms[1]), t2) {
            
                let tv = TruthValue.deduction(j1.truthValue, TruthValue(1, reliance))
                let jr1 = Judgement(rep1, tv, j1.derivationPath)
                let jr2 = Judgement(rep2, tv, j1.derivationPath)
                
                let t1s = r.allRules.flatMap { r in
                    [rule_generator(r)((jr1, jr2)),
                     rule_generator(r)((jr2, jr1))]
                }.compactMap { $0 }

                var x: [Judgement?] = []

                for t1j in t1s {
                    x.append(contentsOf: Rules.allCases.flatMap { r in
                        r.apply((t1j, j2))
                    })
                }
                
                return x.isEmpty ? nil : x
            }
        }
    }
    return nil
}

private func variableIntroductionIndependent(_ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?] {
    variableIntroduction(dependent: false, t1, t2, j1, j2, r)
}

private func variableIntroductionDependent(_ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?] {
    variableIntroduction(dependent: true, t1, t2, j1, j2, r)
}

private func variableIntroduction(dependent: Bool, _ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?] {
    var x: [Judgement?] = []
    if case .statement(_, let cop1, _) = t1, cop1 == .inheritance,
       case .statement(_, let cop2, _) = t2, cop2 == .inheritance {
        
        let common = Set(t1.terms).intersection(t2.terms)
        
        if !common.isEmpty {
            
            if dependent { checkOverlap = false }
            
            var vari = r.conditional.flatMap { r in
                [rule_generator(r)((j1, j2)),
                 rule_generator(r)((j2, j1))] // switch order of premises
            }.compactMap { $0 }
            
            if dependent == false { // Table 10.3
                vari.append(contentsOf: r.variable_and_temporal.flatMap { r in
                    [rule_generator(r)((j1, j2)),
                     rule_generator(r)((j2, j1))] // switch order of premises
                }.compactMap { $0 })
            }
            
            if dependent { checkOverlap = true }
            
            let rep: [Judgement] = vari.compactMap { j in
                var r = j.statement
                for (i, c) in common.enumerated() {
                    if dependent {
                        r = r.replace(termName: c.description, depVarName: "x\(i)")
                    } else {
                        r = r.replace(termName: c.description, indepVarName: "x\(i)")
                    }
                }
                if j.statement.description == r.description {
                    return nil // variable substitution was not successful
                }
                return Judgement(r, j.truthValue, j.derivationPath)
            }
            
            x.append(contentsOf: rep)
        }
    }
    return x
}


private func helper(_ terms: (Term, Term), _ other: Term) -> (Term, Term)? {
    let vars1: [LogicTerm] = terms.0.terms.map {
        if case .variable(let v) = $0 {
            return LogicVariable(named: v.name ?? "_") // TODO: properly handle anonymous variables
        }
        return LogicValue($0.description)
    }
    let vars2: [LogicTerm] = terms.1.terms.map {
        if case .variable(let v) = $0 {
            return LogicVariable(named: v.name ?? "_") // TODO: properly handle anonymous variables
        }
        return LogicValue($0.description)
    }
    
    let vari1: [String] = terms.0.terms.compactMap({ if case .variable(let v) = $0 { return v.name } ; return nil })
    let vari2: [String] = terms.1.terms.compactMap({ if case .variable(let v) = $0 { return v.name } ; return nil })

    let variInt = Set(vari1).intersection(Set(vari2))
    if variInt.isEmpty {
        return nil
    }
    
    let vars3: [LogicTerm] = other.terms.map { LogicValue($0.description) }
    
    var ll1: List = .empty
    for v in vars1.reversed() {
        ll1 = List.cons(v, ll1)
    }
    
    var ll2: List = .empty
    for v in vars2.reversed() {
        ll2 = List.cons(v, ll2)
    }
    
    var ll3: List = .empty
    for v in vars3.reversed() {
        ll3 = List.cons(v, ll3)
    }
    
    var sol = Dictionary<String, String>()
    
    for varName in variInt {
        sol[varName] = ""
    }
    
    var solved = false
    for s in solve((ll1 === ll3) || (ll2 === ll3)) {
        if solved == false { solved = true }
        for v in variInt.map({LogicVariable(named: $0)}) {
            let sub = s[v]
            if !sub.equals(v) { // LogicKit will return self by default
                sol[v.name] = "\(sub)"
            }
        }
    }
    
    if !solved {
        return nil
    }
//                print(">>>+++===[[", sol)
    
    var rep1 = terms.0
    var rep2 = terms.1
    for (k, v) in sol {
        rep1 = rep1.replace(varName: k, termName: v)
        rep2 = rep2.replace(varName: k, termName: v)
//                    print("+++\n", terms[0], rep1, "\n", terms[1], rep2)
    }
    
    return (rep1, rep2)
}
