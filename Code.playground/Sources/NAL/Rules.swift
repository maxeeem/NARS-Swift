/// Statement is a fundamental type
public typealias Rule = (Statement, Statement, Statement, TruthFunction)
public typealias Apply = (_ judgements: (Judgement, Judgement)) -> Judgement? // reduce operation

public typealias Infer = (Judgement) -> Judgement? /// Single-premise rules

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

public extension Rules {
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
            
            if case .compound(let conn, let ts1) = t1, conn == .n {
//                print("1.", t1, t2)
                if ts1[0] == t2 { // TODO: use similarity helper to account for symmetrical connectors and copulas
                    return [] // no conclusion can be reached if premises are just opposite of each other
                }
            }
            if case .compound(let conn, let ts2) = t2, conn == .n {
//                print("2.", t1, t2)
                if ts2[0] == t1 { // TODO: use similarity helper to account for symmetrical connectors and copulas
                    return [] // no conclusion can be reached if premises are just opposite of each other
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
           
            var x: [Judgement?] = []
                        
            // apply rules
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
            
            let unique = x.compactMap({$0}).removeDuplicates()
//            print("+++", x)
//            print("===", unique)
            return unique
        }
    }
}

// MARK: Rule application

private var checkOverlap = false // TODO: dirty trick to get dependent-variable introduction to work

public let rule_generator: (_ rule: Rule) -> Apply = { (arg) -> ((Judgement, Judgement)) -> Judgement? in
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
        
        guard var result = logicReasoning(c) else {
            return nil // no conclusion
        }
        
        // TODO: come up with a better way
        result = determineOrder()
        result = accountForExemplification()
        
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
                if terms.count == 0 {
                    return nil // empty compound
                }
                if terms.count == 1 {
                    if connector == .intSet || connector == .extSet {
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
                return connector.connect(terms)
                
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
        
        func determineOrder() -> Term {
            // TODO: get rid of this dirty trick and determine temporal order of the conclusion properly
            if case .statement(var cs, var cc, var cp) = result, cc == .equivalence || cc == .implication {
                if case .statement(let j1s, let j1c, let j1p) = j1.statement,
                   case .statement(let j2s, let j2c, let j2p) = j2.statement {
                    
                    if (j1c.isConcurrent && j2c.isConcurrent) {
                        // both concurrent
                        return .statement(cs, cc.concurrent, cp)
                    }
                                        
                    if (j1c.isPredictive && j2c.isPredictive) {
                        // both predicitve
                        return .statement(cs, cc.predictive, cp)
                    }
                    
                    if (j1c.isRetrospective && j2c.isRetrospective) {
                        // both retrospective
                        if cc == .equivalence {
                            return .statement(cp, .predictiveEq, cs)
                        } else {
                            return .statement(cs, cc.retrospective, cp)
                        }
                    }
                    
                    if (j1c.isConcurrent && j2c.isPredictive)
                        || (j2c.isConcurrent && j1c.isPredictive) {
                        // one is concurrent, another is predictive
                        return .statement(cs, cc.predictive, cp)
                    }
                    
                    if (j1c.isConcurrent && j2c.isRetrospective)
                        || (j2c.isConcurrent && j1c.isRetrospective) {
                        // one is concurrent, another is retrospective
                        return .statement(cs, cc.retrospective, cp)
                    }
                    
                    // Complex
                    
                    if j1c.isPredictive && j2c.isRetrospective {
                        var list = [j1s, j1p]
                        if let idx = list.firstIndex(of: j2s) {
                            list.insert(j2p, at: idx)
                        }
                        if let idx = list.firstIndex(of: j2p) {
                            list.insert(j2s, at: idx+1)
                        }
                        if let cpi = list.firstIndex(of: cp), let csi = list.firstIndex(of: cs) {
                            if cpi > csi {
                                // predicate after subject
                                return .statement(cs, cc.predictive, cp)
                            } else {
                                // predicate before subject
                                if cc == .equivalence {
                                    return .statement(cp, .predictiveEq, cs)
                                } else {
                                    return .statement(cs, cc.retrospective, cp)
                                }
                            }
                        }
                    }
                    
                    if j2c.isPredictive && j1c.isRetrospective {
                        var list = [j2s, j2p]
                        if let idx = list.firstIndex(of: j1s) {
                            list.insert(j1p, at: idx)
                        }
                        if let idx = list.firstIndex(of: j1p) {
                            list.insert(j1s, at: idx+1)
                        }
                        if let cpi = list.firstIndex(of: cp), let csi = list.firstIndex(of: cs) {
                            if cpi > csi {
                                // predicate after subject
                                return .statement(cs, cc.predictive, cp)
                            } else {
                                // predicate before subject
                                if cc == .equivalence {
                                    return .statement(cp, .predictiveEq, cs)
                                } else {
                                    return .statement(cs, cc.retrospective, cp)
                                }
                            }
                        }
                    }
                }
            }
//            if result == (("John" * "key_101") --> "hold") {
//                // set tense for the conclusion
//            }
            return result
        }
        
        func accountForExemplification() -> Term {
            let rule = tf(j1.truthValue, j2.truthValue).rule

            if rule == .exemplification {
                if case .statement(let rs, let rc, let rp) = result {
                    if rc.isPredictive {
                        return .statement(rs, rc.atemporal.retrospective, rp)
                    } else if rc.isRetrospective {
                        return .statement(rs, rc.atemporal.predictive, rp)
                    }
                }
            }
            
            return result
        }
    }
}



// MARK: - Variable introduction and elimination

private func variableEliminationIndependent(_ t1: Statement, _ t2: Statement) -> Statement {
    if case .statement(_, let cop1, _) = t1, cop1 == .implication || cop1 == .equivalence {
        return Term.match(t: t1, s: t2) ?? t1
    }
    return t1
}

private func variableEliminationDependent(_ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?]? {
    if case .compound(let conn, _) = t1, conn == .c || conn == .U || conn == .Ω {
        var x: [Judgement?] = []
        
        if let h = Term.match(t: t1, s: t2) {
            let tv = TruthValue.deduction(j1.truthValue, TruthValue(1, reliance))

            let res = r.allRules.flatMap { r in
                h.terms.flatMap { (t: Term) -> [Judgement?] in
                    let j = Judgement(t, tv, j1.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
                    return [rule_generator(r)((j, j2)),
                            rule_generator(r)((j2, j))]
                }
            }
            
            for rs in res.compactMap({ $0 }) {
                x.append(contentsOf: Rules.allCases.flatMap { r in
                    r.apply((rs, j2))
                })
            }
        }
        
        return x.isEmpty ? nil : x
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
