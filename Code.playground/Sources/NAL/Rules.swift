public typealias Rule  = (Statement, Statement, Statement, TruthFunction)
public typealias Apply = (_ judgements: (Judgement, Judgement)) -> Judgement?

public typealias Infer = (Judgement) -> Judgement? /// Single-premise rules

public enum Rules: String, CaseIterable {
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
        let immediate: [Infer] = [negation(j1:), conversion(j1:), contraposition(j1:)]
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
    
    public func backward(_ judgements: (Judgement, Judgement)) -> [Judgement?] {
        let (j1, j2) = judgements
        
        var x: [Judgement?] = []
        
        // apply rules
        self.allRules.forEach { r in
            let (p1, p2, c, tf) = r

            let q = j1
            let j = j2
            if let m = Term.match_backward(t: p1 => c, s: j.statement => q.statement, r: p2) {
                let rule = tf(.tautology, .tautology).rule
                let evidence = Judgement.mergeEvidence(q, j)
                x.append(Judgement(m, TruthValue(frequency: 1.0, confidence: 0.9, rule: rule), evidence))
            }
            if let m = Term.match_backward(t: p2 => c, s: j.statement => q.statement, r: p1) {
                let rule = tf(.tautology, .tautology).rule
                let evidence = Judgement.mergeEvidence(q, j)
                x.append(Judgement(m, TruthValue(frequency: 1.0, confidence: 0.9, rule: rule), evidence))
            }
        }
        
        let unique = x.compactMap({$0}).removeDuplicates()
        return unique
    }

    public func apply(_ judgements: (Judgement, Judgement)) -> [Judgement?] {
        var (j1, j2) = judgements
        
        var t1 = j1.statement // test
        var t2 = j2.statement // test
        //            print(p1, p2, j1, j2)
        
        if case .compound(let conn, let ts1) = t1, conn == .n {
            if ts1[0] == t2 { // TODO: use similarity helper to account for symmetrical connectors and copulas
                return [] // no conclusion can be reached if premises are just opposite of each other
            }
        }
        if case .compound(let conn, let ts2) = t2, conn == .n {
            if ts2[0] == t1 { // TODO: use similarity helper to account for symmetrical connectors and copulas
                return [] // no conclusion can be reached if premises are just opposite of each other
            }
        }

        /// variable elimination
        
        var x: [Judgement?] = []
        
        /// independent
        func variableEliminationIndependent(_ j1: Judgement, _ j2: Judgement) -> Statement {
            if let sub = Term.match(t: j1.statement, s: j2.statement), Term.validate(sub) != nil {
                let jsub = Judgement(sub, j1.truthValue, j1.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
                x.append(jsub)
                if case .statement(_, let cop1, _) = j1.statement, cop1.atemporal == .implication || cop1.atemporal == .equivalence {
                    return sub
                }
            }
            return j1.statement
        }
        t1 = variableEliminationIndependent(j1, j2)
        t2 = variableEliminationIndependent(j2, j1)

        /// dependent
        if let result = variableEliminationDependent(t1, t2, j1, j2, self) {
            return result
        } else if let result = variableEliminationDependent(t2, t1, j2, j1, self) {
            return result
        }
        
        /// rule application
        
        j1 = Judgement(t1, j1.truthValue, j1.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
        j2 = Judgement(t2, j2.truthValue, j2.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
                
        // apply rules
        self.allRules.forEach { r in
            x.append(rule_applicator(r)((j1, j2)))
        }
        // switch order of premises
        self.allRules.forEach { r in
            x.append(rule_applicator(r)((j2, j1)))
        }
        
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
        return unique
    }
}

// MARK: Rule application

public var rule_applicator: (_ rule: Rule) -> Apply {
    { (arg) -> ((Judgement, Judgement)) -> Judgement? in
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
            
            
            // TODO: handle temporal compounds
            // TODO: check that compounds do not contain each other
            
            /*
             * MARK: check results
             */
            
            if case .compound = result { // TODO: move this check elsewhere
                if j1.evidenceOverlap(j2) {
                    return nil
                }
            }
            if let statement = Term.validate(result), !statement.isTautology {
                let truthValue = tf(j1.truthValue, j2.truthValue)
                let derivationPath = Judgement.mergeEvidence(j1, j2)
                return Judgement(statement, truthValue, derivationPath, tense: j1.tense ?? j2.tense)
            }
            
            return nil // PROGRAM END
            
            
            // MARK: - helpers
            
            func temporalReasoning(_ t: Term) -> Term? {
                if j1.timestamp != ETERNAL, j2.timestamp != ETERNAL,
                   j1.timestamp != j2.timestamp, // TODO: investigate where is this coming from
                   case .statement(let cs, var cc, let cp) = t,
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
                        cc = cc.concurrent
                    } else if forward {
                        //                    print(">>", t)
                        cc = cc.predictive
                    } else {
                        //                    print("<<", t)
                        cc = cc.retrospective
                    }
                    
                    // use temporal conclusion
                    return .statement(cs, cc, cp)
                }
                
                return t // use original conclusion
            }
            
            func logicReasoning(_ t: Term) -> Term? {
                var result = t
                let test1: LogicGoal = (p1.logic() === j1.statement.logic())
                let test2: LogicGoal = (p2.logic() === j2.statement.logic())
                
                let substitution = solve(test1 && test2).makeIterator().next()
                
                // TODO: use LogicVariableFactory to avoid collision with terms
                // i.e. terms names "S" and "P" will fail a check below and produce no conclusion
                
                if let sol = substitution {
                    let ts = (p1.terms + p2.terms + c.terms).flatMap({ $0.terms.map({ $0.logic() }) })
                    let valid = sol.allSatisfy { (v, _) in
                        ts.contains { $0.equals(v) }
                    }
                    
                    if valid {
                        for item in sol {
                            result = result.replace(termName: item.LogicVariable.name, term: .from(logic: item.LogicTerm))
                        }
                    }
                }
                                
                return (result == t) ? nil : result
            }
            
            func determineOrder() -> Term {
                // TODO: get rid of this dirty trick and determine temporal order of the conclusion properly
                // or is this how it's supposed to be?
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
                            return .statement(cs, cc.retrospective, cp)
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
                        var list = [Term]()
                        
                        if j1c.isPredictive && j2c.isRetrospective {
                            list = [j1s, j1p]
                            if let idx = list.firstIndex(of: j2s) {
                                list.insert(j2p, at: idx)
                            }
                            if let idx = list.firstIndex(of: j2p) {
                                list.insert(j2s, at: idx+1)
                            }
                        } else if j2c.isPredictive && j1c.isRetrospective {
                            list = [j2s, j2p]
                            if let idx = list.firstIndex(of: j1s) {
                                list.insert(j1p, at: idx)
                            }
                            if let idx = list.firstIndex(of: j1p) {
                                list.insert(j1s, at: idx+1)
                            }
                        }
                        
                        if let cpi = list.firstIndex(of: cp), let csi = list.firstIndex(of: cs) {
                            if cpi > csi {
                                // predicate after subject
                                return .statement(cs, cc.predictive, cp)
                            } else {
                                // predicate before subject
                                return .statement(cs, cc.retrospective, cp)
                            }
                        }                    
                    }
                }
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
}


// MARK: - Variable introduction and elimination

private func variableEliminationDependent(_ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?]? {
    if case .compound(let conn, _) = t1, conn == .c || conn == .U || conn == .Ω {
        var x: [Judgement?] = []
        
        if let h = Term.match(t: t1, s: t2) {
            let tv = TruthValue.deduction(j1.truthValue, TruthValue(1, reliance))

            let res = r.allRules.flatMap { r in
                h.terms.flatMap { (t: Term) -> [Judgement?] in
                    let j = Judgement(t, tv, j1.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
                    return [rule_applicator(r)((j, j2)),
                            rule_applicator(r)((j2, j))]
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
            
            var vari = r.conditional.flatMap { r in
                [rule_applicator(r)((j1, j2)),
                 rule_applicator(r)((j2, j1))] // switch order of premises
            }.compactMap { $0 }
            
            if dependent == false { // Table 10.3
                vari.append(contentsOf: r.variable_and_temporal.flatMap { r in
                    [rule_applicator(r)((j1, j2)),
                     rule_applicator(r)((j2, j1))] // switch order of premises
                }.compactMap { $0 })
            }
                        
            let rep: [Judgement] = vari.compactMap { j in
                var r = j.statement
                for (i, c) in common.enumerated() {
                    var c = c
                    if case .compound(let con, let terms) = c {
                        if (con == .intSet || con == .extSet) && terms.count == 1 { // TODO: handle multiple
                            c = terms[0]
                        }
                    }
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
