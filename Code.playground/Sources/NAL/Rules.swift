
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
    public var apply: (_ judgements: (Judgement, Judgement)) -> [Judgement?] {
        { j in
            var (j1, j2) = j
//            print("\n>>>", j)
            
            var t1 = j1.statement // test
            var t2 = j2.statement // test
    //        print(p1, p2, j1, j2)
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
            
            /// independent
            t1 = variableEliminationIndependent(t1, t2)
            t2 = variableEliminationIndependent(t2, t1)
             
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
        
        // temporal
        // TODO: need to check copulas to ensure temporal order can be established
        if j1.timestamp != ETERNAL, j2.timestamp != ETERNAL,
           case .statement(var cs, var cc, var cp) = c,
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
//                    print("||", c)
                if cc == .implication {
                    cc = .concurrentImp
                } else if cc == .equivalence {
                    cc = .concurrentEq
                }
            } else if forward {
//                    print(">>", c)
                if cc == .implication {
                    cc = .predictiveImp
                } else if cc == .equivalence {
                    cc = .predictiveEq
                }
            } else {
//                    print("<<", c)
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
            c = .statement(cs, cc, cp)
        }

        var result = c // conclusion

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
//        print("}}}}", j1, j2, result)
        if result == c { // no conclusion
            return nil
        }
        
        // helper
        func validate(_ term: Term) -> Term? {
            switch term {
                
            case .compound(let connector, let terms):
                if terms.count != 2 { // TODO: handle multiple components
                    if terms.count == 1, connector == .intSet || connector == .extSet {
                        return term // instances and properties are allowed one component
                    }
                    return nil
                }
                if checkOverlap && j1.evidenceOverlap(j2) {
                    return nil
                }
                guard let compound = ç.connect(terms[0], connector, terms[1]) else {
                    return nil // invalid compound
                }
                return compound
                
            case .statement(let subject, let cop, let predicate):
                if let sub = validate(subject), let pre = validate(predicate) {
                    return .statement(sub, cop, pre)
                }
                return nil
                
            default:
                return term
            }
        }
        
        // TODO: handle temporal compounds
        // TODO: check that compounds do not contain each other

//        print("here", result)
        if let statement = validate(result) {
//            print("accepted", result)
            
            if statement.isTautology {
                return nil
            }

            let truthValue = tf(j1.truthValue, j2.truthValue)
            let derivationPath = Judgement.mergeEvidence(j1, j2)
            
//            print("accepted", statement, truthValue)
            
            return Judgement(statement, truthValue, derivationPath, tense: j1.tense ?? j2.tense)
        }
        
        return nil
    }
}


// MARK: Helpers -- TODO: REMOVE -- used in variable introduction only prior to new rule_generator

private var identifyCommonTerms: ((Statement, Statement)) -> Quad<Bool?> = { (arg) in
    let t1 = arg.0.terms
    let t2 = arg.1.terms
    let res = t1 + t2
    var out = Quad<Bool?>(nil, nil, nil, nil)
    var tmp = Array<Term>()
    Array(0..<3+1)
        .compactMap { i in
            let t = term(at: i, in: res)
            return t == nil ? nil : (i, t!)
        }
        .forEach { (arg: (Int, Term)) in
            let (i, t) = arg
            if i == 0 {
                tmp.append(t)
                set(&out, i, false)
            }
            if i == 1 && !helper(i, t) {
                tmp.append(t)
                set(&out, i, false)
            }
            if i == 2 && !helper(i, t) {
                tmp.append(t)
                set(&out, i, nil)
            }
            if i == 3 { 
                helper(i, t)             
//                set(&out, i, nil)
            }
        }
        @discardableResult
        func helper(_ i: Int, _ t: Term) -> Bool {
            if tmp.contains(t) {
                if let idx = firstIndex(of: t, in: res) {
                    set(&out, idx, true)
                }
                set(&out, i, true)
                return true
            }
            return false
        }
 /*
        .forEach { (arg: (Int, Term)) in
            let (i, t) = arg
            let seen = helper(i, t)
            tmp.append(t)
            set(&out, i, false)
            if !seen {
                tmp.append(t)
                set(&out, i, nil)
            }
    }
    func helper(_ i: Int, _ t: Term) -> Bool {
        if !tmp.contains(t) { return false }
        let idx = firstIndex(of: t, in: res)
        if idx != nil { set(&out, idx!, true) }
        set(&out, i, true)
        return true
    }
 */
    return out
}


// MARK: Utility

private func +(_ a: [Term], b: [Term]) -> Quad<Term> {
    let a = (a.count == 1) ? a + [.NULL] : a
    let b = (b.count == 1) ? b + [.NULL] : b
    assert(a.count == 2 && b.count == 2)
    return (a[0], a[1], b[0], b[1])
}

private func firstIndex<T>(of t: T, in q: Quad<T>) -> Int? {
    (q.0 == t ? 0 :
    (q.1 == t ? 1 :
    (q.2 == t ? 2 :
    (q.3 == t ? 3 : nil))))
}

private func term(at i: Int, in q: Quad<Term>) -> Term? {
    (i == 0 ? q.0 :
    (i == 1 ? q.1 :
    (i == 2 ? q.2 :
    (i == 3 ? q.3 : nil))))
}

private func set(_ q: inout Quad<Bool?>, _ i: Int, _ value: Bool?) {
    (i == 0 ? q.0 = value :
    (i == 1 ? q.1 = value :
    (i == 2 ? q.2 = value :
    (i == 3 ? q.3 = value : () ))))
}

private func countTruths(in q: Quad<Bool?>) -> Int {
    var i = 0
    let x = true
    if q.0 == x { i += 1 }
    if q.1 == x { i += 1 }
    if q.2 == x { i += 1 }
    if q.3 == x { i += 1 }
    return i
}

// MARK: Variable introduction and elimination

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

        if let common = firstIndex(of: true, in: identifyCommonTerms((t1, t2))),
           let xc = term(at: common, in: (t1.terms + t2.terms)) {

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
                var r: Term
                if dependent {
                    r = j.statement.replace(termName: xc.description, depVarName: "x")
                } else {
                    r = j.statement.replace(termName: xc.description, indepVarName: "x")
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


public func helper(_ terms: (Term, Term), _ other: Term) -> (Term, Term)? {
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
