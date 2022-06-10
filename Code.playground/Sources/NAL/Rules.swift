
/// Swift Tuple is a basic primitive
//typealias Triple = (Bool?, Bool?, Bool?)
typealias Quad<T: Equatable> = (T, T, T, T)

/// Statement is a fundamental type
public typealias Rule = (Statement, Statement, Statement, TruthFunction)
public typealias Apply = (_ judgements: (Judgement, Judgement)) -> Judgement? // reduce operation

public typealias Infer = (Judgement) -> Judgement?

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
            
        variableEliminationIndependent:
        if case .statement(let sub1, let cop1, let pre1) = t1, cop1 == .implication || cop1 == .equivalence {
            if false == sub1.terms.contains(where: { if case .variable = $0 { return true } ; return false }) {
                break variableEliminationIndependent
            }
            let vars1: [LogicTerm] = sub1.terms.map {
                if case .variable(let v) = $0 {
                    return LogicVariable(named: v.name ?? "_") // TODO: properly handle anonymous variables
                }
                return LogicValue($0.description)
            }
            let vars2: [LogicTerm] = pre1.terms.map {
                if case .variable(let v) = $0 {
                    return LogicVariable(named: v.name ?? "_") // TODO: properly handle anonymous variables
                }
                return LogicValue($0.description)
            }
            
            let vari1: [String] = sub1.terms.compactMap({ if case .variable(let v) = $0 { return v.name } ; return nil })
            let vari2: [String] = pre1.terms.compactMap({ if case .variable(let v) = $0 { return v.name } ; return nil })
            
            let variInt = Set(vari1).intersection(Set(vari2))
            if variInt.isEmpty {
                break variableEliminationIndependent
            }

            let vars3: [LogicTerm] = t2.terms.map {
                if case .variable(let v) = $0 {
                    return LogicVariable(named: v.name ?? "_") // TODO: properly handle anonymous variables
                }
                return LogicValue($0.description)
            }
            
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
                break variableEliminationIndependent
            }
//            print(">>>+++===", sol)
            
            var rep1 = sub1
            var rep2 = pre1
            for (k, v) in sol {
                rep1 = rep1.replace(varName: k, termName: v)
                rep2 = rep2.replace(varName: k, termName: v)
//                print("+++\n", sub1, rep1, "\n", pre1, rep2)
            }
            
            t1 = .statement(rep1, cop1, rep2)
        }
            
        variableEliminationIndependent2:
        if case .statement(let sub1, let cop1, let pre1) = t2, cop1 == .implication || cop1 == .equivalence {
            if false == sub1.terms.contains(where: { if case .variable = $0 { return true } ; return false }) {
                break variableEliminationIndependent2
            }
            let vars1: [LogicTerm] = sub1.terms.map {
                if case .variable(let v) = $0 {
                    return LogicVariable(named: v.name ?? "_") // TODO: properly handle anonymous variables
                }
                return LogicValue($0.description)
            }
            let vars2: [LogicTerm] = pre1.terms.map {
                if case .variable(let v) = $0 {
                    return LogicVariable(named: v.name ?? "_") // TODO: properly handle anonymous variables
                }
                return LogicValue($0.description)
            }
            
            let vari1: [String] = sub1.terms.compactMap({ if case .variable(let v) = $0 { return v.name } ; return nil })
            let vari2: [String] = pre1.terms.compactMap({ if case .variable(let v) = $0 { return v.name } ; return nil })
            
            let variInt = Set(vari1).intersection(Set(vari2))
            if variInt.isEmpty {
                break variableEliminationIndependent2
            }

            let vars3: [LogicTerm] = t1.terms.map {
                if case .variable(let v) = $0 {
                    return LogicVariable(named: v.name ?? "_") // TODO: properly handle anonymous variables
                }
                return LogicValue($0.description)
            }
            
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
                break variableEliminationIndependent2
            }
//            print(">>>+++===", sol)
            
            var rep1 = sub1
            var rep2 = pre1
            for (k, v) in sol {
                rep1 = rep1.replace(varName: k, termName: v)
                rep2 = rep2.replace(varName: k, termName: v)
//                print("+++\n", sub1, rep1, "\n", pre1, rep2)
            }
            
            t2 = .statement(rep1, cop1, rep2)
        }
            
            
        variableEliminationDependent:
        if case .compound(let conn, let terms) = t1, conn == .c || conn == .U || conn == .Ω {
            if terms.count == 2 { // TODO: handle compounds with more terms
                let vars1: [LogicTerm] = terms[0].terms.map {
                    if case .variable(let v) = $0 {
                        return LogicVariable(named: v.name ?? "_") // TODO: properly handle anonymous variables
                    }
                    return LogicValue($0.description)
                }
                let vars2: [LogicTerm] = terms[1].terms.map {
                    if case .variable(let v) = $0 {
                        return LogicVariable(named: v.name ?? "_") // TODO: properly handle anonymous variables
                    }
                    return LogicValue($0.description)
                }
                
                let vari1: [String] = terms[0].terms.compactMap({ if case .variable(let v) = $0 { return v.name } ; return nil })
                let vari2: [String] = terms[1].terms.compactMap({ if case .variable(let v) = $0 { return v.name } ; return nil })

                let variInt = Set(vari1).intersection(Set(vari2))
                if variInt.isEmpty {
                    break variableEliminationDependent
                }
                
                let vars3: [LogicTerm] = t2.terms.map { LogicValue($0.description) }
                
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
                    break variableEliminationDependent
                }
//                print(">>>+++===[[", sol)
                
                var rep1 = terms[0]
                var rep2 = terms[1]
                for (k, v) in sol {
                    rep1 = rep1.replace(varName: k, termName: v)
                    rep2 = rep2.replace(varName: k, termName: v)
//                    print("+++\n", terms[0], rep1, "\n", terms[1], rep2)
                }
                
                let tv = TruthValue.deduction(j1.truthValue, TruthValue(1, reliance))
                let jr1 = Judgement(rep1, tv, j1.derivationPath)
                let jr2 = Judgement(rep2, tv, j1.derivationPath)
                
                let t1s = self.allRules.flatMap { r in
                    [rule_generator(r)((jr1, jr2)),
                     rule_generator(r)((jr2, jr1))]
                }.compactMap { $0 }
//                print("t1s", t1s)
                for t1j in t1s {
                    x.append(contentsOf: Rules.allCases.flatMap { r in
                        r.apply((t1j, j2))
//                        [rule_generator(r)((t1j, j2)),
//                         rule_generator(r)((j2, t1j))] // switch order of premises
                    })
                }
                
                return x
//                t1 = .compound(conn, [rep1, rep2])
            }
        }
            
        variableEliminationDependent2:
        if case .compound(let conn, let terms) = t2, conn == .c || conn == .U || conn == .Ω {
            if terms.count == 2 { // TODO: handle compounds with more terms
                let vars1: [LogicTerm] = terms[0].terms.map {
                    if case .variable(let v) = $0 {
                        return LogicVariable(named: v.name ?? "_") // TODO: properly handle anonymous variables
                    }
                    return LogicValue($0.description)
                }
                let vars2: [LogicTerm] = terms[1].terms.map {
                    if case .variable(let v) = $0 {
                        return LogicVariable(named: v.name ?? "_") // TODO: properly handle anonymous variables
                    }
                    return LogicValue($0.description)
                }
                
                let vari1: [String] = terms[0].terms.compactMap({ if case .variable(let v) = $0 { return v.name } ; return nil })
                let vari2: [String] = terms[1].terms.compactMap({ if case .variable(let v) = $0 { return v.name } ; return nil })

                let variInt = Set(vari1).intersection(Set(vari2))
                if variInt.isEmpty {
                    break variableEliminationDependent2
                }
                
                let vars3: [LogicTerm] = t1.terms.map { LogicValue($0.description) }
                
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
                    break variableEliminationDependent2
                }
//                print(">>>+++===[[", sol)
                
                var rep1 = terms[0]
                var rep2 = terms[1]
                for (k, v) in sol {
                    rep1 = rep1.replace(varName: k, termName: v)
                    rep2 = rep2.replace(varName: k, termName: v)
//                    print("+++\n", terms[0], rep1, "\n", terms[1], rep2)
                }
                
                
                let jr1 = Judgement(rep1, j2.truthValue, j2.derivationPath)
                let jr2 = Judgement(rep2, j2.truthValue, j2.derivationPath)
                
                let t2s = self.allRules.flatMap { r in
                    [rule_generator(r)((jr1, jr2)),
                     rule_generator(r)((jr2, jr1))]
                }.compactMap { $0 }
//                print("\n", self)
//                print("t2s", t2s)
                for t2j in t2s {
                    x.append(contentsOf: Rules.allCases.flatMap { r in
                        r.apply((j1, t2j))
//                        [rule_generator(r)((j1, t2j)),
//                         rule_generator(r)((t2j, j1))] // switch order of premises
                    })
                }
//                print("x", j1, x)
                return x
//                t2 = .compound(conn, [rep1, rep2])
            }
        }
        
            
            /// original code
            
            j1 = Judgement(t1, j1.truthValue, j1.derivationPath)
            j2 = Judgement(t2, j2.truthValue, j2.derivationPath)
            
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
                if case .statement(_, let cop1, _) = t1, cop1 == .inheritance,
                   case .statement(_, let cop2, _) = t2, cop2 == .inheritance {
                    
                    if let common = firstIndex(of: true, in: identifyCommonTerms((t1, t2))),
                       let xc = term(at: common, in: (t1.terms + t2.terms)) {
                        
                        let vari = variable_and_temporal.flatMap { r in
                            [rule_generator(r)((j1, j2)),
                             rule_generator(r)((j2, j1))] // switch order of premises
                        }.compactMap { $0 }
                        
                        let rep: [Judgement] = vari.compactMap { j in
                            let r = j.statement.replace(termName: xc.description, indepVarName: "x")
                            if j.statement.description == r.description {
                                return nil // variable substitution was not successful
                            }
                            return Judgement(r, j.truthValue, j.derivationPath)
                        }
                        
                        x.append(contentsOf: rep)
                    }
                }
            }
            
            /// dependent-variable introduction
            
            if self == .intersection {
                if case .statement(_, let cop1, _) = t1, cop1 == .inheritance,
                   case .statement(_, let cop2, _) = t2, cop2 == .inheritance {

                    if let common = firstIndex(of: true, in: identifyCommonTerms((t1, t2))),
                       let xc = term(at: common, in: (t1.terms + t2.terms)) {

                        checkOverlap = false
                        let vari = conditional.flatMap { r in
                            [rule_generator(r)((j1, j2)),
                             rule_generator(r)((j2, j1))] // switch order of premises
                        }.compactMap { $0 }
                        checkOverlap = true
                        
                        let rep: [Judgement] = vari.compactMap { j in
                            let r = j.statement.replace(termName: xc.description, depVarName: "x")
                            if j.statement.description == r.description {
                                return nil // variable substitution was not successful
                            }
                            return Judgement(r, j.truthValue, j.derivationPath)
                        }
                        
                        x.append(contentsOf: rep)

                    }
                }
            }
            
            // TODO: multi-variable introduction rules
            
            let unique = Dictionary(grouping: x.flatMap({$0})) {
                $0.statement.description
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

    var p1I = false
    var p2I = false

    // add implicit terms
    switch p1 {
    case .symbol: fallthrough
    case .compound:
        p1 = .statement(.symbol("E"), .implication, p1)
        p1I = true
    case .statement: fallthrough
    case .variable: fallthrough
    case .operation:
        break
    }
    switch p2 {
    case .symbol: fallthrough
    case .compound:
        p2 = .statement(.symbol("E"), .implication, p2)
        p2I = true
    case .statement: fallthrough
    case .variable: fallthrough
    case .operation:
        break
    }
    switch c {
    case .symbol: fallthrough
    case .compound:
        c = .statement(.symbol("E"), .implication, c)
    case .statement: fallthrough
    case .variable: fallthrough
    case .operation:
        break
    }

    let commonTerms = identifyCommonTerms((p1, p2))
    let total = countTruths(in: commonTerms)
    
    if total < 2 { // no conclusion
        return { _ in nil }
    }
    
    return { (arg) in
        let (j1, j2) = arg
        var t1 = j1.statement // test
        var t2 = j2.statement // test
//        print(p1, p2, j1, j2)
//        print("=", commonTerms)
//        return nil

        
        if p1I {
            t1 = .statement(.symbol("E"), .implication, t1)
        }
        if p2I {
            t2 = .statement(.symbol("E"), .implication, t2)
        }
        
        let first = firstIndex(of: false, in: commonTerms)! // 1
        let common = firstIndex(of: true, in: commonTerms)! // 0
        let second = firstIndex(of: nil, in: commonTerms) ?? common // 2
        let x1 = term(at: first, in: (t1.terms + t2.terms)) // animal
        let xc = term(at: common, in: (t1.terms + t2.terms)) // bird
        let x2 = term(at: second, in: (t1.terms + t2.terms)) // robin
        
        let ct1 = commonTerms.0 == true ? xc : // bird
            commonTerms.0 == false ? x1 : // animal
            x2 // robin
        let ct2 = commonTerms.1 == true ? xc : // bird
            commonTerms.1 == false ? x1 : // animal
            x2 // robin
        let ct3 = commonTerms.2 == true ? xc : // bird
            commonTerms.2 == false ? x1 : // animal
            x2 // robin
        let ct4 = commonTerms.3 == true ? xc : // bird
            commonTerms.3 == false ? x1 : // animal
            x2 // robin
        
        guard case .statement(_, let c1, _) = p1,
              case .statement(_, let c2, _) = p2 else {
            return nil // should never happen
        }
        
        let s1: Statement = .statement(ct1!, c1, ct2!)
        let s2: Statement = .statement(ct3!, c2, ct4!)
//        print("\n---", s1, s2)
//        print("===", t1, t2)
//        print(s1 == t1, s2 == t2)
//        if "\(s1) \(s2)" == "E => ({Birdie} -> {Tweety} ∧ {Tweety} -> {Birdie}) ({Birdie} -> {Tweety} ∧ {Tweety} -> {Birdie}) <=> ({Birdie} <–> {Tweety})" {
//            print("yo")
//        }

        if s1 == t1, s2 == t2 {
//            print("here")
            // conclusion
            var statement: Statement!
            var terms = p1.terms + p2.terms
            
            // from original conclusion (c) get
            // subject (cs) copula (cc) predicate (cp)
            guard case .statement(let cs, let cc, let cp) = c else {
                return nil // should never happen
            }
            
            if case .compound(let ct, let ts) = cp, ts.count == 2 { // compound term
                
                // TODO: check that compounds do not contain each other
                
                // apply composition
                let subject = firstIndex(of: cs, in: terms)! // M, 0
                let pT1 = firstIndex(of: ts[0], in: terms)!
                let pT2 = firstIndex(of: ts[1], in: terms)!
                terms = t1.terms + t2.terms
                let sTerm = term(at: subject, in: terms)!
                let pTerm1 = term(at: pT1, in: terms)!
                let pTerm2 = term(at: pT2, in: terms)!
                if checkOverlap && j1.evidenceOverlap(j2) {
                    return nil
                }
                guard let compound = ç.connect(pTerm1, ct, pTerm2) else {
                    return nil // invalid compound
                }
                statement = .statement(sTerm, cc, compound)
                
            } else if case .compound(let ct, let ts) = cs, ts.count == 2 { // compound term
                    
                    // TODO: check that compounds do not contain each other
                    
                    // apply composition
                    let predicate = firstIndex(of: cp, in: terms)! // M, 0
                    let sT1 = firstIndex(of: ts[0], in: terms)!
                    let sT2 = firstIndex(of: ts[1], in: terms)!
                    terms = t1.terms + t2.terms
                    let pTerm = term(at: predicate, in: terms)!
                    let sTerm1 = term(at: sT1, in: terms)!
                    let sTerm2 = term(at: sT2, in: terms)!
                    if checkOverlap && j1.evidenceOverlap(j2) {
                        return nil
                    }
                    guard let compound = ç.connect(sTerm1, ct, sTerm2) else {
                        return nil // invalid compound
                    }
                    statement = .statement(compound, cc, pTerm)
                
            } else {
                let subject = firstIndex(of: cs, in: terms)!
                let predicate = firstIndex(of: cp, in: terms)!
                terms = t1.terms + t2.terms
                statement = .statement(term(at: subject, in: terms)!, cc, term(at: predicate, in: terms)!)
            }
            
            if statement.isTautology {
                return nil
            }
            
            // remove implicit terms
            if case .statement(let s, let c, let p) = statement, s == .symbol("E"), c == .implication {
                statement = p
            }
            
            let truthValue = tf(j1.truthValue, j2.truthValue)
            let derivationPath = Judgement.mergeEvidence(j1, j2)
            return Judgement(statement, truthValue, derivationPath)
        }
        return nil
    }
}



// MARK: Helpers

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
