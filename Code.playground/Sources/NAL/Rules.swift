
/// Swift Tuple is a basic primitive
typealias Triple = (Bool?, Bool?, Bool?)
typealias Quad   = (Bool?, Bool?, Bool?, Bool?)

/// Statement is a fundamental type
public typealias Rule = (Statement, Statement, Statement, TruthFunction)
public typealias Apply = (_ judgements: (Judgement, Judgement)) -> Judgement? // reduce operation

public enum Rules: String, CaseIterable {
    case identity
    // NAL-1
    case deduction
    case induction
    case abduction
    case conversion
    case exemplification
    // NAL-2
    case comparison
    case analogy
    case resemblance
    // Compositional
    case intersection
    case union
    case difference
}

extension Rules {
    var tf: TruthFunction {
        TruthValue.truthFunction(self)
    }
    public var apply: (_ judgements: (Judgement, Judgement)) -> [Judgement?] {
        { j in
            self.rule.map { r in
                rule_generator(r)(j)
            }
        }
    }
}

// MARK: Rule application

let rule_generator: (_ rule: Rule) -> Apply = { (arg) -> ((Judgement, Judgement)) -> Judgement? in
    var (p1, p2, c, tf) = arg
    
    // add implicit terms
    if case .term(let t) = p1 {
        p1 = .statement(.word("E"), .implication, t)
    } else if case .term(let t) = p2 {
        p2 = .statement(.word("E"), .implication, t)
    }
    
    let commonTerms = identifyCommonTerms((p1, p2))
    let total = countTruths(in: commonTerms)
    if //total == 4 || // .identity
        total < 2 { // no conclusion
        return { _ in nil }
    }
    
    return { (arg) in
        let (j1, j2) = arg
        let t1 = j1.statement // test
        let t2 = j2.statement // test
//        print(p1, p2, j1, j2)
//        print("=", commonTerms)
//        return nil
        
        
        if p1.isTautology && !t1.isTautology || !p1.isTautology && t1.isTautology {
            return nil
        }
        
        if p2.isTautology && !t2.isTautology || !p2.isTautology && t2.isTautology {
            return nil
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
        
        let s1 = Statement(ct1!, c1, ct2!)
        let s2 = Statement(ct3!, c2, ct4!)
        
//        print(s1, s2)
//        print("))))", c.predicate)
        
        //        
//        case .deduction:
//        ///    true, false,      nil, true
//        return (M --> P,         S --> M, S --> P, tf)
//                bird-->animal     robin-->bird
        
//        let s1 = Statement(t1.subject, p1.copula, t1.predicate)
//        let s2 = Statement(
//            commonTerms.2 == true ? commonTerms.0 == true ?
//            t1.subject : t1.predicate : commonTerms.1 == true ? "null"• : t2.subject
//        , p2.copula, // validate copula
//            commonTerms.3 == true ? commonTerms.0 == true ?
//            t1.subject : t1.predicate : commonTerms.1 == true ? "null"• : t2.predicate
//        )

        if s1 == t1, s2 == t2 {
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
                let compound = ç.connect(pTerm1, ct, pTerm2)
                statement = Statement(sTerm, cc, compound)
                
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
                    let compound = ç.connect(sTerm1, ct, sTerm2)
                    statement = Statement(compound, cc, pTerm)
                
            } else {
                let subject = firstIndex(of: cs, in: terms)!
                let predicate = firstIndex(of: cp, in: terms)!
                terms = t1.terms + t2.terms
                statement = Statement(term(at: subject, in: terms)!, cc, term(at: predicate, in: terms)!)
            }
            
            // remove implicit terms
            if case .statement(let s, let c, let p) = statement, s == .word("E"), c == .implication {
                statement = .term(p)
            }
            
            let truthValue = statement.isTautology ? TruthValue(1, 1) : tf(j1.truthValue, j2.truthValue)
            return Judgement(statement, truthValue)
        }
        return nil
    }
}

private var identifyCommonTerms: ((Statement, Statement)) -> Quad = { (arg) in
    let t1 = arg.0.terms
    let t2 = arg.1.terms
    let res = t1 + t2
    var out = Quad(nil, nil, nil, nil)
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

internal typealias Terms  = Quadra<Term>

public typealias Quadra<T: Equatable> = (T, T, T, T)

private func +(_ a: [Term], b: [Term]) -> Terms {
    assert(a.count == 2 && b.count == 2)
    return (a[0], a[1], b[0], b[1])
}

private func firstIndex<T>(of t: T, in q: Quadra<T>) -> Int? {
    (q.0 == t ? 0 :
    (q.1 == t ? 1 :
    (q.2 == t ? 2 :
    (q.3 == t ? 3 : nil))))
}

private func term(at i: Int, in q: Terms) -> Term? {
    (i == 0 ? q.0 :
    (i == 1 ? q.1 :
    (i == 2 ? q.2 :
    (i == 3 ? q.3 : nil))))
}

private func set(_ q: inout Quad, _ i: Int, _ value: Bool?) {
    (i == 0 ? q.0 = value :
    (i == 1 ? q.1 = value :
    (i == 2 ? q.2 = value :
    (i == 3 ? q.3 = value : () ))))
}

private func countTruths(in q: Quad) -> Int {
    var i = 0
    let x = true
    if q.0 == x { i += 1 }
    if q.1 == x { i += 1 }
    if q.2 == x { i += 1 }
    if q.3 == x { i += 1 }
    return i
}

