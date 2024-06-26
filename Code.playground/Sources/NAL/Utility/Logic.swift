

extension Sequence where Element == Term {
    func toList() -> List {
        var list: List = .empty
        for term in self.reversed() {
            list = List.cons(term.logic(), list)
        }
        return list
    }
}


extension Term {
    func logic() -> LogicTerm {
        switch self {
        case .symbol:
            return self
        case .compound(let c, let terms):
            return List.cons(c, terms.toList())
        case .statement(let s, let c, let p):
            return List.cons(c, [s, p].toList())
        case .variable:
            return LogicVariable(named: self.description)
        case .operation(let op, let terms):
            return List.cons(LogicValue(op), terms.toList())
        }
    }
        
    static func from(logic: LogicTerm) -> Term {
        if let term = logic as? Term {
            return term
        }
        if let variable = logic as? LogicVariable {
            if let vari = Variable(variable.name) {
                return .variable(vari)
            }
        }
        if let list = logic as? List {
            if case .cons(let head, let tail) = list {
                if let connector = head as? Connector { // compound
                    return .compound(connector, process(list: tail))
                }
                
                if let copula = head as? Copula { // statement
                    let terms = process(list: tail)
                    return .statement(terms[0], copula, terms[1])
                }
                
                if let op = head as? LogicValue<String> { // operation
                    return .operation(op.wrapped, process(list: tail))
                }
            }
        }
        // helper
        func process(list: LogicTerm) -> [Term] {
            var terms: [Term] = []
            if case .cons(let head, let tail) = list as? List {
                terms.append(Term.from(logic: head))
                terms.append(contentsOf: process(list: tail))
            }
            return terms
        }
        
        return .NULL // DEFAULT
    }
}

extension Term: LogicTerm {}

extension Connector: LogicTerm {}

extension Copula: LogicTerm {
    func equals(_ other: LogicTerm) -> Bool {
        if let rhs = (other as? Copula) {
            return rhs.atemporal == self.atemporal
        }
        return false
    }
}


extension Term {
    public static func logic_match(t1: Term, t2: Term) -> Bool {
        let sol = solve(t1.logic() === t2.logic()).makeIterator().next()
        
        if sol == nil {
            return false
        }

        for item in sol! {
            let t = Term.from(logic: item.LogicTerm)
            let v = Term.from(logic: item.LogicVariable)
            if Term.getTerms(t).contains(v) {
                return false
            }
        }
        return true
    }

    public static func logic_solve(t1: Term, t2: Term) -> Term? {
        guard var result = ç.connect(t1, .c, t2) else {
            return nil
        }
        for sol in solve(t1.logic() === t2.logic()) {
            for item in sol {
                result = result.replace(termName: item.LogicVariable.name, term: .from(logic: item.LogicTerm))
            }
        }
        if result == ç.connect(t1, .c, t2) {
            return nil
        }
        if result.terms.count == 2 {
            if result.terms[0] == result.terms[1] {
                return result.terms[0]
            }
        }
        return nil
    }
}
