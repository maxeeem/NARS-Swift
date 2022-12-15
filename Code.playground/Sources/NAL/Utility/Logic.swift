

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
            return List.cons(c, List.cons(s.logic(), List.cons(p.logic(), List.empty)))
            
        case .variable:
            //        switch vari {
            //        case .independent(let name):
            //            return List.cons(LogicValue("var-ind"), List.cons(LogicVariable(named: name), List.empty))
            //        case .dependent(let name, let vars):
            //            var ll: List = .empty
            //            for v in vars.reversed() {
            //                ll = List.cons(LogicVariable(named: v), ll)
            //            }
            //            ll = List.cons(LogicValue("var-ind"), ll)
            //            return List.cons(LogicValue("var-dep"), List.cons(LogicVariable(named: name ?? "x()"), ll))
            //        }
            return LogicVariable(named: self.description) // TODO: handle nested variables
//            return LogicVariable(named: vari.name ?? "x")
            
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
