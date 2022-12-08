

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
            return LogicValue(self)
        case .compound(let c, let terms):
            return List.cons(LogicValue(c), terms.toList())
        case .statement(let s, let c, let p):
            return List.cons(LogicValue(c.atemporal), List.cons(s.logic(), List.cons(p.logic(), List.empty)))
            
        case .variable(let vari):
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
        if let value = logic as? LogicValue<Term> {
            return value.extract()
        }
        if let variable = logic as? LogicVariable {
            if let vari = Variable(variable.name) {
                return .variable(vari)
            }
        }
        if let list = logic as? List {
            if case .cons(let head, let tail) = list {
                if let value = head as? LogicValue<Connector> { // compound
                    return .compound(value.extract(), process(list: tail))
                }
                
                if let value = head as? LogicValue<Copula> { // statement
                    let terms = process(list: tail)
                    return .statement(terms[0], value.extract(), terms[1])
                }
                
                if let value = head as? LogicValue<String> { // operation
                    return .operation(value.extract(), process(list: tail))
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
