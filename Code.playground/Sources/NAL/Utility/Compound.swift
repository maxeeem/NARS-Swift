
public typealias ç = Connector /// shorthand

extension Connector {
    var term: Term { Term.symbol(rawValue) }
    
    public static func e_(_ r: Term, _ t1: Term, _ t2: Term) -> Term { connect(.compound(.x, [r]), .e, .compound(.x, [t1, t2])) }
    public static func i_(_ r: Term, _ t1: Term, _ t2: Term) -> Term { connect(.compound(.x, [r]), .i, .compound(.x, [t1, t2])) }

    public func image(_ r: Term, _ t1: Term, _ t2: Term) -> Term {
        if self == .e || self == .i {
            return ç.connect(.compound(.x, [r]), self, .compound(.x, [t1, t2]))
        }
        return .NULL
    }
    
    public func connect(_ ts: [Term]) -> Term! {
        var ts = ts
        if ts.count < 2 {
            return nil // invalid compound
        }
        if ts.count > 2, let tail = ts.popLast(), let head = connect(ts) {
            return ç.connect(head, self, tail)
        }
        return ç.connect(ts[0], self, ts[1])
    }
    
    internal static func connect(_ t1: Term, _ c: Connector, _ t2: Term) -> Term! {
        var con = c
        let t1t = (c == .c || c == .d) ? Set([Term.symbol(t1.description)]) : Set(t1.terms)
        let t2t = (c == .c || c == .d) ? Set([Term.symbol(t2.description)]) : Set(t2.terms)
        var res = t1t.union(t2t)
        
        if c == .c || c == .d {
            if case .compound(let c1, _) = t1, (c1 == .c || c1 == .d) {
                return nil
            }
            if case .compound(let c2, _) = t2, (c2 == .c || c2 == .d) {
                return nil
            }
        }
        
        guard case .compound = t1, case .compound = t2, (c != .c || c != .d) else {
            // at least one term is a simple term
            if c != .x { // product can contain subproducts with repeating elements
                guard t1t.intersection(t2t).isEmpty else {
                    return nil // terms should not contain each other
                }
            }
            return validate(res) ? .compound(c, [t1, t2]) : nil
        }
        
        // TODO: should we be filtering terms by intensional/extension
        if [.intSet, .extSet, .Ω, .U, .l, .ø].contains(c) {
            if case .compound(let c1, _) = t1,
               case .compound(let c2, _) = t2 {
                if c1 == .x || c1 == .e || c1 == .i
                    || c2 == .x || c2 == .e || c2 == .i {
                    return nil
                }
            }
        }
        
        switch c {
        /// definition 7.1 -- intensional/extensional sets
        case .intSet: res = t1t.union(t2t)
        case .extSet: res = t1t.union(t2t)
        
        /// definition 7.6 -- extensional intersection
        case .Ω: res = t1t.intersection(t2t)
        /// definition 7.7 -- intensional intersection
        case .U: res = t1t.union(t2t)
            
        /// definition 7.8 -- extensional difference
        case .l: res = t1t.subtracting(t2t); con = .U
        /// definition 7.9 -- intensional difference
        case .ø: res = t1t.subtracting(t2t); con = .Ω
            
        /// definition 8.1 -- sequence
        case .x: return .compound(.x, [t1, t2])

        /// first term is a relation // TODO: need to validate
        case .e: return .compound(.e, t1.terms + t2.terms)
        case .i: return .compound(.i, t1.terms + t2.terms)
            
        case .n: return nil // handled separately
        case .c: res = t1t.intersection(t2t) // -- extensional difference
        case .d: res = t1t.union(t2t) // -- intensional intersection
        
        case .s: return .compound(.s, t1.terms + t2.terms)
        case .p: return .compound(.p, t1.terms + t2.terms)
        }
        
        // MARK: Validation
        
        // intention/extension sets are allowed one component
        if res.count == 1, case .compound(let c, _) = res.first, c == .intSet || c == .extSet {
            return .compound(con, Array(res))
        }
        
        return validate(res) ? .compound(con, Array(res).sorted()) : nil
    }
    
    /// MARK: helpers
    
    private static func validate(_ s: Set<Term>) -> Bool {
        if s.count < 2 { return false }
        // check if terms contain each other
        let result = s.flatMap { Term.getTerms($0) }
        if result.count != Set(result).count {
            return false 
        }
        return true
    }
}

