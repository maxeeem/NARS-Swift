
extension Copula {
    var atemporal: Copula {
        switch self {
        case .predictiveImp: fallthrough
        case .retrospectiveImp: fallthrough
        case .concurrentImp: return .implication
        case .predictiveEq: fallthrough
        case .retrospectiveEq: fallthrough
        case .concurrentEq: return .equivalence
        default: return self
        }
    }
    var concurrent: Copula {
        switch self {
        case .implication: return .concurrentImp
        case .equivalence: return .concurrentEq
        default: return self
        }
    }
    var predictive: Copula {
        switch self {
        case .implication: return .predictiveImp
        case .equivalence: return .predictiveEq
        default: return self
        }
    }
    var retrospective: Copula {
        switch self {
        case .implication: return .retrospectiveImp
        case .equivalence: return .retrospectiveEq
        default: return self
        }
    }
    var isConcurrent: Bool {
        self == .concurrentEq || self == .concurrentImp
    }
    var isPredictive: Bool {
        self == .predictiveEq || self == .predictiveImp
    }
    var isRetrospective: Bool {
        self == .retrospectiveImp || self == .retrospectiveEq
    }
}

public extension Term {
    func anticipation(for t: Term) -> Term? {
        switch self {
        case .statement(let s, let c, _):
            if s == t && c.isPredictive {
                return self
            }
            return nil
            
        case .compound(_, _):
            fallthrough // TODO: finish using temporal connectors
            
        default:
            return nil
        }
    }
}
