
extension Copula {
    var atemporal: Copula {
        switch self {
        case .predictiveImp: fallthrough
        case .retrospectiveImp: fallthrough
        case .concurrentImp: return .implication
        case .predictiveEq: fallthrough
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
        case .equivalence: return .predictiveEq
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
        self == .retrospectiveImp
    }
}
