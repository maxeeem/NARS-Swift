import NAL

extension Connector {
    var dialects: [String] {
        var dialects = [rawValue]
        switch self {
        case .n:
            dialects.append("--")
        default:
            break
        }
        return dialects
    }
    var all: String {
        dialects.map({ "'\($0)'" }).joined(separator: "|")
    }
    static func canonical(_ s: String) -> String? {
        allCases.map(\.dialects).first(where: { $0.contains(s) })?.first
    }
}
