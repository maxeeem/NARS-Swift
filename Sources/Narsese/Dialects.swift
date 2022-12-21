public enum Dialect {
    case canonical, ona, opennars, swift
}

extension Connector {
    var swift: [String] {
        var v: [String] = []
        switch self {
        case .Ω:
            v += ["&"]
        case .U:
            v += ["|"]
        case .l:
            v += ["-"]
        case .ø:
            v += ["~"]
        case .x:
            v += ["*"]
        case .n:
            v += ["--", "!"]
        case .c:
            v += ["&&"]
        case .d:
            v += ["||"]
        default:
            v += [rawValue]
        }
        return v
    }
    
    var ona: [String] {
        var v: [String] = []
        switch self {
        case .c:
            v += [";"] // TODO: currently conflicts with canonical parallel conjunction
        case .d:
            v += ["_"]
        default:
            v += [rawValue]
        }
        return v
    }
    
    var variants: [Dialect: [String]] {
        [
            .canonical: [rawValue],
            .swift: swift,
            .ona: ona
        ]
    }
    
    func all(_ dialect: Dialect = .swift) -> String {
        variants[dialect, default: []]
            .map({ "'\($0)'" })
            .joined(separator: "|")
    }

    // MARK: Canonical
    
    static func canonical(_ s: String, dialect: Dialect = .swift) throws -> Connector {
        let connector = allCases.first { c in
            c.variants[dialect, default: []].contains(s)
        }
        
        guard let connector = connector else {
            enum ConnectorParsingError: Error {
                case invalidInput(String)
            }
            throw ConnectorParsingError.invalidInput(s)
        }
        
        return connector
    }
}
