import NAL

public enum Dialect: String, CaseIterable {
    case canonical, ona, opennars, swift
}

extension Copula {
    // TODO: add dialects for copulas
}

extension Connector {
    func variants(_ dialect: Dialect) -> [String] {
        switch dialect {
        case .canonical: return [rawValue]
        case .ona:       return ona
        case .opennars:  return [] // TODO: add mapping
        case .swift:     return swift
        }
    }

    var swift: [String] {
        switch self {
        case .Ω: return ["&"]
        case .U: return ["|"]
        case .l: return ["-"]
        case .ø: return ["~"]
        case .x: return ["*"]
        case .n: return ["--", "!"]
        case .c: return ["&&"]
        case .d: return ["||"]
        default: return [rawValue]
        }
    }
    
    var ona: [String] {
        switch self {
        case .c: return [";"] // TODO: currently conflicts with canonical parallel conjunction
        case .d: return ["_"]
        default: return [rawValue]
        }
    }
    
    // MARK: Variations
    
    func all(_ dialect: Dialect) -> String {
        let vars = variants(dialect)
            .map { "'\($0)'" }
            .joined(separator: "|")
        return "(\(vars))"
    }

    // MARK: Primary
    
    static func primary(_ dialect: Dialect) -> String {
        [ç.Ω, .U, .x, .c, .d, .s, .p].all(dialect)
    }
    
    static func neg(_ dialect: Dialect) -> String {
        [ç.n].all(dialect)
    }
    
    static func diff(_ dialect: Dialect) -> String {
        [ç.l, .ø].all(dialect)
    }
    
    // MARK: Canonical
    
    static func canonical(_ s: String, dialect: Dialect) throws -> Connector {
        let connector = allCases.first { c in
            c.variants(dialect).contains(s)
        }
        
        guard let connector = connector else {
            enum ConnectorParsingError: Error {
                case invalidInput(String)
            }
            throw ConnectorParsingError.invalidInput(s)
        }
        
        return connector
    }
    
    // MARK: Validator
    
    static func validate(_ dialect: Dialect) throws {
        let duplicates = allCases
            .flatMap { $0.variants(dialect) }
            .duplicates()
        if !duplicates.isEmpty {
            enum DialectParsingError: Error {
                case duplicateConnectors([String])
            }
            throw DialectParsingError.duplicateConnectors(duplicates)
        }
    }
}


// MARK: - Helpers

/// from https://stackoverflow.com/a/51057578
extension Array where Element: Hashable {
    func duplicates() -> Array {
        let groups = Dictionary(grouping: self, by: {$0})
        let duplicateGroups = groups.filter {$1.count > 1}
        let duplicates = Array(duplicateGroups.keys)
        return duplicates
    }
}

extension Array where Element == Connector {
    func all(_ dialect: Dialect) -> String {
        map { $0.all(dialect) }.joined(separator: "|")
    }
}
