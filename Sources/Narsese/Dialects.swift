import NAL

public enum Dialect: String, CaseIterable {
    case canonical, ona, opennars, swift
}

// MARK: Copula

extension Copula: Translatable {
    func variants(_ dialect: Dialect) -> [String] {
        switch dialect {
        case .canonical: return canonical
        case .ona:       return [] // TODO: add mapping
        case .opennars:  return opennars
        case .swift:     return swift
        }
    }

    var canonical: [String] {
        [rawValue.replacingOccurrences(of: "\\", with: "\\\\")]
    }
    
    var swift: [String] {
        canonical // may change if we use unicode for canonical
    }
    
    var opennars: [String] {
        switch self {
        case .inheritance:
            return ["-->"]
        case .implication:
            return ["==>"]
        case .instance:
            return ["{--"]
        case .property:
            return ["--]"]
        case .insProp:
            return ["{-]"]
        case .predictiveImp:
            return ["=/>"]
        case .concurrentImp:
            return ["=|>"]
        case .retrospectiveImp:
            return ["=\\\\>"]
        case .predictiveEq:
            return ["</>"]
        case .concurrentEq:
            return ["<|>"]
        case .retrospectiveEq:
            return ["<\\\\>"]
        default:
            return canonical  // TODO: finish mapping
        }
    }
}

// MARK: Connector

extension Connector: Translatable {
    func variants(_ dialect: Dialect) -> [String] {
        switch dialect {
        case .canonical: return canonical
        case .ona:       return ona
        case .opennars:  return swift // TODO: add mapping
        case .swift:     return swift
        }
    }

    var canonical: [String] {
        [rawValue.replacingOccurrences(of: "\\", with: "\\\\")]
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
        default: return canonical
        }
    }
    
    var ona: [String] {
        switch self {
        case .c: return [";"] // TODO: currently conflicts with canonical parallel conjunction
        case .d: return ["_"]
        default: return canonical
        }
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
}


// MARK: - Translatable

protocol Translatable: CaseIterable {
    func variants(_ dialect: Dialect) -> [String]
    func all(_ dialect: Dialect) -> String

    static func canonical(_ s: String, dialect: Dialect) throws -> Self
    static func validate(_ dialect: Dialect) throws
}

enum ConnectorParsingError: Error {
    case invalidInput(String)
}

enum DialectParsingError: Error {
    case duplicateConnectors([String])
}

extension Translatable {
    func all(_ dialect: Dialect) -> String {
        let vars = variants(dialect)
            .map { "'\($0)'" }
            .joined(separator: "|")
        return "(\(vars))"
    }

    static func canonical(_ s: String, dialect: Dialect) throws -> Self {
        let connector = allCases.first { c in
            c.variants(dialect)
                .map { $0.replacingOccurrences(of: "\\\\", with: "\\") }
                .contains(s)
        }
        guard let connector = connector else {
            throw ConnectorParsingError.invalidInput(s)
        }
        return connector
    }
    
    static func validate(_ dialect: Dialect) throws {
        let duplicates = allCases
            .flatMap { $0.variants(dialect) }
            .duplicates()
        if !duplicates.isEmpty {
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

extension Array where Element: Translatable {
    func all(_ dialect: Dialect) -> String {
        map { $0.all(dialect) }.joined(separator: "|")
    }
}
