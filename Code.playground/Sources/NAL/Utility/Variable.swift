
extension Variable {
    var name: String? {
        switch self {
        case .independent(let string):
            return string
        case .dependent(let optional, _):
            return optional
        case .query(let optional):
            return optional
        }
    }
}

extension Variable {
    init?(_ string: String) {
        if string.hasPrefix("?") {
            let name = string.suffix(from: string.index(string.startIndex, offsetBy: 1))
            self = .query(name.isEmpty ? nil : String(name))
            return
        }
        if string.hasPrefix("#") {
            let name = string.suffix(from: string.index(string.startIndex, offsetBy: 1))
            if name.isEmpty {
                self = .dependent(nil, [])
                return
            } else {
                // TODO: parse independent vars list
                if let idx = name.firstIndex(of: "(") {
                    let trimmed = name.prefix(upTo: idx)
                    self = .dependent(String(trimmed), [])
                    return
                }
                self = .independent(String(name))
                return
            }
        }
        return nil
    }
}
