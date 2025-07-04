
public extension Array where Element == Judgement {
    func removeDuplicates() -> [Judgement] {
        let unique = Dictionary(grouping: self) {
            $0.identifier
        }.values.compactMap {
            $0.max { j1, j2 in
                let j = choice(j1: j1, j2: j2)
                return j.statement == j2.statement
            }
        }
        return unique
    }
}

extension Judgement {
    public init(_ statement: Statement, _ truthValue: TruthValue, _ derivationPath: [String] = [], tense: Tense? = nil, timestamp: UInt32 = 0) {
        self.statement = statement
        self.truthValue = truthValue
        self.tense = tense
        self.timestamp = (tense == nil && timestamp == ETERNAL) ? ETERNAL : timestamp
        if derivationPath.isEmpty {
            let description = Judgement.sortedDescription(statement)
            self.derivationPath = ["\(description)+\((truthValue.f, truthValue.c, timestamp, truthValue.rule))"]
        } else {
            self.derivationPath = derivationPath
        }
    }
    
    private static func sortedDescription(_ statement: Statement) -> String {
        var st = ""
        switch statement {
        case .statement(let s, let c, let p):
            if c == .similarity {
                st = (s < p) ? "\(s) \(c.rawValue) \(p)" : "\(p) \(c.rawValue) \(s)"
            } else if c == .equivalence {
                let sub = sortedDescription(s)
                let pre = sortedDescription(p)
                st = (sub < pre) ? "(\(sub)) \(c.rawValue) (\(pre))" : "(\(pre)) \(c.rawValue) (\(sub))"
            } else {
                st = "\(statement)"
            }
        case .compound(let c, let terms):
            if c == .c || c == .d || c == .n {
                st = "\(c.rawValue) \(terms.map{"(\(sortedDescription($0)))"}.sorted().joined(separator: ", "))"
            } else {
                st = "\(statement)"
            }
        default:
            st = "\(statement)"
        }
        return st
    }
}

extension Judgement {
    static func mergeEvidence(_ j1: Judgement, _ j2: Judgement) -> [String] {
        if j1.derivationPath.isEmpty {
            return j2.derivationPath
        } else if j2.derivationPath.isEmpty {
            return j1.derivationPath
        } else {
            var tail: [String] = []
            if j1.derivationPath.count < j2.derivationPath.count {
                tail = Array(j2.derivationPath.suffix(from: j1.derivationPath.endIndex))
            } else if j2.derivationPath.count > j1.derivationPath.count {
                tail = Array(j1.derivationPath.suffix(from: j1.derivationPath.count))
            }
            return (zip(j1.derivationPath, j2.derivationPath).reduce([], { partialResult, next in
                partialResult + (next.0 == next.1 ? [next.0] : [next.0, next.1])
            }) + tail).suffix(100)
        }
    }
    
    public func evidenceOverlap(_ j2: Judgement) -> Bool {
        let sameRoot = derivationPath.first == j2.derivationPath.first
        let p1 = sameRoot ? Array(derivationPath.dropFirst()) : derivationPath
        let p2 = sameRoot ? Array(j2.derivationPath.dropFirst()) : j2.derivationPath

        if p1.isEmpty && p2.isEmpty {
            return true // judgements have the same root
        } else if (p1.isEmpty && !p2.isEmpty) || (p2.isEmpty && !p1.isEmpty) {
            return false // same root but one is empty indicating user input
        } else if p1.count == 1 && p2.count == 1 {
            if p1[0].hasSuffix("\(ETERNAL))") && p2[0].hasSuffix("\(ETERNAL))") {
                // judgements are both eternal
                if p1[0] == p2[0] // same path or one is a theorem which has E as its evidential base
                    || p1[0].hasSuffix("+(1.0, 1.0, \(ETERNAL))") || p2[0].hasSuffix("+(1.0, 1.0, \(ETERNAL))") {
//                    // TODO: do proper comparison taking into account symmetrical statements
//                    // so <bird <-> swan> should be same as <swan <-> bird>
                    return true // same path
                } else {
                    return false // different path
                }
            }
        }
        
        return !Set(p1).intersection(Set(p2)).isEmpty
    }
}
