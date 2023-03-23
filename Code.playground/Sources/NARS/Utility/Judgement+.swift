
extension Judgement {
    static func updateTimestamp(_ j: Judgement, _ timeProvider: () -> UInt32) -> Judgement {
        var j = j
        let now = timeProvider()
        if j.derivationPath.count == 1 { // also update derivationPath
            return Judgement(j.statement, j.truthValue, tense: j.tense, timestamp: now)
        } else {
            j.timestamp = now
            return j
        }
    }
}

extension Sentence {
    func setTimestamp(_ timeProviderMs: () -> UInt32) -> Sentence {
        if case .judgement(let j) = self {
            if j.timestamp == 0 {
                return .judgement(.updateTimestamp(j, timeProviderMs))
            }
        }
        return self
    }
}

extension Judgement {
    func represent(_ j: Judgement) -> Judgement? {
        var rep: Statement?
        if case .statement(let s, let c, let p) = statement, c == .inheritance, s == j.statement {
            if case .compound(let con, let ts) = p, con == .e {
                if ts.count == 3, ts[0] == "represent", ts[1] == .º {
                    if ts[2].terms.contains(where: {if case .variable = $0 { return true } else {return false}}) {
                        return nil
                    }
                    rep = ts[2]
                }
            }
        }
        return (rep == nil) ? nil : Judgement(rep!, j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp)
    }
}
