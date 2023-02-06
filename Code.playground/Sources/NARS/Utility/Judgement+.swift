
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
    var flipped: Judgement? {
        var flipped: Statement?
        if case .statement(let sub, let cop, let pre) = statement, (cop == .equivalence || cop == .similarity) {
            flipped = .statement(pre, cop, sub)
        }
        if case .compound(let conn, let terms) = statement, conn == .c || conn == .U || conn == .Ω {
            if terms.count == 2 { // TODO: handle compounds with multiple terms
                flipped = .compound(conn, terms.reversed())
            }
        }
        return (flipped == nil) ? nil : Judgement(flipped!, truthValue, derivationPath, tense: tense, timestamp: timestamp)
    }
}
