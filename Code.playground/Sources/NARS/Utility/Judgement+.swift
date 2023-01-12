//import Dispatch

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
