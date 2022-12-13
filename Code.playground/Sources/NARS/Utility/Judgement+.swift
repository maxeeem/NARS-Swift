import Dispatch

extension Judgement {
    static func updateTimestamp(_ j: Judgement) -> Judgement {
        var j = j
        let now = DispatchWallTime.now()
        if j.derivationPath.count == 1 { // also update derivationPath
            return Judgement(j.statement, j.truthValue, tense: j.tense, timestamp: now.rawValue)
        } else {
            j.timestamp = now.rawValue
            return j
        }
    }
}
