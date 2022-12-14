import NAL

extension Item {
    // TODO: this needs to be handled properly
    mutating func adjustPriority(_ derivedJudgements: [Judgement]) {
        if let maxPriority = derivedJudgements.map({$0.truthValue.confidence}).max() {
            let newPriority = (priority + maxPriority) / 2
            priority = min(newPriority, 0.9)
        }
    }
}
