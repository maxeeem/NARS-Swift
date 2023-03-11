import NARS
import Narsese


func contents(_ s: String) -> String {
    // TODO: parse :|: and %0.9% etc
    let start = s.index(s.startIndex, offsetBy: 0)
    let end = s.index(s.endIndex, offsetBy: -1)
    let contents = String(s[start..<end])
    return contents
}


extension Sentence {
    init?(_ s: String, parser: Narsese) {
        if let duration = Int(s) {
            self = .cycle(duration)
            return
        }
        
        let contents = contents(s)

        do {
            let term = try Term(contents, parser: parser)

            if s.hasSuffix(">.") {
                self = .judgement(term-*)
                return
            }
            
            if s.hasSuffix(">?") {
                self = .question(term-?)
                return
            }
            
            if s.hasSuffix(">!") {
                self = .goal(term-!)
                return
            }
            return nil
        } catch {
            print(error)
        }
        
        return nil
    }
}
