#if canImport(Glibc)
  import Glibc
#elseif canImport(CRT)
  import CRT
#elseif canImport(Darwin)
  import Darwin
#elseif canImport(WASILibc)
  import WASILibc
#endif

import NARS
import Narsese

func main() {
    do {
        var time: UInt32 = 0
        let timeProviderMs: () -> UInt32 = { time += 1 ; return time }

        let nars = NARS(timeProviderMs: timeProviderMs)

        let narsese = try Narsese.init()
        
        print("NARS started. Type 'q' to exit.\n")
        print("<task>          \n    execute narsese  - <bird -> animal>.")
        print(":alias <task>   \n    create new alias - :bird <bird -> animal>.")
        print("$alias          \n    execute an alias - $bird [will execute] <bird -> animal>.")
        print("reset           \n    perform system reset")
        print("10              \n    cycle for 10 seconds\n")
        print("Ready for input \n")
        
        var aliases: [String: Sentence] = [:]
        
        while let input = readInput() {
            if input == "q" {
                print("Done.")
                exit(0)
            }
            if input == "reset" {
                nars.reset()
                print("Ready for input \n")
                continue
            }
            if input.isEmpty {
                continue // return key pressed
            }
            if input.hasPrefix(":") {
                let alias = input.dropFirst().prefix(while: { !$0.isWhitespace })
                let task = input.suffix(from: input.index(after: alias.endIndex))
                if !alias.isEmpty, !task.isEmpty,
                    let s = Sentence(String(task), parser: narsese) {
                    aliases[String(alias)] = s
                    print("\(alias) := \(task)")
                } else {
                    print("invalid action: \(task)")
                }
                continue // new alias
            }
            if input.hasPrefix("$") {
                let alias = input.dropFirst().prefix(while: { !$0.isWhitespace })
                if let task = aliases[String(alias)] {
                    nars.perform(task)
                } else {
                    print("invalid alias: \(alias)")
                }
                continue // run alias
            }
            guard let s = Sentence(input, parser: narsese) else {
                print("invalid query: \(input)")
                continue
            }
            nars.perform(s)
        }
    } catch {
        print(error)
        exit(1)
    }
}

main()


// MARK: Helpers

func readInput() -> String? {
    guard let line = readLine(strippingNewline: true) else {
        return nil
    }
    var stripped = ""
    for c in line {
        // remove arrow key presses
        if !["", ""].contains(c) {
            stripped.append(c)
        }
    }
    return stripped
}
