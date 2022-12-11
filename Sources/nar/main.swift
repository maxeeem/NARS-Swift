#if os(Linux)
  import Glibc
#elseif os(Windows)
  import CRT
#else
  import Darwin
#endif

import NARS

func main() {
    var cycle = false
    
    guard CommandLine.arguments.count <= 2 else {
        print("Usage: nar [-c]")
        print("Pass optional -c flag to enable cycling")
        exit(1)
    }
    
    if CommandLine.arguments.last == "-c" {
        cycle = true
    }
    
    let nars = NARS(cycle: cycle)
    
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
            if !alias.isEmpty, !task.isEmpty, let s = Sentence(String(task)) {
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
        guard let s = Sentence(input) else {
            print("invalid query: \(input)")
            continue
        }
        nars.perform(s)
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
