#if os(Linux)
  import Glibc
#elseif os(Windows)
  import CRT
#else
  import Darwin
#endif

import NARS
import Narsese

/* import Commander */ /// included as source -- credit: Kyle Fuller

let main = command(
    Flag("cycle", flag: "c", description: "Enable cycling"),
    Option("dialect", default: Dialect.swift, description: "Narsese Dialect \(Dialect.allCases)")
) { cycle, dialect in
    
    let nars = NARS(cycle: cycle)
    
    let narsese: Narsese
    
    do {
        narsese = try Narsese(dialect: dialect)
    } catch {
        print(error)
        exit(1)
    }
    
    print("NARS started. Type 'q' to exit.\n")
    print("<task>          \n    execute narsese  - <bird -> animal>.")
//    print(":alias <task>   \n    create new alias - :bird <bird -> animal>.")
//    print("$alias          \n    execute an alias - $bird [will execute] <bird -> animal>.")
    print("reset           \n    perform system reset")
    print("10              \n    cycle for 10 seconds\n")
    print("Ready for input \n")
    
//    var aliases: [String: Sentence] = [:]
    
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
//        if input.hasPrefix(":") {
//            let alias = input.dropFirst().prefix(while: { !$0.isWhitespace })
//            let task = input.suffix(from: input.index(after: alias.endIndex))
//            if !alias.isEmpty, !task.isEmpty,
//               let s = Sentence(String(task), parser: narsese) {
//                aliases[String(alias)] = s
//                print("\(alias) := \(task)")
//            } else {
//                print("invalid action: \(task)")
//            }
//            continue // new alias
//        }
//        if input.hasPrefix("$") {
//            let alias = input.dropFirst().prefix(while: { !$0.isWhitespace })
//            if let task = aliases[String(alias)] {
//                nars.perform(task)
//            } else {
//                print("invalid alias: \(alias)")
//            }
//            continue // run alias
//        }
        guard let s = Sentence(input, parser: narsese) else {
            print("invalid query: \(input)")
            continue
        }
        nars.perform(s)
    }
}

main.run()


// MARK: Helpers

func readInput() -> String? {
    guard let line = readLine(strippingNewline: true) else {
        return nil
    }
    var stripped = ""
    for c in line {
        // remove arrow key presses
        if !["", "", "", ""].contains(c) {
            stripped.append(c)
        }
    }
    return stripped
}

extension Dialect: ArgumentConvertible {
    public init(parser: ArgumentParser) throws {
        guard let dialect = Dialect(rawValue: parser.description) else {
            enum DialectParsingError: Error {
                case invalidDialect
            }
            throw DialectParsingError.invalidDialect
        }
        self = dialect
    }
    
    public var description: String { rawValue }
}
