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

/* import Commander */ /// included as source -- credit: Kyle Fuller

import Darwin
 
var orig_termios = termios()
 
func reset_terminal_mode() {
  tcsetattr(0, TCSANOW, &orig_termios)
}
 
func set_conio_terminal_mode() {
  tcgetattr(0, &orig_termios)
  var new_termios = orig_termios
  atexit(reset_terminal_mode)
  cfmakeraw(&new_termios)
  tcsetattr(0, TCSANOW, &new_termios)
}
 
func kbhit() -> Bool {
  var fds = [ pollfd(fd: STDIN_FILENO, events: Int16(POLLIN), revents: 0) ]
  let res = poll(&fds, 1, 0)
  return res > 0
}
 
func getChar() -> Int {
  var c: UInt8 = 0
 
  let r = read(0, &c, 1)
  if r < 0 {
    return r
  } else {
    return Int(c)
  }
}

let main = command(
//    Flag("cycle", flag: "c", description: "Enable cycling"),
    Option("dialect", default: Dialect.swift, description: "Narsese Dialect \(Dialect.allCases)")
    
) { dialect in

    var time: UInt32 = 0
    let timeProviderMs: () -> UInt32 = { time += 1 ; return time }
    
    var verbose = false
    
    let nars = NARS(timeProviderMs: timeProviderMs) {
        if verbose {
            print($0)
            
        } else {
            if !$0.hasPrefix(". ⏱") {
                print($0)
            }
        }
    }
    
    let narsese: Narsese
    
    do {
        narsese = try Narsese.init(dialect: dialect)
    } catch {
        print(error)
        exit(1)
    }

    print("NARS started.\n")
    
    print("<task>          \n    execute narsese  - <bird -> animal>.")
    print(":alias <task>   \n    create new alias - :bird <bird -> animal>.")
    print("$alias          \n    execute an alias - $bird [will execute] <bird -> animal>.")
    print("reset           \n    perform system reset")
    print("10              \n    cycle for 10 seconds\n")
    
    print("------          \n")
    print("v - verbose toggle [default: off]")
    print("q - quit\n")

    print("Ready for input \n")
    
    
    fflush(stdout)
    
    set_conio_terminal_mode()
    

    var aliases: [String: Sentence] = [:]
    
    var input: String = ""

    while true {
        
        if kbhit() {
            let key = getChar()
            
            if key == 13 {  // return key
                if input == "q" {
                    break
                }
                if input == "v" {
                    input = ""
                    verbose.toggle()
                    print("Verbose:", verbose ? "ON" : "OFF")
                    fflush(stdout)
                    continue
                }
                if input == "reset" {
                    input = ""
                    nars.reset()
                    print("Ready for input \n")
                    fflush(stdout)
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
                    input = ""
                    print("invalid query: \(input)")
                    fflush(stdout)
                    continue
                }

                input = "" // clear input

                nars.perform(s)

                fflush(stdout)

            } else {
                
                input.append(String(UnicodeScalar(key)!))
            }
            
        } else {
            nars.perform(.cycle)
            
            fflush(stdout)
        }
    }
    
    print("Done.")
    
    print("\r")
    reset_terminal_mode()
    
    exit(0)
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
