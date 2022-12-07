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
    
    while let input = readInput() {
        if input == "q" {
            print("Done.")
            exit(0)
        }
        if input.isEmpty {
            continue // return key pressed
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

func contents(_ s: String) -> String {
    // TODO: parse :|: and %0.9% etc
    let start = s.index(s.startIndex, offsetBy: 0)
    let end = s.index(s.endIndex, offsetBy: -1)
    let contents = String(s[start..<end])
    return contents
}


// MARK: parse narsese string

extension Sentence {
    init?(_ s: String) {
        if let duration = Int(s) {
            self = .cycle(duration)
            return
        }
        
        let contents = contents(s)

        do {
            let term = try Term(contents)

            if s.hasSuffix(">.") {
                self = .judgement(term-*)
                return
            }
            
            if s.hasSuffix(">?") {
                self = .question(term-?)
                return
            }
        } catch {
            print(error)
        }
        
        return nil
    }
}


extension Term {
    init(_ s: String) throws {
        
        // MARK: Narsese grammar

        let grammarString = """
        exp              = '<', (statement | term), '>';

        statement        = term, space, copula, space, term;
        copula           = '->' | '=>';
        term             = word | exp | statement;

        word             = {letter|digit|_|-};

        space            = [{' '}];

        digit            = '0' ... '9';
        letter           = 'A' ... 'Z' | 'a' ... 'z';
        """

        let grammar = try Grammar(ebnf: grammarString, start: "exp")

        let parser = EarleyParser(grammar: grammar)
        
        let ast = try parser.syntaxTree(for: s)
        
        func convert(tree: SyntaxTree<NonTerminal, Range<String.Index>>) throws -> Term {
            switch tree {
            case .node(key: let key, children: let children):
        //        print(">", key.name)
                switch key.name {
                case "exp":
                    return try convert(tree: children[1])
                case "statement":
                    let sub = try convert(tree: children[0])
                    let cop = try convert(tree: children[2])
                    let pre = try convert(tree: children[4])
                    let copula = Copula(rawValue: cop.description)!
                    return .statement(sub, copula, pre)
                    
                case "term":
                    return try convert(tree: children.first!)
                case "copula":
                    let word = String(s[tree.leafs.first!.lowerBound ..< tree.leafs.last!.upperBound])
                    return .symbol(word)
                case "word":
                    let word = String(s[tree.leafs.first!.lowerBound ..< tree.leafs.last!.upperBound])
                    return .symbol(word)
                default:
                    return .NULL
                }
            default:
                return .NULL
            }
        }
        
        self = try convert(tree: ast)
        return
    }
}
