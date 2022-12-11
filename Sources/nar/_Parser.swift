
import NARS

// MARK: Helpers

func contents(_ s: String) -> String {
    // TODO: parse :|: and %0.9% etc
    let start = s.index(s.startIndex, offsetBy: 0)
    let end = s.index(s.endIndex, offsetBy: -1)
    let contents = String(s[start..<end])
    return contents
}


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
        let grammar = try Grammar(ebnf: Narsese.grammar, start: "exp")
        let parser = EarleyParser(grammar: grammar)
        let ast = try parser.syntaxTree(for: s)
        
        func convert(tree: SyntaxTree<NonTerminal, Range<String.Index>>) throws -> Term {
            switch tree {
            case .node(key: let key, children: let children):
//                print(">", key.name)
                switch key.name {
                case "exp": // expression in angle brackets
                    return try convert(tree: children[1])
                case "statement":
                    let sub = try convert(tree: children[0])
                    let pre = try convert(tree: children[4])
                    let cop = String(s[children[2].leafs.first!])
                    let copula = Copula(rawValue: cop)!
                    return .statement(sub, copula, pre)
                case "term":
                    return try convert(tree: children.first!)
                case "compound":
                    return try convert(tree: children.first!)
                case "compound-set":
                    let braces = String(s[tree.leafs.first!] + s[tree.leafs.last!])
                    let connector = Connector(rawValue: braces)!
                    let terms = children[1]
                    let first = terms.children!.first!
                    let t1 = try convert(tree: first)
                    if terms.children!.count > 1 {
                        let tn = try terms.children![1...]
                            .map { try convert(tree: $0) }
                            .filter { $0 != .NULL }
                        return .compound(connector, [t1] + tn)
                    }
                    return .compound(connector, [t1])
                case "compound-prefix":
                    let con = String(s[children[1].leafs.first!])
                    let canonical = ç.canonical(con)!
                    let connector = Connector(rawValue: canonical)!
                    let first = children[3].children!.first!
                    let t1 = try convert(tree: first)
                    let terms = children[5]
                    let second = terms.children!.first!
                    let t2 = try convert(tree: second)
                    if terms.children!.count > 1 {
                        let tn = try terms.children![1...]
                            .map { try convert(tree: $0) }
                            .filter { $0 != .NULL }
                        return .compound(connector, [t1, t2] + tn)
                    }
                    return .compound(connector, [t1, t2])
                case "compound-infix":
                    let con = String(s[children[3].leafs.first!])
                    let canonical = ç.canonical(con)!
                    let connector = Connector(rawValue: canonical)!
                    let t1 = try convert(tree: children[1])
                    let t2 = try convert(tree: children[5])
                    return .compound(connector, [t1, t2])
                case "compound-neg":
                    let con = String(s[children[1].leafs.first!])
                    let canonical = ç.canonical(con)!
                    let connector = Connector(rawValue: canonical)!
                    let term = try convert(tree: children[3])
                    return .compound(connector, [term])
                case "variable":
                    let word = String(s[tree.leafs.first!.lowerBound ..< tree.leafs.last!.upperBound])
                    let vari = Variable(word)!
                    return .variable(vari)
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
