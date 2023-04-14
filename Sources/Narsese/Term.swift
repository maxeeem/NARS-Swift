import NAL

public extension Term {
    init(_ s: String, parser: Narsese) throws {
        let ast = try parser.parse(s)
        
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
                    let copula = try Copula.canonical(cop, dialect: parser.dialect)
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
                    let connector = try ç.canonical(con, dialect: parser.dialect)
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
                case "compound-image":
                    let con = String(s[children[1].leafs.first!])
                    let connector = try ç.canonical(con, dialect: parser.dialect)
                    let first = children[3].children!.first!
                    let r = try convert(tree: first)
                    if ["º", "_"].contains(String(s[children[5].leafs.first!])) {
                        let t = try convert(tree: children[7])
                        return connector.image(r, .º, t)
                    } else {
                        let t = try convert(tree: children[5])
                        return connector.image(r, t, .º)
                    }
                case "compound-infix":
                    let con = String(s[children[3].leafs.first!])
                    let connector = try ç.canonical(con, dialect: parser.dialect)
                    let t1 = try convert(tree: children[1])
                    let t2 = try convert(tree: children[5])
                    return .compound(connector, [t1, t2])
                case "compound-neg":
                    let con = String(s[children[1].leafs.first!])
                    let connector = try ç.canonical(con, dialect: parser.dialect)
                    let term = try convert(tree: children[3])
                    return .compound(connector, [term])
                case "operation":
                    let op = try convert(tree: children[2])
                    if children.count <= 4 { // no arguments
                        return .operation(op.description, [])
                    }
                    let terms = try children[4].children?
                                    .map({ try convert(tree: $0) })
                                    .filter { $0 != .NULL }
                    return .operation(op.description, terms ?? [])
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
