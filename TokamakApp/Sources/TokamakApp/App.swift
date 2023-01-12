import TokamakDOM

class NARS_Singleton: ObservableObject {
    var verbose = true
    @Published var history = [String]()
    
    init() {
        do {
            let narsese = try Narsese.init()
        } catch {
            print(error)
        }
    }

    var count = 0
    var time: UInt32 = 0
    lazy var timeProviderMs: () -> UInt32 = { self.time += 1 ; return self.time }

    lazy var instance = NARS(timeProviderMs: timeProviderMs) { [unowned self] s in
        if self.verbose == false && s.contains("⏱") { return }
//        DispatchQueue.main.async {
            self.count += 1
            self.history.append("\(self.count) " + s);
//        }
//        print(s)
    }
}

@main
struct TokamakApp: App {
    @StateObject var nars = NARS_Singleton()

    var body: some Scene {
        WindowGroup("Tokamak App") {
            ContentView()
                .environmentObject(nars)
                .onAppear {
                    nars.instance.perform(
                        ("bird" --> "animal")-*,
                        ("robin" --> "bird")-*
                    )
                }
        }
    }
}


struct ContentView: View {
    @EnvironmentObject var nars: NARS_Singleton

    var body: some View {
        VStack {
            Text("Hello, \(nars.instance.name)!")
            Spacer()
            ZStack(alignment: .topTrailing) {
                ScrollView {
//                    ScrollViewReader { value in
                        ForEach(nars.history, id: \.self) { line in
                            Text(line)
                                .font(.footnote)
                                .frame(maxWidth: .infinity, alignment: .leading)
                        }
//                        .onChange(of: nars.history.count) { newValue in
//                            value.scrollTo(nars.history[newValue - 1])
//                        }
//                    }
                }
                Button("🧨") {
                    nars.instance.reset()
                    nars.count += 1
                    nars.history.append("\(nars.count) .  🧨 Reset completed!")
                }
                .font(Font.title)
                .background(Color.white)
                .foregroundColor(.red)
                .clipShape(Circle())
                .shadow(radius: 1)
                .padding()
            }
        }
    }
}






public struct Narsese {
    public var parser: Parser!
    public init() throws {
        let grammar = try Grammar(ebnf: grammar, start: "word")
        parser = EarleyParser(grammar: grammar)
    }
    
    public func parse(_ s: String) throws -> ParseTree {
        try parser.syntaxTree(for: s)
    }
    
    public let grammar = """
        word             = {letter|digit|_};

        digit            = '0' ... '9';
        letter           = 'A' ... 'Z' | 'a' ... 'z';
        """
    
    /*
     
     space            = [{' '}];
     term             = word | exp | statement;
     
     exp              = '<', (statement | term), '>';

     statement        = term, space, copula, space, term;
     
     copula           = '->' | '<->' | '=>' | '<=>'
                        | '•->' | '->•' | '•->•'
                        | '/=>' | '\\\\=>' | '|=>' | '/<=>' | '|<=>'
     ;
     
     variable         = indep-var | dep-var | query-var;
     
     indep-var        = '#', word;
     dep-var          = '#', [word, '(', [{indep-var|','|space}], ')'];
     query-var        = '?', [word];
     
     compound         = compound-set
                        | compound-prefix
                        | compound-infix
                        | compound-neg
                        | compound-image
     ;
     
     compound-set     = ('{', terms, '}') | ('[', terms, ']');
     compound-prefix  = '(', ((connector, seq, term, seq, terms)
                             |(connector-diff, seq, term, seq, term))
                        ,')'
     ;
     
     compound-infix   = '(', term, seq, (connector|connector-diff), seq, term, ')';
     compound-neg     = '(', \(ç.n.all), seq, term, ')';
     
     compound-image   = '(',
                            ('/' | '\\\\'), seq, term,
                              ((terms, 'º', terms)
                              | (terms, 'º', [terms])
                              | ([terms], 'º', terms))
                        ,')'
     ;
     
     connector        = \([ç.Ω, .U, .x, .c, .d, .s, .p].map(\.all).joined(separator: "|"));
     
     connector-diff   = \([ç.l, .ø].map(\.all).joined(separator: "|"));

     terms            = term, [{seq-comma, term}|{seq-space, term}];
     
     seq              = seq-comma | seq-space;
     seq-comma        = space, ',', space;
     seq-space        = space, ' ', space;

     
     */
}


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
//
//  BNFImporter.swift
//  Covfefe
//
//  Created by Palle Klewitz on 14.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

/// A grammar describing the Backus-Naur form
var bnfGrammar: Grammar {
    
    let syntax = "syntax" ~~> n("optional-whitespace") <|> n("newlines") <|> n("rule") <|> n("rule") <+> n("newlines") <|> n("syntax") <+> n("newlines") <+> n("rule") <+> (n("newlines") <|> [[]])
    let rule = "rule" ~~> n("optional-whitespace") <+> n("rule-name-container") <+> n("optional-whitespace") <+> n("assignment-operator") <+> n("optional-whitespace") <+> n("expression") <+> n("optional-whitespace")
    
    let optionalWhitespace = "optional-whitespace" ~~> [[]] <|> n("whitespace") <+> [n("optional-whitespace")]
    let whitespace = "whitespace" ~~> SymbolSet.whitespace <|> n("comment")
    let newlines = "newlines" ~~> t("\n") <|> t("\n") <+> n("optional-whitespace") <+> n("newlines")
    
    let comment = "comment" ~~> t("(") <+> t("*") <+> n("comment-content") <+> t("*") <+> t(")") <|> t("(") <+> t("*") <+> t("*") <+> t("*") <+> t(")")
    let commentContent = "comment-content" ~~> n("comment-content") <+> n("comment-content-char") <|> [[]]
    // a comment cannot contain a * followed by a ) or a ( followed by a *
    let commentContentChar = try! "comment-content-char" ~~> rt("[^*(]") <|> n("comment-asterisk") <+> rt("[^)]") <|> n("comment-open-parenthesis") <+> rt("[^*]") <|> n("comment")
    let commentAsterisk = "comment-asterisk" ~~> n("comment-asterisk") <+> t("*") <|> t("*")
    let commentOpenParenthesis = "comment-open-parenthesis" ~~> n("comment-open-parenthesis") <+> t("(") <|> t("(")
    
    let assignmentOperator = "assignment-operator" ~~> t(":") <+> t(":") <+> t("=")
    
    let ruleNameContainer = "rule-name-container" ~~> t("<") <+> n("rule-name") <+> t(">")
    let ruleName = "rule-name" ~~> n("rule-name") <+> n("rule-name-char") <|> [[]]
    let ruleNameChar = try! "rule-name-char" ~~> rt("[a-zA-Z0-9-_]")
    
    let expression = "expression" ~~> n("concatenation") <|> n("alternation")
    let alternation = "alternation" ~~> n("expression") <+> n("optional-whitespace") <+> t("|") <+> n("optional-whitespace") <+> n("concatenation")
    let concatenation = "concatenation" ~~> n("expression-element") <|> n("concatenation") <+> n("optional-whitespace") <+> n("expression-element")
    let expressionElement = "expression-element" ~~> n("literal") <|> n("rule-name-container") <|> n("expression-group") <|> n("expression-repetition") <|> n("expression-optional")
    let expressionGroup = "expression-group" ~~> t("(") <+> n("optional-whitespace") <+> n("expression") <+> n("optional-whitespace") <+> t(")")
    let expressionRepetition = "expression-repetition" ~~> t("{") <+> n("optional-whitespace") <+> n("expression") <+> n("optional-whitespace") <+> t("}")
    let expressionOptional = "expression-optional" ~~> t("[") <+> n("optional-whitespace") <+> n("expression") <+> n("optional-whitespace") <+> t("]")
    let literal = "literal" ~~> t("'") <+> n("string-1") <+> t("'") <|> t("\"") <+> n("string-2") <+> t("\"") <|> n("range-literal")
    let string1 = "string-1" ~~> n("string-1") <+> n("string-1-char") <|> [[]]
    let string2 = "string-2" ~~> n("string-2") <+> n("string-2-char") <|> [[]]
    
    let rangeLiteral = "range-literal" ~~> n("single-char-literal") <+> n("optional-whitespace") <+> t(".") <+> t(".") <+> t(".") <+> n("optional-whitespace") <+> n("single-char-literal")
    let singleCharLiteral = "single-char-literal" ~~> t("'") <+> n("string-1-char") <+> t("'") <|> t("\"") <+> n("string-2-char") <+> t("\"")
    
    // no ', \, \r or \n
    let string1char = try! "string-1-char" ~~> rt("[^'\\\\\r\n]") <|> n("string-escaped-char") <|> n("escaped-single-quote")
    let string2char = try! "string-2-char" ~~> rt("[^\"\\\\\r\n]") <|> n("string-escaped-char") <|> n("escaped-double-quote")
    
    let stringEscapedChar = "string-escaped-char" ~~> n("unicode-scalar") <|> n("carriage-return") <|> n("line-feed") <|> n("tab-char") <|> n("backslash")
    let unicodeScalar = "unicode-scalar" ~~> t("\\") <+> t("u") <+> t("{") <+>  n("unicode-scalar-digits") <+> t("}")
    let unicodeScalarDigits = "unicode-scalar-digits" ~~> [n("digit")] <+> (n("digit") <|> [[]]) <+> (n("digit") <|> [[]]) <+> (n("digit") <|> [[]]) <+> (n("digit") <|> [[]]) <+> (n("digit") <|> [[]]) <+> (n("digit") <|> [[]]) <+> (n("digit") <|> [[]])
    let digit = try! "digit" ~~> rt("[0-9a-fA-F]")
    let carriageReturn = "carriage-return" ~~> t("\\") <+> t("r")
    let lineFeed = "line-feed" ~~> t("\\") <+> t("n")
    let tabChar = "tab-char" ~~> t("\\") <+> t("t")
    let backslash = "backslash" ~~> t("\\") <+> t("\\")
    let singleQuote = "escaped-single-quote" ~~> t("\\") <+> t("'")
    let doubleQuote = "escaped-double-quote" ~~> t("\\") <+> t("\"")
    
    var productions: [Production] = []
    productions.append(contentsOf: syntax)
    productions.append(rule)
    productions.append(contentsOf: optionalWhitespace)
    productions.append(contentsOf: whitespace)
    productions.append(contentsOf: comment)
    productions.append(contentsOf: commentContent)
    productions.append(contentsOf: commentContentChar)
    productions.append(contentsOf: commentAsterisk)
    productions.append(contentsOf: commentOpenParenthesis)
    productions.append(contentsOf: newlines)
    productions.append(assignmentOperator)
    productions.append(ruleNameContainer)
    productions.append(contentsOf: ruleName)
    productions.append(ruleNameChar)
    productions.append(contentsOf: expression)
    productions.append(alternation)
    productions.append(contentsOf: concatenation)
    productions.append(contentsOf: expressionElement)
    productions.append(expressionGroup)
    productions.append(expressionRepetition)
    productions.append(expressionOptional)
    productions.append(contentsOf: literal)
    productions.append(contentsOf: string1)
    productions.append(contentsOf: string2)
    productions.append(contentsOf: string1char)
    productions.append(contentsOf: string2char)
    productions.append(rangeLiteral)
    productions.append(contentsOf: singleCharLiteral)
    productions.append(contentsOf: stringEscapedChar)
    productions.append(unicodeScalar)
    productions.append(contentsOf: unicodeScalarDigits)
    productions.append(digit)
    productions.append(carriageReturn)
    productions.append(lineFeed)
    productions.append(tabChar)
    productions.append(backslash)
    productions.append(singleQuote)
    productions.append(doubleQuote)
    
    return Grammar(productions: productions, start: "syntax")
}

enum LiteralParsingError: Error {
    case invalidUnicodeScalar(Int)
    case invalidRange(lowerBound: Character, upperBound: Character, description: String)
}

public extension Grammar {
    
    /// Creates a new grammar from a specification in Backus-Naur Form (BNF)
    ///
    ///     <pattern1> ::= <alternative1> | <alternative2>
    ///        <pattern2> ::= 'con' 'catenation'
    ///
    /// - Parameters:
    ///   - bnfString: String describing the grammar in BNF
    ///   - start: Start non-terminal
    @available(*, unavailable, renamed: "init(bnf:start:)")
    init(bnfString: String, start: String) throws {
        try self.init(bnf: bnfString, start: start)
    }
    
    /// Creates a new grammar from a specification in Backus-Naur Form (BNF)
    ///
    ///     <pattern1> ::= <alternative1> | <alternative2>
    ///        <pattern2> ::= 'con' 'catenation'
    ///
    /// - Parameters:
    ///   - bnf: String describing the grammar in BNF
    ///   - start: Start non-terminal
    init(bnf bnfString: String, start: String) throws {
        let grammar = bnfGrammar
        let parser = EarleyParser(grammar: grammar)
        let syntaxTree = try parser
            .syntaxTree(for: bnfString)
            .explode{["expression"].contains($0)}
            .first!
            .filter{!["optional-whitespace", "newlines"].contains($0)}!
        
        let ruleDeclarations = syntaxTree.allNodes(where: {$0.name == "rule"})
        
        func ruleName(from container: SyntaxTree<NonTerminal, Range<String.Index>>) -> String {
            return container
                .allNodes(where: {$0.name == "rule-name-char"})
                .flatMap{$0.leafs}
                .reduce("") { partialResult, range -> String in
                    partialResult.appending(bnfString[range])
            }
        }
        
        func character(fromCharacterExpression characterExpression: ParseTree) throws -> Character {
            guard let child = characterExpression.children?.first else {
                fatalError()
            }
            switch child {
            case .leaf(let range):
                return bnfString[range.lowerBound]
                
            case .node(key: "string-escaped-char", children: let children):
                guard let child = children.first else {
                    fatalError()
                }
                switch child {
                case .leaf:
                    fatalError()
                    
                case .node(key: "unicode-scalar", children: let children):
                    let hexString: String = children.dropFirst(3).dropLast().flatMap {$0.leafs}.map {bnfString[$0]}.joined()
                    // Grammar guarantees that hexString is always a valid hex integer literal
                    let charValue = Int(hexString, radix: 16)!
                    guard let scalar = UnicodeScalar(charValue) else {
                        throw LiteralParsingError.invalidUnicodeScalar(charValue)
                    }
                    return Character(scalar)
                
                case .node(key: "carriage-return", children: _):
                    return "\r"
                    
                case .node(key: "line-feed", children: _):
                    return "\n"
                    
                case .node(key: "tab-char", children: _):
                    return "\t"
                    
                case .node(key: "backslash", children: _):
                    return "\\"
                    
                default:
                    fatalError()
                }
                
            case .node(key: "escaped-single-quote", children: _):
                return "'"
                
            case .node(key: "escaped-double-quote", children: _):
                return "\""
                
            default:
                fatalError()
            }
        }
        
        func string(fromStringExpression stringExpression: ParseTree, knownString: String = "") throws -> String {
            if let children = stringExpression.children, children.count == 2 {
                let char = try character(fromCharacterExpression: children[1])
                return try string(fromStringExpression: children[0], knownString: "\(char)\(knownString)")
            } else {
                return knownString
            }
        }
        
        func terminal(fromLiteral literal: ParseTree) throws -> Terminal {
            guard let children = literal.children else {
                fatalError()
            }
            if children.count == 3 {
                let stringNode = children[1]
                return try Terminal(string: string(fromStringExpression: stringNode))
            } else if children.count == 1 {
                let rangeExpression = children[0]
                guard rangeExpression.root == "range-literal" else {
                    fatalError()
                }
                guard let children = rangeExpression.children, children.count == 5 else {
                    fatalError()
                }
                let lowerBound = try character(fromCharacterExpression: children[0].children![1])
                let upperBound = try character(fromCharacterExpression: children[4].children![1])
                
                guard lowerBound <= upperBound else {
                    throw LiteralParsingError.invalidRange(lowerBound: lowerBound, upperBound: upperBound, description: "lowerBound must be less than or equal to upperBound")
                }
                
                return Terminal(range: lowerBound ... upperBound)
            }
            
            fatalError()
        }
        
        func makeProductions(from expression: SyntaxTree<NonTerminal, Range<String.Index>>, named name: String) throws -> (productions: [Production], additionalRules: [Production]) {
            guard let type = expression.root?.name else {
                return ([], [])
            }
            guard let children = expression.children else {
                return ([], [])
            }
            switch type {
            case "alternation":
                let (lhs, lhsAdd) = try makeProductions(from: children[0], named: "\(name)-a0")
                let (rhs, rhsAdd) = try makeProductions(from: children[2], named: "\(name)-a1")
                return ((lhs + rhs).map {Production(pattern: NonTerminal(name: name), production: $0.production)}, lhsAdd + rhsAdd)
                
            case "concatenation":
                if children.count == 2 {
                    let (lhsProductions, lhsAdd) = try makeProductions(from: children[0], named: "\(name)-c0")
                    let (rhsProductions, rhsAdd) = try makeProductions(from: children[1], named: "\(name)-c1")
                    
                    return (crossProduct(lhsProductions, rhsProductions).map { arg -> Production in
                        let (lhs, rhs) = arg
                        return Production(pattern: NonTerminal(name: name), production: lhs.production + rhs.production)
                    }, lhsAdd + rhsAdd)
                } else if children.count == 1 {
                    return try makeProductions(from: children[0], named: name)
                } else {
                    fatalError()
                }
                
            case "expression-element":
                guard children.count == 1 else {
                    return ([], [])
                }
                switch children[0].root!.name {
                case "literal":
                    let t = try terminal(fromLiteral: children[0])
                    if t.isEmpty {
                        return ([Production(pattern: NonTerminal(name: name), production: [])], [])
                    } else {
                        return ([Production(pattern: NonTerminal(name: name), production: [.terminal(t)])], [])
                    }
                    
                case "rule-name-container":
                    let nonTerminalName = ruleName(from: children[0])
                    return ([Production(pattern: NonTerminal(name: name), production: [n(nonTerminalName)])], [])
                    
                case "expression-group":
                    guard let group = children[0].children else {
                        fatalError()
                    }
                    assert(group.count == 3)
                    return try makeProductions(from: group[1], named: name)
                    
                case "expression-repetition":
                    guard let group = children[0].children else {
                        fatalError()
                    }
                    assert(group.count == 3)
                    let subruleName = "\(name)-r"
                    let (subRules, additionalRules) = try makeProductions(from: group[1], named: subruleName)
                    let repetitionRules = subRules.map { rule in
                        Production(pattern: NonTerminal(name: subruleName), production: [n(subruleName)] + rule.production)
                    }
                    return ([Production(pattern: NonTerminal(name: name), production: [n(subruleName)])], additionalRules + subRules + repetitionRules)
                    
                case "expression-optional":
                    guard let group = children[0].children else {
                        fatalError()
                    }
                    assert(group.count == 3)
                    let subruleName = "\(name)-o"
                    let (productions, additionalProductions) = try makeProductions(from: group[1], named: subruleName)
                    let optionalProductions = productions.map {
                        Production(pattern: $0.pattern, production: [])
                    }
                    let subproduction = Production(pattern: NonTerminal(name: name), production: [n(subruleName)])
                    return ([subproduction], additionalProductions + productions + optionalProductions)
                    
                default:
                    fatalError()
                }
                
            default:
                fatalError()
            }
        }
        
        let (productions, helperRules): ([Production], [Production]) = try ruleDeclarations.reduce(into: ([], [])) { acc, ruleDeclaration in
            guard let children = ruleDeclaration.children, children.count == 3 else {
                return
            }
            let name = ruleName(from: children[0])
            let (productions, additionalRules) = try makeProductions(from: children[2], named: name)
            acc.0.append(contentsOf: productions)
            acc.1.append(contentsOf: additionalRules)
        }
        
        if (productions + helperRules).contains(where: { (production: Production) -> Bool in
            production.generatedNonTerminals.contains("EOL")
        }) && !(productions + helperRules).contains(where: { (production: Production) -> Bool in
            production.pattern == "EOL"
        }) {
            self.init(productions: productions + helperRules + ["EOL" ~~> t("\n")], start: NonTerminal(name: start), utilityNonTerminals: helperRules.map {$0.pattern}.collect(Set.init))
        } else {
            self.init(productions: productions + helperRules, start: NonTerminal(name: start), utilityNonTerminals: helperRules.map {$0.pattern}.collect(Set.init))
        }
        
    }
}
//
//  StringUtility.swift
//  Covfefe
//
//  Created by Palle Klewitz on 19.09.17.
//  Copyright (c) 2017 - 2020 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

public extension String {
    
    /// Returns the ranges of all matches of a regular expression which is provided as the pattern argument
    ///
    /// - Parameter pattern: Regular expression for which matches should be searched
    /// - Returns: Ranges of matches in the string for the given regular expression
    /// - Throws: An error indicating that the provided regular expression is invalid.
    func matches(for pattern: String) throws -> [Range<String.Index>] {
        return try matches(for: pattern, in: startIndex ..< endIndex)
    }
    
    /// Returns the ranges of all matches of the provided regular expression
    ///
    /// - Parameter expression: Regular expression for which matches should be searched
    /// - Returns: Ranges of matches in the string for the given regular expression
    func matches(for expression: NSRegularExpression) -> [Range<String.Index>] {
        return matches(for: expression, in: startIndex ..< endIndex)
    }

    /// Returns the ranges of all matches of a regular expression which is provided as the pattern argument
    ///
    /// - Parameters:
    ///   - pattern: Regular expression for which matches should be searched
    ///   - range: Range of the string which should be checked
    /// - Returns: Ranges of matches in the string for the given regular expression
    /// - Throws: An error indicating that the provided regular expression is invalid.
    func matches(for pattern: String, in range: Range<String.Index>) throws -> [Range<String.Index>] {
        let expression = try NSRegularExpression(pattern: pattern, options: [])
        return matches(for: expression, in: range)
    }
    
    /// Returns the ranges of all matches of the provided regular expression
    ///
    /// - Parameters:
    ///   - expression: Regular expression for which matches should be searched
    ///   - range: Range of the string which should be checked
    /// - Returns: Ranges of matches in the string for the given regular expression
    func matches(for expression: NSRegularExpression, in range: Range<String.Index>) -> [Range<String.Index>] {
        let range = NSRange(range, in: self)
        let matches = expression.matches(in: self, options: [], range: range)
        return matches.compactMap { match -> Range<String.Index>? in
            return Range(match.range, in: self)
        }
    }
    
    /// Returns a boolean value indicating that the string has a prefix which can be matched by the given regular expression
    ///
    /// - Parameter pattern: Regular expression
    /// - Returns: True, if the regular expression matches a substring beginning at the start index of the string
    /// - Throws: An error indicating that the provided regular expression is invalid
    func hasRegularPrefix(_ pattern: String) throws -> Bool {
        return try hasRegularPrefix(pattern, from: self.startIndex)
    }
    
    /// Returns a boolean value indicating that the string has a prefix which can be matched by the given regular expression
    ///
    /// - Parameter pattern: Regular expression
    /// - Returns: True, if the regular expression matches a substring beginning at the start index of the string
    func hasRegularPrefix(_ expression: NSRegularExpression) -> Bool {
        return hasRegularPrefix(expression, from: self.startIndex)
    }
    
    /// Returns a boolean value indicating that the string has a prefix beginning at the given start index
    /// which can be matched by the given regular expression
    ///
    /// - Parameter pattern: Regular expression
    /// - Parameter startIndex: Start index for the search
    /// - Returns: True, if the regular expression matches a substring beginning at the start index of the string
    /// - Throws: An error indicating that the provided regular expression is invalid
    func hasRegularPrefix(_ pattern: String, from startIndex: String.Index) throws -> Bool {
        return try rangeOfRegularPrefix(pattern, from: startIndex) != nil
    }
    
    /// Returns a boolean value indicating that the string has a prefix beginning at the given start index
    /// which can be matched by the given regular expression
    ///
    /// - Parameter pattern: Regular expression
    /// - Parameter startIndex: Start index for the search
    /// - Returns: True, if the regular expression matches a substring beginning at the start index of the string
    /// - Throws: An error indicating that the provided regular expression is invalid
    func hasRegularPrefix(_ expression: NSRegularExpression, from startIndex: String.Index) -> Bool {
        return rangeOfRegularPrefix(expression, from: startIndex) != nil
    }
    
    /// Returns the range of a match for the given regular expression beginning at the start of the string
    ///
    /// - Parameter pattern: Regular expression
    /// - Returns: Range of the prefix matched by the regular expression or nil, if no match was found
    /// - Throws: An error indicating that the provided regular expression is invalid
    func rangeOfRegularPrefix(_ pattern: String) throws -> Range<String.Index>? {
        return try rangeOfRegularPrefix(pattern, from: self.startIndex)
    }
    
    /// Returns the range of a match for the given regular expression beginning at the start of the string
    ///
    /// - Parameter pattern: Regular expression
    /// - Returns: Range of the prefix matched by the regular expression or nil, if no match was found
    /// - Throws: An error indicating that the provided regular expression is invalid
    func rangeOfRegularPrefix(_ expression: NSRegularExpression) -> Range<String.Index>? {
        return rangeOfRegularPrefix(expression, from: self.startIndex)
    }
    
    /// Returns the range of a match for the given regular expression beginning at the start of the string
    ///
    /// - Parameter pattern: Regular expression
    /// - Parameter startIndex: Start index for the search
    /// - Returns: Range of the prefix matched by the regular expression or nil, if no match was found
    /// - Throws: An error indicating that the provided regular expression is invalid
    func rangeOfRegularPrefix(_ pattern: String, from lowerBound: String.Index) throws -> Range<String.Index>? {
        let expression = try NSRegularExpression(pattern: pattern, options: [])
        return rangeOfRegularPrefix(expression, from: lowerBound)
    }
    
    /// Returns the range of a match for the given regular expression beginning at the start of the string
    ///
    /// - Parameter pattern: Regular expression
    /// - Parameter startIndex: Start index for the search
    /// - Returns: Range of the prefix matched by the regular expression or nil, if no match was found
    /// - Throws: An error indicating that the provided regular expression is invalid
    func rangeOfRegularPrefix(_ expression: NSRegularExpression, from lowerBound: String.Index) -> Range<String.Index>? {
        let range = NSRange(lowerBound ..< self.endIndex, in: self)
        guard let match = expression.firstMatch(in: self, options: .anchored, range: range) else {
            return nil
        }
        return Range(match.range, in: self)
    }
    
    /// Returns a boolean value indicating that the string ends with a substring matched by the given regular expression
    ///
    /// - Parameter pattern: Regular expression
    /// - Returns: True, if a match was found
    /// - Throws: An error indicating that the regular expression is invalid
    func hasRegularSuffix(_ pattern: String) throws -> Bool {
        return try matches(for: pattern).contains(where: { range -> Bool in
            range.upperBound == self.endIndex
        })
    }
    
    /// Returns a boolean value indicating that the string ends with a substring matched by the given regular expression
    ///
    /// - Parameter pattern: Regular expression
    /// - Returns: True, if a match was found
    /// - Throws: An error indicating that the regular expression is invalid
    func hasRegularSuffix(_ expression: NSRegularExpression) -> Bool {
        return matches(for: expression).contains(where: { range -> Bool in
            range.upperBound == self.endIndex
        })
    }
    
    /// Returns the range of a substring matched by the given regular expression ending at the end index of the string
    ///
    /// - Parameter pattern: Regular expression
    /// - Returns: Range of the match or nil, if no match was found
    /// - Throws: An error indicating that the regular expression is invalid
    func rangeOfRegularSuffix(_ pattern: String) throws -> Range<String.Index>? {
        return try matches(for: pattern).first(where: { range -> Bool in
            range.upperBound == self.endIndex
        })
    }
    
    /// Returns the range of a substring matched by the given regular expression ending at the end index of the string
    ///
    /// - Parameter pattern: Regular expression
    /// - Returns: Range of the match or nil, if no match was found
    /// - Throws: An error indicating that the regular expression is invalid
    func rangeOfRegularSuffix(_ expression: NSRegularExpression) throws -> Range<String.Index>? {
        return matches(for: expression).first(where: { range -> Bool in
            range.upperBound == self.endIndex
        })
    }
    
    /// Returns a boolean value indicating that the string has a prefix described by the given terminal symbol.
    ///
    /// - Parameter prefix: Sequence of terminal symbols
    /// - Returns: True, if the string has a prefix described by the given non-terminal sequence
    func hasPrefix(_ prefix: Terminal) -> Bool {
        return hasPrefix(prefix, from: self.startIndex)
    }
    
    /// Returns a boolean value indicating that the string has a prefix from the given start index described by the given
    /// terminal symbol
    ///
    /// - Parameters:
    ///   - prefix: Sequence of terminal symbols
    ///   - startIndex: Index from which the search should start
    /// - Returns: True, if the string has a prefix from the given start index described by the given non-terminal sequence
    func hasPrefix(_ prefix: Terminal, from startIndex: String.Index) -> Bool {
        switch prefix {
        case .characterRange(let range, _):
            guard let first = self[startIndex...].first else {
                return false
            }
            return range.contains(first)
            
        case .regularExpression(let expression, _):
            return hasRegularPrefix(expression, from: startIndex)
            
        case .string(string: let string, hash: _):
            return self[startIndex...].hasPrefix(string)
        }
    }
    
    /// Returns the range of the prefix described by the given sequence of terminal symbols
    /// starting a the given start index
    ///
    /// - Parameter
    ///   - prefix: Sequence of terminal symbols
    ///   - startIndex: Index from which the search should start
    /// - Returns: The range of the prefix or nil, if no matching prefix has been found
    func rangeOfPrefix(_ prefix: Terminal, from startIndex: String.Index) -> Range<String.Index>? {
        switch prefix {
        case .characterRange(let range, _):
            guard let first = self[startIndex...].first else {
                return nil
            }
            if range.contains(first) {
                return startIndex ..< self.index(after: startIndex)
            } else {
                return nil
            }
            
        case .regularExpression(let expression, _):
            return rangeOfRegularPrefix(expression, from: startIndex)
            
        case .string(string: let prefixString, hash: _):
            let range = startIndex ..< (self.index(startIndex, offsetBy: prefixString.count, limitedBy: endIndex) ?? endIndex)
            return self.range(of: prefixString, range: range)
        }
    }
        
    /// Performs replacements using the given replacement rules.
    /// The replacements are performed in order.
    /// Each replacement is a tuple of strings, where the first string is the pattern that is replaced
    /// and the second string is the string that is placed.
    ///
    /// - Parameter replacements: Sequence of replacements to be performed
    /// - Returns: String generated by performing the sequence of replacements provided
    func replacingOccurrences<Replacements: Sequence>(_ replacements: Replacements) -> String where Replacements.Element == (String, String) {
        return replacements.reduce(self) { acc, rule in
            acc.replacingOccurrences(of: rule.0, with: rule.1)
        }
    }
    
    /// Escapes all special characters that need to be escaped to be escaped for the string to be printed as a string literal.
    /// This includes backslashes, line feeds, carriage returns and tab characters.
    var literalEscaped: String {
        return self.replacingOccurrences(
            [
                ("\\", "\\\\"),
                ("\n", "\\n"),
                ("\r", "\\r"),
                ("\t", "\\t"),
            ]
        )
    }

    /// Escapes all special characters that need to be escaped to be escaped for the string to be printed as a string literal enclosed by single quotes.
    /// This includes single quotes, backslashes, line feeds, carriage returns and tab characters.
    var singleQuoteLiteralEscaped: String {
        return literalEscaped.replacingOccurrences(of: "'", with: "\\'")
    }
    
    /// Escapes all special characters that need to be escaped to be escaped for the string to be printed as a string literal enclosed by double quotes.
    /// This includes double quotes, backslashes, line feeds, carriage returns and tab characters.
    var doubleQuoteLiteralEscaped: String {
        return literalEscaped.replacingOccurrences(of: "\"", with: "\\\"")
    }
}
//
//  Productions.swift
//  Covfefe
//
//  Created by Palle Klewitz on 07.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

/// A production describing what symbols can be generated starting from a given non-terminal pattern
public struct Production: Codable {
    
    /// Starting pattern
    public let pattern: NonTerminal
    
    /// Symbols produced from the starting pattern
    public let production: [Symbol]
    
    /// Chain of non-terminals which have been eliminated during normalization
    public let nonTerminalChain: [NonTerminal]?
    
    public let hashValue: Int
    
    public func hash(into hasher: inout Hasher) {
        hasher.combine(hashValue)
    }
    
    /// Creates a new production
    ///
    /// - Parameters:
    ///   - pattern: Starting pattern
    ///   - production: Generated sequence of symbols
    public init(pattern: NonTerminal, production: ProductionString) {
        self.pattern = pattern
        self.production = production.characters
        self.nonTerminalChain = nil
        
        self.hashValue = pattern.hashValue ^ self.production.map{$0.hashValue}.reduce(0, ^)
    }
    
    /// Creates a new production
    ///
    /// - Parameters:
    ///   - pattern: Starting pattern
    ///   - production: Generated sequence of symbols
    ///   - chain: Non-terminals which have been filtered out during normalization
    public init(pattern: NonTerminal, production: [Symbol], chain: [NonTerminal]? = nil) {
        self.pattern = pattern
        self.production = production
        self.nonTerminalChain = chain
        
        self.hashValue = pattern.hashValue ^ production.map{$0.hashValue}.reduce(0, ^)
    }
    
    /// A production is final if it only generates terminal symbols
    public var isFinal: Bool {
        return self.production.allSatisfy { symbol -> Bool in
            if case .terminal(_) = symbol {
                return true
            } else {
                return false
            }
        }
    }
    
    /// A production is in Chomsky normal form if it generates exactly 2 non-terminals
    /// exclusive or one or zero terminal symbols
    public var isInChomskyNormalForm: Bool {
        if isFinal {
            return production.count == 1
        }
        return self.production.allSatisfy { symbol -> Bool in
            if case .nonTerminal(_) = symbol {
                return true
            } else {
                return false
            }
        } && self.production.count == 2
    }
    
    /// Sequence of terminals generated by this production
    public var generatedTerminals: [Terminal] {
        return production.compactMap{ symbol -> Terminal? in
            guard case .terminal(let terminal) = symbol  else {
                return nil
            }
            return terminal
        }
    }
    
    var terminalPrefix: [Terminal] {
        let prefix = production.prefix(while: { symbol -> Bool in
            if case .terminal(_) = symbol {
                return true
            } else {
                return false
            }
        })
        return prefix.compactMap { symbol -> Terminal? in
            guard case .terminal(let terminal) = symbol else {
                return nil
            }
            return terminal
        }
    }
    
    var terminalSuffix: [Terminal] {
        let suffix = production.reversed().prefix(while: { symbol -> Bool in
            if case .terminal(_) = symbol {
                return true
            } else {
                return false
            }
        }).reversed()
        
        return suffix.compactMap { symbol -> Terminal? in
            guard case .terminal(let terminal) = symbol else {
                return nil
            }
            return terminal
        }
    }
    
    var generatedNonTerminals: [NonTerminal] {
        return production.compactMap { symbol -> NonTerminal? in
            guard case .nonTerminal(let nonTerminal) = symbol else {
                return nil
            }
            return nonTerminal
        }
    }
    
    func generatesEmpty(in grammar: Grammar) -> Bool {
        let groupedProductions = Dictionary(grouping: grammar.productions, by: {$0.pattern})
        
        func generatesEmpty(_ nonTerminal: NonTerminal, path: Set<NonTerminal>) -> Bool {
            if path.contains(nonTerminal) {
                return false
            }
            
            let directProductions = groupedProductions[nonTerminal, default: []]
            return directProductions.contains { production -> Bool in
                if production.production.isEmpty {
                    return true
                }
                return production.generatedNonTerminals.count == production.production.count
                    && production.generatedNonTerminals.allSatisfy { pattern -> Bool in
                        generatesEmpty(pattern, path: path.union([nonTerminal]))
                }
            }
        }
        
        return self.production.allSatisfy { symbol -> Bool in
            switch symbol {
            case .terminal:
                return false
                
            case .nonTerminal(let nonTerminal):
                return generatesEmpty(nonTerminal, path: [])
            }
        }
    }
}

extension Production: Hashable {
    
    public static func ==(lhs: Production, rhs: Production) -> Bool {
        return lhs.pattern == rhs.pattern && lhs.production == rhs.production
    }
}

extension Production: CustomStringConvertible {
    public var description: String {
        return "\(pattern.name) ~~> \(production.map{$0.description}.joined(separator: " "))"
    }
}

extension Production: CustomDebugStringConvertible {
    public var debugDescription: String {
        return """
        production {
            pattern: \(self.pattern)
            produces: \(self.production.map{$0.description})
            chain: \(self.nonTerminalChain?.map{$0.description}.joined(separator: ", ") ?? "empty")
        }
        """
    }
}

precedencegroup ProductionPrecedence {
    associativity: left
    lowerThan: AdditionPrecedence
}

infix operator ~~> : ProductionPrecedence

/// Generates a production from a given non-terminal and produced sequence of symbols
///
/// - Parameters:
///   - lhs: Non-terminal pattern
///   - rhs: Produced string of symbols
/// - Returns: Production with the given pattern and generated result
public func ~~> (lhs: NonTerminal, rhs: ProductionString) -> Production {
    return Production(pattern: lhs, production: rhs)
}

/// Generates a set of productions from a given non-terminal and produced result
///
/// - Parameters:
///   - lhs: Non-terminal pattern
///   - rhs: Collection of possible produced strings of symbols
/// - Returns: Productions with the given pattern and generated results
public func ~~> (lhs: NonTerminal, rhs: ProductionResult) -> [Production] {
    return rhs.elements.map { producedString in
        return Production(pattern: lhs, production: producedString)
    }
}

/// Generates a production from the given non-terminal to the given symbol
///
/// - Parameters:
///   - lhs: Non-terminal pattern
///   - rhs: Produced symbol
/// - Returns: Production with the given pattern generating the given symbol
public func ~~> (lhs: NonTerminal, rhs: Symbol) -> Production {
    return Production(pattern: lhs, production: [rhs])
}

//
//  Terminal.swift
//  Covfefe
//
//  Created by Palle Klewitz on 20.02.18.
//

import Foundation

/// A terminal symbol which can occur in a string recognized by a parser and which cannot be
/// replaced by any production
public enum Terminal {
    /// A terminal that is a string. The terminal is matched when the tokenized subsequence of a word is equal to this string.
    case string(string: String, hash: Int)
    
    /// A terminal that is a range of characters. The terminal is matched when the tokenized subsequence is a character contained in this range.
    case characterRange(range: ClosedRange<Character>, hash: Int)
    
    /// A terminal that is a regular epxression. The terminal is matched when the tokenized subsequence is contained in the language generated by the given regular expression
    case regularExpression(expression: NSRegularExpression, hash: Int)
}

public extension Terminal {
    
    /// Creates a terminal that is a string. The terminal is matched when the tokenized subsequence of a word is equal to this string.
    ///
    /// - Parameter string: Terminal string
    init(string: String) {
        self = .string(string: string, hash: string.hashValue)
    }
    
    /// Creates a terminal that is a range of characters. The terminal is matched when the tokenized subsequence is a character contained in this range.
    ///
    /// - Parameter range: Range of matched characters
    init(range: ClosedRange<Character>) {
        self = .characterRange(range: range, hash: range.hashValue)
    }
    
    /// Creates a terminal that is a regular epxression. The terminal is matched when the tokenized subsequence is contained in the language generated by the given regular expression
    ///
    /// - Parameter expression: Regular expression specifying the language matched by terminal
    /// - Throws: An error indicating that the regular expression is invalid
    init(expression: String) throws {
        let regex = try NSRegularExpression(pattern: expression, options: [])
        self = .regularExpression(expression: regex, hash: expression.hashValue)
    }
    
    
    /// Indicates that this terminal matches the empty string and only the empty string.
    var isEmpty: Bool {
        switch self {
        case .characterRange:
            return false
            
        case .regularExpression(let expression, _):
            return expression.pattern.isEmpty
            
        case .string(let string, _):
            return string.isEmpty
        }
    }
}

extension Terminal: ExpressibleByStringLiteral {
    public init(stringLiteral value: String) {
        self.init(string: value)
    }
}

extension Terminal: Hashable {
    public static func == (lhs: Terminal, rhs: Terminal) -> Bool {
        switch (lhs, rhs) {
        case (.string(string: let ls, hash: _), .string(string: let rs, hash: _)):
            return ls == rs
            
        case (.characterRange(range: let lr, hash: _), .characterRange(range: let rr, hash: _)):
            return lr == rr
            
        case (.regularExpression(expression: let le, hash: _), .regularExpression(expression: let re, hash: _)):
            return le.pattern == re.pattern
            
        default:
            return false
        }
    }
    
    public var hashValue: Int {
        switch self {
        case .characterRange(range: _, hash: let hash):
            return hash
            
        case .regularExpression(expression: _, hash: let hash):
            return hash
            
        case .string(string: _, hash: let hash):
            return hash
        }
    }
    
    public func hash(into hasher: inout Hasher) {
        hasher.combine(hashValue)
    }
}

extension Terminal: CustomStringConvertible {
    public var description: String {
        switch self {
        case .string(let string, _):
            return string
            
        case .characterRange(let range, _):
            return "\(range.lowerBound) ... \(range.upperBound)"
            
        case .regularExpression(let expression, _):
            return expression.pattern
        }
    }
}

extension Terminal: Codable {
    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        switch try container.decode(TerminalCoding.self, forKey: .type) {
        case .string:
            let string = try container.decode(String.self, forKey: .value)
            self = .string(string: string, hash: string.hashValue)
            
        case .characterRange:
            let range = try container.decode(ClosedRange<Character>.self, forKey: .value)
            self = .characterRange(range: range, hash: range.hashValue)
            
        case .regularExpression:
            let pattern = try container.decode(String.self, forKey: .value)
            self = try .regularExpression(expression: NSRegularExpression(pattern: pattern, options: []), hash: pattern.hashValue)
        }
    }
    
    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        
        switch self {
        case .string(let string, _):
            try container.encode(TerminalCoding.string, forKey: .type)
            try container.encode(string, forKey: .value)
            
        case .characterRange(let range, _):
            try container.encode(TerminalCoding.characterRange, forKey: .type)
            try container.encode(range, forKey: CodingKeys.value)
            
        case .regularExpression(let expression, _):
            try container.encode(TerminalCoding.regularExpression, forKey: .type)
            try container.encode(expression.pattern, forKey: .value)
        }
    }
    
    private enum CodingKeys: String, CodingKey {
        case type
        case value
    }
    
    private enum TerminalCoding: String, Codable {
        case string
        case characterRange
        case regularExpression
    }
}

extension Character: Codable {
    public func encode(to encoder: Encoder) throws {
        var container = encoder.singleValueContainer()
        try container.encode(String(self))
    }
    
    public init(from decoder: Decoder) throws {
        let container = try decoder.singleValueContainer()
        let str = try container.decode(String.self)
        guard let char = str.first, str.count == 1 else {
            throw DecodingError.dataCorruptedError(in: container, debugDescription: "Missing character")
        }
        self = char
    }
}

#if !swift(>=4.1)
extension ClosedRange: Hashable where Bound: Hashable {
    public var hashValue: Int {
        return lowerBound.hashValue ^ upperBound.hashValue
    }
}
#endif
//
//  PrefixGrammar.swift
//  Covfefe
//
//  Created by Palle Klewitz on 16.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

public extension Grammar {
    
    /// Generates a grammar which recognizes all prefixes of the original grammar.
    ///
    /// For example if a grammar would generate `a*(b+c)`,
    /// the prefix grammar would generate the empty string, `a`, `a*`, `a*(`, `a*(b`
    /// `a*(b+` `a*(b+c` and `a*(b+c)`
    ///
    /// - Returns: A grammar generating all prefixes of the original grammar
    func prefixGrammar() -> Grammar {
        let prefixProductions = productions.flatMap { (production) -> [Production] in
            let prefixes = production.production.prefixes().map { sequence -> [Symbol] in
                guard let last = sequence.last else {
                    return sequence
                }
                guard case .nonTerminal(let nonTerminal) = last else {
                    return sequence
                }
                return sequence.dropLast() + [.nonTerminal(NonTerminal(name: "\(nonTerminal.name)-pre"))]
            } + [[]]
            
            return prefixes.map {Production(pattern: NonTerminal(name: "\(production.pattern.name)-pre"), production: $0)}
        }
        let allProductions: [Production] = self.productions + prefixProductions + (NonTerminal(name: "\(self.start.name)-pre-start") ~~> n("\(self.start.name)-pre") <|> .nonTerminal(self.start))
        return Grammar(
            productions: allProductions.uniqueElements().collect(Array.init),
            start: NonTerminal(name: "\(self.start.name)-pre-start"),
            utilityNonTerminals: self.utilityNonTerminals
        )
    }
}
//
//  Tokenizer.swift
//  Covfefe
//
//  Created by Palle Klewitz on 15.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

/// A string tokenizer which tokenizes a string based on final productions of a context free grammar.
public protocol Tokenizer {

    /// Tokenizes the given word and returns a sequence of possible tokens for each unit of the string
    ///
    /// For a grammar
    ///
    ///        A -> a | A B
    ///        B -> a | B b
    ///
    /// and a string "ab"
    ///
    /// The tokenizer generates the tokenization
    ///
    ///        [[a], [b]]
    ///
    /// - Parameter word: Word which should be tokenized
    /// - Returns: Tokenization of the word
    /// - Throws: A syntax error if the word could not be tokenized according to rules of the recognized language
    func tokenize(_ word: String) throws -> [[(terminal: Terminal, range: Range<String.Index>)]]
}

/// A simple tokenizer which uses a all terminals in a grammar for tokenization.
///
/// Terminals may not overlap partially.
/// If two terminals, `ab` and `bc` exist and `abc` is tokenized,
/// the tokenizer will not find an occurrence of the second terminal.
public struct DefaultTokenizer: Tokenizer {
    
    /// All terminals which the tokenizer can recognize
    private let terminals: [Terminal]
    
    /// Creates a new tokenizer using a Chomsky normalized grammar
    ///
    /// - Parameter grammar: Grammar specifying the rules with which a string should be tokenized.
    public init(grammar: Grammar) {
        self.terminals = grammar.productions.flatMap{$0.generatedTerminals}
    }
    
    /// Tokenizes the given word and returns a sequence of possible tokens for each unit of the string
    ///
    /// For a grammar
    ///
    ///        A -> a | A B
    ///        B -> a | B b
    ///
    /// and a string "ab"
    ///
    /// The tokenizer generates the tokenization
    ///
    ///        [[a], [b]]
    ///
    /// - Parameter word: Word which should be tokenized
    /// - Returns: Tokenization of the word
    /// - Throws: A syntax error if the word could not be tokenized according to rules of the recognized language
    public func tokenize(_ word: String) throws -> [[(terminal: Terminal, range: Range<String.Index>)]] {
        return try tokenize(word: word, from: word.startIndex, partialResult: [])
    }
    
    /// Recursive tokenization function
    ///
    /// - Parameters:
    ///   - word: Word which should be tokenized
    ///   - startIndex: Index from which the tokenization should start
    ///   - partialResult: Tokenization of the substring up to the start index
    /// - Returns: A tokenization of the substring starting at the start index
    /// - Throws: A syntax error if the string contained a token which was is not recognized by the tokenizer
    private func tokenize(word: String, from startIndex: String.Index, partialResult: [[(Terminal, Range<String.Index>)]]) throws -> [[(terminal: Terminal, range: Range<String.Index>)]] {
        if word[startIndex...].isEmpty {
            return partialResult
        }
        let matches = terminals.filter { terminal -> Bool in
            word.hasPrefix(terminal, from: startIndex)
        }
        guard
            let first = matches.first,
            let firstMatchRange = word.rangeOfPrefix(first, from: startIndex)
        else {
            throw SyntaxError(range: startIndex ..< word.endIndex, in: word, reason: .unknownToken)
        }
        
        let terminalRanges = matches.map{($0, firstMatchRange)}
        return try tokenize(word: word, from: firstMatchRange.upperBound, partialResult: partialResult + [terminalRanges])
    }
}
//
//  EarleyParser.swift
//  Covfefe
//
//  Created by Palle Klewitz on 27.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

/// Represents a partial parse of a production
fileprivate struct ParseStateItem {
    /// The partially parsed production
    let production: Production
    
    /// The index of the next symbol to be parsed
    let productionPosition: Int
    
    /// The index of the first token parsed in this partial parse
    let startTokenIndex: Int
}

extension ParseStateItem {
    var isCompleted: Bool {
        return !production.production.indices.contains(productionPosition)
    }
    
    func advanced() -> ParseStateItem {
        guard !isCompleted else {
            return self
        }
        return ParseStateItem(
            production: production,
            productionPosition: productionPosition + 1,
            startTokenIndex: startTokenIndex
        )
    }
    
    var nextSymbol: Symbol? {
        guard production.production.indices.contains(productionPosition) else {
            return nil
        }
        return production.production[productionPosition]
    }
}

extension ParseStateItem: Hashable {
    static func ==(lhs: ParseStateItem, rhs: ParseStateItem) -> Bool {
        return lhs.production == rhs.production
            && lhs.productionPosition == rhs.productionPosition
            && lhs.startTokenIndex == rhs.startTokenIndex
    }
    
    var hashValue: Int {
        return production.hashValue ^ productionPosition.hashValue ^ (startTokenIndex.hashValue << 32) ^ (startTokenIndex.hashValue >> 32)
    }
    
    public func hash(into hasher: inout Hasher) {
        hasher.combine(hashValue)
    }
}

extension ParseStateItem: CustomStringConvertible {
    var description: String {
        let producedString = production.production.map { symbol -> String in
            switch symbol {
            case .nonTerminal(let nonTerminal):
                return "<\(nonTerminal.name)>"
                
            case .terminal(let terminal):
                return "\"\(terminal.description.replacingOccurrences(of: "\n", with: "\\n"))\""
            }
        }.enumerated().reduce("") { (partialResult, string) in
            if string.offset == productionPosition {
                return partialResult.appending(" • \(string.element)")
            }
            return partialResult.appending(" \(string.element)")
        }
        if isCompleted {
            return "<\(production.pattern)> ::=\(producedString) • (\(startTokenIndex))"
        } else {
            return "<\(production.pattern)> ::=\(producedString) (\(startTokenIndex))"
        }
        
    }
}

/// A parse edge
fileprivate struct ParsedItem {
    /// The production
    let production: Production
    
    /// State index at which the item was completed
    let completedIndex: Int
}

extension ParsedItem: Hashable {
    var hashValue: Int {
        return production.hashValue ^ completedIndex.hashValue
    }
    
    public func hash(into hasher: inout Hasher) {
        hasher.combine(hashValue)
    }
    
    static func ==(lhs: ParsedItem, rhs: ParsedItem) -> Bool {
        return lhs.production == rhs.production && lhs.completedIndex == rhs.completedIndex
    }
}

extension ParsedItem: CustomStringConvertible {
    var description: String {
        let producedString = production.production.reduce("") { (partialResult, symbol) -> String in
            switch symbol {
            case .nonTerminal(let nonTerminal):
                return partialResult.appending(" <\(nonTerminal.name)>")
                
            case .terminal(let terminal):
                return partialResult.appending(" '\(terminal.description.replacingOccurrences(of: "\n", with: "\\n"))'")
            }
        }
        return "<\(production.pattern.name)> ::=\(producedString) (\(completedIndex))"
    }
}


/// A parser generator implementation that internally uses
/// the Earley algorithm.
///
/// Creates a syntax tree in O(n^3) worst case run time.
/// For unambiguous grammars, the run time is O(n^2).
/// For almost all LR(k) grammars, the run time is O(n).
/// Best performance can be achieved with left recursive grammars.
///
/// For ambiguous grammars, the runtime for parses may increase,
/// as some expressions have exponentially many possible parse trees depending on expression length.
/// This exponential growth can be avoided by only generating a single parse tree with `syntaxTree(for:)`.
///
/// For unambiguous grammars, the Earley parser performs better than the CYK parser.
public struct EarleyParser: AmbiguousGrammarParser {
    
    /// The grammar recognized by the parser
    public let grammar: Grammar
    
    /// All non terminals which have productions which can produce an empty string
    private let nullableNonTerminals: Set<NonTerminal>
    
    /// Generates an earley parser for the given grammar.
    ///
    /// Creates a syntax tree in O(n^3) worst case run time.
    /// For unambiguous grammars, the run time is O(n^2).
    /// For almost all LR(k) grammars, the run time is O(n).
    /// Best performance can be achieved with left recursive grammars.
    public init(grammar: Grammar) {
        self.grammar = grammar
        self.nullableNonTerminals = grammar.productions.compactMap { production in
            if production.generatesEmpty(in: grammar) {
                return production.pattern
            } else {
                return nil
            }
        }.collect(Set.init)
    }
    
    // Collects productions which can be reached indirectly.
    // e.g. for a production A -> BC which is not already partially parsed, all productions starting from B will be collected.
    private func predict(
        productions: [NonTerminal: [Production]],
        item: ParseStateItem,
        currentIndex: Int,
        knownItems: Set<ParseStateItem>
    ) -> [ParseStateItem] {
        guard
            let symbol = item.nextSymbol,
            case .nonTerminal(let nonTerminal) = symbol
        else {
            return []
        }
        // Create new parse states for each non terminal which has been reached and filter out every known state
        let addedItems = productions[nonTerminal, default: []].map {
            ParseStateItem(production: $0, productionPosition: 0, startTokenIndex: currentIndex)
        }
        
        // If a nullable symbol was added, advance the production that added this symbol
        if nullableNonTerminals.contains(nonTerminal) {
            return addedItems + [item.advanced()]
        }
        
        return addedItems
    }
    
    // Finds productions which produce a non terminal and checks,
    // if the expected terminal matches the observed one.
    private func scan(state: Set<ParseStateItem>, token: Terminal) -> Set<ParseStateItem> {
        return state.reduce(into: []) { partialResult, item in
            // Check that the current symbol of the production is a terminal and if yes
            // check that it matches the current token
            guard
                let next = item.nextSymbol,
                case .terminal(let terminal) = next,
                terminal == token
            else {
                return
            }
            // Create a new state with an advanced production position.
            partialResult.insert(item.advanced())
        }
    }
    
    // Finds completed items
    private func complete(
        item: ParseStateItem,
        allStates: [Set<ParseStateItem>],
        knownItems: Set<ParseStateItem>
    ) -> [ParseStateItem] {
        guard item.isCompleted else {
            return []
        }
        
        // Find the items which were used to enqueue the completed items
        let generatingItems = (allStates.indices.contains(item.startTokenIndex) ? allStates[item.startTokenIndex] : [])
            .filter { stateItem in
                !stateItem.isCompleted
                    && stateItem.nextSymbol == Symbol.nonTerminal(item.production.pattern)
            }
        
        return generatingItems.map { item in
            item.advanced()
        }
    }
    
    private func processState(
        productions: [NonTerminal: [Production]],
        allStates: [Set<ParseStateItem>],
        knownItems: Set<ParseStateItem>,
        newItems: Set<ParseStateItem>
    ) -> Set<ParseStateItem> {
        var addedItems: Set<ParseStateItem> = newItems
        var knownItems: Set<ParseStateItem> = knownItems
        
        repeat {
            addedItems = addedItems.reduce(into: Set<ParseStateItem>()) { (addedItems, item) in
                switch item.nextSymbol {
                case .none:
                    let completed = complete(item: item, allStates: allStates, knownItems: knownItems)
                    addedItems.reserveCapacity(addedItems.count + completed.count)
                    addedItems.formUnion(completed)

                case .some(.terminal):
                    break // Terminals are processed in scan before

                case .some(.nonTerminal):
                    let predicted = predict(productions: productions, item: item, currentIndex: allStates.count, knownItems: knownItems)
                    addedItems.reserveCapacity(addedItems.count + predicted.count)
                    addedItems.formUnion(predicted)
                }
            }.subtracting(knownItems)

            knownItems.reserveCapacity(addedItems.count + knownItems.count)
            knownItems.formUnion(addedItems)

        } while !addedItems.isEmpty

        return knownItems
    }
    
    private func buildSyntaxTrees(
        stateCollection: [Set<ParsedItem>],
        tokenization: [[(terminal: Terminal, range: Range<String.Index>)]],
        rootItem: ParsedItem,
        startIndex: Int,
        ignoreAmbiguousItems: Bool
    ) -> [ParseTree] {
        
        guard !rootItem.production.production.isEmpty else {
            return [SyntaxTree(key: rootItem.production.pattern)]
        }
        
        func resolve(
            unresolved: ArraySlice<Symbol>,
            position: Int
        ) -> [[(Int, Either<ParsedItem, Terminal>)]] {
            guard position <= rootItem.completedIndex else {
                return []
            }
            
            guard let first = unresolved.first else {
                if position == rootItem.completedIndex {
                    return [[]]
                } else {
                    return []
                }
            }
            switch first {
            case .nonTerminal(let nonTerminal):
                let candidates = stateCollection[position].lazy.filter { candidate -> Bool in
                    candidate.production.pattern == nonTerminal
                        && (candidate != rootItem || startIndex != position)
                        && candidate.completedIndex <= rootItem.completedIndex
                }
                let resolvedCandidates = candidates.lazy.flatMap { candidate -> [[(Int, Either<ParsedItem, Terminal>)]] in
                    let resolved = resolve(
                        unresolved: unresolved.dropFirst(),
                        position: candidate.completedIndex
                    )
                    return resolved.map{$0 + [(position, .first(candidate))]}
                }
                if ignoreAmbiguousItems {
                    guard let first = resolvedCandidates.first else {
                        return []
                    }
                    return [first]
                } else {
                    return resolvedCandidates.collect(Array.init)
                }
                
            case .terminal(let terminal):
                // A terminal can only be scanned if there is at least one token left.
                guard position < tokenization.count else {
                    return []
                }
                // The position might be wrong, so we check that the terminal actually occurred at the current position
                guard tokenization[position].contains(where: {$0.terminal == terminal}) else {
                    return []
                }
                // Try to resolve the rest.
                let rest = resolve(unresolved: unresolved.dropFirst(), position: position + 1)
                return rest.map{$0 + [(position, .second(terminal))]}
            }
        }
        
        // Faster return for unambiguous grammars
        if ignoreAmbiguousItems {
            let first = resolve(unresolved: ArraySlice(rootItem.production.production), position: startIndex)[0].reversed()
            let children = first.map { (element) -> ParseTree in
                let (position, root) = element
                return root.combine({ parsedItem -> ParseTree in
                    self.buildSyntaxTrees(
                        stateCollection: stateCollection,
                        tokenization: tokenization,
                        rootItem: parsedItem,
                        startIndex: position,
                        ignoreAmbiguousItems: ignoreAmbiguousItems
                    ).first!
                }, { _ -> ParseTree in
                    return .leaf(tokenization[position].first!.range)
                })
            }
            return [ParseTree.node(key: rootItem.production.pattern, children: children)]
        }
        
        let parseTrees = resolve(
            unresolved: ArraySlice(rootItem.production.production),
            position: startIndex
        ).map { children -> [(Int, Either<ParsedItem, Terminal>)] in
            children.reversed()
        }.lazy.flatMap { children -> [[ParseTree]] in
            children.map { element -> [ParseTree] in
                let (position, root) = element
                return root.combine({ parsedItem -> [ParseTree] in
                    self.buildSyntaxTrees(
                        stateCollection: stateCollection,
                        tokenization: tokenization,
                        rootItem: parsedItem,
                        startIndex: position,
                        ignoreAmbiguousItems: ignoreAmbiguousItems
                    )
                }, { _ -> [ParseTree] in
                    return [.leaf(tokenization[position].first!.range)]
                })
            }.combinations()
        }.map { (children) -> ParseTree in
            ParseTree.node(key: rootItem.production.pattern, children: children)
        }
        
        if parseTrees.isEmpty {
            fatalError("Internal error: Could not build syntax tree after successful parse.")
        }
        
        if ignoreAmbiguousItems {
            return [parseTrees.first!]
        }
        
        return parseTrees.collect(Array.init)
    }
    
    private func parse(_ string: String) throws -> ([Set<ParsedItem>], [[(terminal: Terminal, range: Range<String.Index>)]]) {
        //TODO: Better support for right recursion
        
        let nonTerminalProductions = Dictionary(grouping: grammar.productions, by: {$0.pattern})
        
        // The start state contains all productions which can be reached directly from the starting non terminal
        let initState = nonTerminalProductions[grammar.start, default: []].map({ (production) -> ParseStateItem in
            ParseStateItem(production: production, productionPosition: 0, startTokenIndex: 0)
        }).collect(Set.init).collect { initState in
            processState(productions: nonTerminalProductions, allStates: [], knownItems: initState, newItems: initState)
        }
        
        var tokenization: [[(terminal: Terminal, range: Range<String.Index>)]] = []
        tokenization.reserveCapacity(string.count)
        
        var stateCollection: [Set<ParseStateItem>] = [initState]
        stateCollection.reserveCapacity(string.count + 1)
        
        var currentIndex = string.startIndex
        
        // Tokenize string while parsing it
        while currentIndex < string.endIndex {
            let lastState = stateCollection.last!
            
            // Collect all terminals which could occur at the current location according to the grammar
            let expectedTerminals = Dictionary(
                grouping: lastState.compactMap { item -> (item: ParseStateItem, terminal: Terminal)? in
                    guard case .some(.terminal(let terminal)) = item.nextSymbol else {
                        return nil
                    }
                    return (item: item, terminal: terminal)
                },
                by: { pair in
                    pair.terminal
                }
            ).mapValues { pairs in
                pairs.map { pair in
                    pair.item
                }
            }
            
            // Find the tokens which match the string
            let (newItems, tokens): ([[ParseStateItem]], [(terminal: Terminal, range: Range<String.Index>)]) =
                expectedTerminals.compactMap { (terminal, items) -> ([ParseStateItem], (terminal: Terminal, range: Range<String.Index>))? in
                    guard let range = string.rangeOfPrefix(terminal, from: currentIndex), range.lowerBound == currentIndex else {
                        return nil
                    }
                    return (items.map{$0.advanced()}, (terminal, range))
                }.collect(unzip)
            
            // Check if tokens have been found. Report a syntax error if none have been found
            guard !newItems.isEmpty else {
                // Find non terminals which are expected at the current location
                let context = lastState.compactMap { item -> NonTerminal? in
                    switch item.nextSymbol {
                    case .none:
                        return nil
                    case .some(.terminal):
                        return nil
                    case .some(.nonTerminal(let nonTerminal)):
                        return nonTerminal
                    }
                }.filter { nonTerminal -> Bool in
                    nonTerminalProductions[nonTerminal, default: []].contains(where: { production -> Bool in
                        if case .some(.terminal(_)) = production.production.first {
                            return true
                        } else {
                            return false
                        }
                    })
                }
                throw SyntaxError(
                    range: currentIndex ..< string.index(after: currentIndex),
                    in: string,
                    reason: context.isEmpty ? .unexpectedToken : .unmatchedPattern,
                    context: context
                )
            }
            
            let newItemSet = newItems.flatMap{$0}.collect(Set.init)
            
            tokenization.append(tokens)
            stateCollection.append(
                processState(
                    productions: nonTerminalProductions,
                    allStates: stateCollection,
                    knownItems: newItemSet,
                    newItems: newItemSet
                )
            )
            
            currentIndex = tokens.first!.range.upperBound
        }
        
        // Find all successfully parsed Earley items
        let parseStates = stateCollection.enumerated().reduce(Array<Set<ParsedItem>>(repeating: [], count: stateCollection.count)) { (parseStates, element) in
            let (index, state) = element
            let completed = state.filter {$0.isCompleted}
            return completed.reduce(into: parseStates) { (parseStates, item) in
                parseStates[item.startTokenIndex].insert(ParsedItem(production: item.production, completedIndex: index))
            }
        }
        
        return (parseStates, tokenization)
    }
    
    /// Creates a syntax tree which explains how a word was derived from a grammar
    ///
    /// - Parameter string: Input word, for which a parse tree should be generated
    /// - Returns: A syntax tree explaining how the grammar can be used to derive the word described by the given tokenization
    /// - Throws: A syntax error if the word is not in the language recognized by the parser
    public func syntaxTree(for string: String) throws -> ParseTree {
        let (parseStates, tokenization) = try parse(string)
        
        guard let match = parseStates.first!.first(where: { item -> Bool in
            item.completedIndex == parseStates.count - 1 &&
                item.production.pattern == grammar.start
        }) else {
            let startItems = parseStates[0].filter { item in
                item.production.pattern == grammar.start
            }
            if let longestMatch = startItems.max(by: {$0.completedIndex < $1.completedIndex}) {
                let range = tokenization[longestMatch.completedIndex].first!.range
                throw SyntaxError(range: range, in: string, reason: .unmatchedPattern)
            } else {
                throw SyntaxError(range: tokenization.first?.first?.range ?? (string.startIndex ..< string.endIndex), in: string, reason: .unmatchedPattern)
            }
        }
        
        return buildSyntaxTrees(stateCollection: parseStates, tokenization: tokenization, rootItem: match, startIndex: 0, ignoreAmbiguousItems: true)[0].explode(grammar.utilityNonTerminals.contains)[0]
    }
    
    /// Generates all syntax trees explaining how a word can be derived from a grammar.
    ///
    /// This function should only be used for ambiguous grammars and if it is necessary to
    /// retrieve all parse trees, as it comes with an additional cost in runtime.
    ///
    /// For unambiguous grammars, this function should return the same results as `syntaxTree(for:)`.
    ///
    /// - Parameter string: Input word, for which all parse trees should be generated
    /// - Returns: All syntax trees which explain how the input was derived from the recognized grammar
    /// - Throws: A syntax error if the word is not in the language recognized by the parser
    public func allSyntaxTrees(for string: String) throws -> [ParseTree] {
        let (parseStates, tokenization) = try parse(string)
        let matches = parseStates.first!.filter { item -> Bool in
            item.completedIndex == parseStates.count - 1 &&
                item.production.pattern == grammar.start
        }
        guard !matches.isEmpty else {
            let startItems = parseStates[0].filter { item in
                item.production.pattern == grammar.start
            }
            if let longestMatch = startItems.max(by: {$0.completedIndex < $1.completedIndex}) {
                let range = tokenization[longestMatch.completedIndex].first!.range
                throw SyntaxError(range: range, in: string, reason: .unmatchedPattern)
            } else {
                throw SyntaxError(range: tokenization.first?.first?.range ?? (string.startIndex ..< string.endIndex), in: string, reason: .unmatchedPattern)
            }
        }
        return matches.flatMap { match in
            buildSyntaxTrees(stateCollection: parseStates, tokenization: tokenization, rootItem: match, startIndex: 0, ignoreAmbiguousItems: false).map {
                $0.explode(grammar.utilityNonTerminals.contains)[0]
            }
        }
    }
}
//
//  Symbols.swift
//  Covfefe
//
//  Created by Palle Klewitz on 07.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

/// A non-terminal symbol, which cannot occurr in a word recognized by a parser
public struct NonTerminal: Codable, Hashable {
    
    public static func ==(lhs: NonTerminal, rhs: NonTerminal) -> Bool {
        return lhs.name == rhs.name
    }
    
    /// Name of the non-terminal
    public let name: String
    
    public let hashValue: Int
    
    public func hash(into hasher: inout Hasher) {
        hasher.combine(hashValue)
    }
    
    /// Creates a new non-terminal symbol with a given name
    ///
    /// - Parameter name: Name of the non-terminal symbol
    public init(name: String) {
        self.name = name
        self.hashValue = name.hashValue
    }
}

extension NonTerminal: CustomStringConvertible {
    public var description: String {
        return name
    }
}

extension NonTerminal: ExpressibleByStringLiteral {
    public typealias StringLiteralType = String
    
    public init(stringLiteral value: String) {
        self.init(name: value)
    }
}

/// A symbol which can either be a terminal or a non-terminal character
///
/// - terminal: A terminal character
/// - nonTerminal: A non-terminal character
public enum Symbol: Codable {
    /// A terminal symbol
    case terminal(Terminal)
    
    /// A non-terminal symbol
    case nonTerminal(NonTerminal)
    
    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        
        if container.allKeys.contains(CodingKeys.terminal) {
            self = try .terminal(container.decode(Terminal.self, forKey: CodingKeys.terminal))
        } else {
            self = try .nonTerminal(container.decode(NonTerminal.self, forKey: CodingKeys.nonTerminal))
        }
    }
    
    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        
        switch self {
        case .terminal(let terminal):
            try container.encode(terminal, forKey: .terminal)
            
        case .nonTerminal(let nonTerminal):
            try container.encode(nonTerminal, forKey: .nonTerminal)
        }
    }
    
    private enum CodingKeys: String, CodingKey {
        case terminal
        case nonTerminal = "non_terminal"
    }
}

/// Creates a new non-regular terminal symbol
///
/// **Note**: The value of the terminal string may not overlap partially with any other non-terminal
/// contained in a grammar.
///
/// - Parameter value: Value of the terminal symbol
/// - Returns: A terminal symbol with the given value
public func t(_ value: String) -> Symbol {
    return Symbol.terminal(Terminal(string: value))
}

/// Creates a new non-terminal symbol
///
/// - Parameter name: Name of the non-terminal symbol
/// - Returns: A non-terminal symbol with the given name
public func n(_ name: String) -> Symbol {
    return Symbol.nonTerminal(NonTerminal(name: name))
}

/// Creates a new regular terminal symbol
///
/// **Note**: The value of the terminal string may not overlap partially with any other non-terminal
/// contained in a grammar. For regular terminals, it it may be deriable to add word boundary markers: `\b`.
///
/// - Parameter value: Regular value of the terminal
/// - Returns: A regular terminal symbol
/// - Throws: An error indicating that the given regular expression is invalid
public func rt(_ value: String) throws -> Symbol {
    return try Symbol.terminal(Terminal(expression: value))
}

extension Symbol: Hashable {
    public var hashValue: Int {
        switch self {
        case .terminal(let t):
            return t.hashValue
            
        case .nonTerminal(let n):
            return n.hashValue
        }
    }
    
    public func hash(into hasher: inout Hasher) {
        switch self {
        case .terminal(let t):
            hasher.combine(t.hashValue)
            
        case .nonTerminal(let n):
            hasher.combine(n.hashValue)
        }
    }
    
    public static func == (lhs: Symbol, rhs: Symbol) -> Bool {
        switch (lhs, rhs) {
        case (.terminal(let l), .terminal(let r)):
            return l == r
            
        case (.nonTerminal(let l), .nonTerminal(let r)):
            return l == r
            
        default:
            return false
        }
    }
}

extension Symbol: CustomStringConvertible {
    public var description: String {
        switch self {
        case .nonTerminal(let n):
            return n.name
            
        case .terminal(let t):
            return t.description
        }
    }
}

//
//  CharacterSets.swift
//  Covfefe
//
//  Created by Palle Klewitz on 07.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

/// A set of terminal or non-terminal symbols
public struct SymbolSet {
    
    /// Whitespace characters (space, tab and line break)
    public static let whitespace = ProductionResult(SymbolSet(" \t\n".map(String.init).map(t)))
    
    /// Lower case letters a to z
    public static let lowercase = ProductionResult(SymbolSet("abcdefghijklmnopqrstuvwxyz".map(String.init).map(t)))
    
    /// Upper case letters A to Z
    public static let uppercase = ProductionResult(SymbolSet("ABCDEFGHIJKLMNOPQRSTUVWXYZ".map(String.init).map(t)))
    
    /// Decimal digits 0 to 9
    public static let numbers = ProductionResult(SymbolSet((0...9).map(String.init).map(t)))
    
    /// Lower and upper case letters a to z and A to Z
    public static var letters: ProductionResult {
        return lowercase <|> uppercase
    }
    
    /// Alphanumeric characters (Letters and numbers)
    public static var alphanumerics: ProductionResult {
        return letters <|> numbers
    }
    
    /// Symbols contained in this symbol set
    public let symbols: [Symbol]
    
    /// Creates a new symbol set given a sequence of symbols
    ///
    /// - Parameter sequence: Sequence of symbols which the symbol set should contain
    public init<S: Sequence>(_ sequence: S) where S.Element == Symbol {
        self.symbols = Array(sequence)
    }
}

/// A string of symbols which can be used in a production of a grammar
public struct ProductionString {
    
    /// Symbols of this production string
    public var characters: [Symbol]
    
    /// Creates a new production string
    ///
    /// - Parameter characters: Symbols of the production string
    public init(_ characters: [Symbol]) {
        self.characters = characters
    }
}

/// A string of non-terminal symbols
public struct NonTerminalString {
    
    /// The non-terminal characters of this string
    public var characters: [NonTerminal]
}

extension NonTerminalString: Hashable {
    public var hashValue: Int {
        return characters.enumerated().reduce(0, { partialHash, element in
            return partialHash ^ ((element.element.hashValue >> (element.offset % 64)) | (element.element.hashValue << (64 - (element.offset % 64))))
        })
    }
    
    public func hash(into hasher: inout Hasher) {
        hasher.combine(hashValue)
    }
    
    public static func ==(lhs: NonTerminalString, rhs: NonTerminalString) -> Bool {
        return lhs.characters == rhs.characters
    }
}

extension ProductionString: ExpressibleByArrayLiteral {
    public typealias ArrayLiteralElement = Symbol
    
    public init(arrayLiteral elements: Symbol...) {
        self.init(elements)
    }
}

/// A production result contains multiple possible production strings
/// which can all be generated from a given non-terminal.
///
/// A production result can be used when creating a production rule with different possible productions:
///
///        "A" ~~> n("B") <|> t("x")
///                      ^ generates a production result
public struct ProductionResult {
    
    /// The possible production strings of this result
    public var elements: [ProductionString]
    
    
    /// Creates a new production result.
    ///
    /// - Parameter symbols: Possible strings which can be produced
    public init(symbols: [ProductionString]) {
        self.elements = symbols
    }
}

extension ProductionResult: ExpressibleByArrayLiteral {
    public typealias ArrayLiteralElement = ProductionString
    
    public init(arrayLiteral elements: ProductionString...) {
        self.init(symbols: elements)
    }
}

public extension ProductionResult {
    
    /// Creates a new production result from a symbol set where every symbol generates a different result independent of other symbols
    ///
    /// - Parameter set: The symbol set to create a production result from
    init(_ set: SymbolSet) {
        self.elements = set.symbols.map{ProductionString([$0])}
    }
}

precedencegroup ConcatenationPrecendence {
    associativity: left
    higherThan: AlternativePrecedence
    lowerThan: AdditionPrecedence
}

precedencegroup AlternativePrecedence {
    associativity: left
    higherThan: ProductionPrecedence
}

infix operator <+> : ConcatenationPrecendence
infix operator <|> : AlternativePrecedence

/// Concatenates two production strings
///
/// - Parameters:
///   - lhs: First production string
///   - rhs: Second production string
/// - Returns: Concatenation of the given production strings
public func <+> (lhs: ProductionString, rhs: ProductionString) -> ProductionString {
    return ProductionString(lhs.characters + rhs.characters)
}

/// Concatenates a production string and a symbol
///
/// - Parameters:
///   - lhs: A production string
///   - rhs: A symbol
/// - Returns: Concatenation of the production string and symbol
public func <+> (lhs: ProductionString, rhs: Symbol) -> ProductionString {
    return ProductionString(lhs.characters + [rhs])
}

/// Concatenates a production string and a symbol
///
/// - Parameters:
///   - lhs: A symbol
///   - rhs: A production string
/// - Returns: Concatenation of the production string and symbol
public func <+> (lhs: Symbol, rhs: ProductionString) -> ProductionString {
    return ProductionString([lhs] + rhs.characters)
}

/// Concatenates two production symbols into a production string
///
/// - Parameters:
///   - lhs: First symbol
///   - rhs: Second symbol
/// - Returns: Concatenation of the given symbols
public func <+> (lhs: Symbol, rhs: Symbol) -> ProductionString {
    return ProductionString([lhs, rhs])
}

/// Concatenates every possible production of the first production result with
/// every possible production of the second production result
///
/// - Parameters:
///   - lhs: First production result
///   - rhs: Second production result
/// - Returns: Every possible concatenation of the production strings in the given production results
public func <+> (lhs: ProductionResult, rhs: ProductionResult) -> ProductionResult {
    return ProductionResult(symbols: crossProduct(lhs.elements, rhs.elements).map(<+>))
}

/// Concatenates every production string of the production result with the given production string
///
/// - Parameters:
///   - lhs: Production result
///   - rhs: Production string
/// - Returns: Concatenation of every production string in the production result with the given production string
public func <+> (lhs: ProductionResult, rhs: ProductionString) -> ProductionResult {
    return ProductionResult(symbols: lhs.elements.map{$0 <+> rhs})
}

/// Concatenates the given production string with every production string of the production result
///
/// - Parameters:
///   - lhs: Production string
///   - rhs: Production result
/// - Returns: Concatenation of the given production string with every production string in the production result
public func <+> (lhs: ProductionString, rhs: ProductionResult) -> ProductionResult {
    return ProductionResult(symbols: rhs.elements.map{lhs <+> $0})
}

/// Generates a production result containing every production string of the given production results
///
/// - Parameters:
///   - lhs: First production result
///   - rhs: Second production result
/// - Returns: Joined production result of the given production results
public func <|> (lhs: ProductionResult, rhs: ProductionResult) -> ProductionResult {
    return ProductionResult(symbols: lhs.elements + rhs.elements)
}

/// Generates a production result containing every production of the left production result and the production string
///
/// - Parameters:
///   - lhs: Production result
///   - rhs: Production string
/// - Returns: A production result generated by merging the left production result with the right production string
public func <|> (lhs: ProductionResult, rhs: ProductionString) -> ProductionResult {
    return ProductionResult(symbols: lhs.elements + [rhs])
}

/// Generates a production result containing the left production string and every production of the right production string
///
/// - Parameters:
///   - lhs: Production string
///   - rhs: Production result
/// - Returns: A production result generated by merging the left production string with the right production result
public func <|> (lhs: ProductionString, rhs: ProductionResult) -> ProductionResult {
    return ProductionResult(symbols: [lhs] + rhs.elements)
}

/// Generates a production result containing the left and right production string
///
/// - Parameters:
///   - lhs: First production string
///   - rhs: Second production string
/// - Returns: A production result containing the left and right production string
public func <|> (lhs: ProductionString, rhs: ProductionString) -> ProductionResult {
    return ProductionResult(symbols: [lhs, rhs])
}

/// Generates a production result containing the left production string and the right symbol
///
/// - Parameters:
///   - lhs: Production string
///   - rhs: Symbol
/// - Returns: A production result allowing the left production string and the right symbol
public func <|> (lhs: ProductionString, rhs: Symbol) -> ProductionResult {
    return ProductionResult(symbols: [lhs, [rhs]])
}

/// Generates a production result containing the left symbol and the right production string
///
/// - Parameters:
///   - lhs: Symbol
///   - rhs: Production string
/// - Returns: A production result allowing the left symbol and the right production string
public func <|> (lhs: Symbol, rhs: ProductionString) -> ProductionResult {
    return ProductionResult(symbols: [[lhs], rhs])
}

/// Generates a production result by appending the right symbol to the left production result
///
/// - Parameters:
///   - lhs: Production result
///   - rhs: Symbol
/// - Returns: A production result allowing every production string of the left production result and the right symbol
public func <|> (lhs: ProductionResult, rhs: Symbol) -> ProductionResult {
    return ProductionResult(symbols: lhs.elements + [[rhs]])
}

/// Generates a production result by appending the left symbol to the right production result
///
/// - Parameters:
///   - lhs: Symbol
///   - rhs: Production result
/// - Returns: A production result allowing the left symbol and every production string of the right production result
public func <|> (lhs: Symbol, rhs: ProductionResult) -> ProductionResult {
    return ProductionResult(symbols: [[lhs]] + rhs.elements)
}

/// Generates a production result containing the left and right symbol
///
/// - Parameters:
///   - lhs: Left symbol
///   - rhs: Right symbol
/// - Returns: A production result allowing either the left or the right symbol
public func <|> (lhs: Symbol, rhs: Symbol) -> ProductionResult {
    return ProductionResult(symbols: [[lhs], [rhs]])
}
//
//  Normalization.swift
//  Covfefe
//
//  Created by Palle Klewitz on 11.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

extension Grammar {
    
    static func eliminateMixedProductions(productions: [Production]) -> [Production] {
        return productions.flatMap { production -> [Production] in
            // Determine, if the production is invalid and if not, return early
            let terminals = production.generatedTerminals
            let nonTerminals = production.generatedNonTerminals
            
            if terminals.isEmpty {
                return [production]
            }
            if nonTerminals.isEmpty && terminals.count == 1 {
                return [production]
            }
            
            // Find all terminals and their indices in the production
            let enumeratedTerminals = production.production.enumerated().compactMap { offset, element -> (Int, Terminal)? in
                guard case .terminal(let terminal) = element else {
                    return nil
                }
                return (offset, terminal)
            }
            
            // Generate new patterns which replace the terminals in the existing production
            let patterns = enumeratedTerminals.map { element -> NonTerminal in
                let (offset, terminal) = element
                return NonTerminal(name: "\(production.pattern.name)-\(String(terminal.hashValue % 65526, radix: 16, uppercase: false))-\(offset)")
            }
            
            // Update the existing production by replacing all terminals with the new patterns
            let updatedProductionElements = zip(enumeratedTerminals, patterns).map{($0.0, $0.1, $1)}.reduce(production.production) { productionElements, element -> [Symbol] in
                let (offset, _, nonTerminal) = element
                var updatedElements = productionElements
                updatedElements[offset] = .nonTerminal(nonTerminal)
                return updatedElements
            }
            
            let updatedProduction = Production(pattern: production.pattern, production: updatedProductionElements)
            
            // Generate new productions which produce the replaced terminals
            let newProductions = zip(enumeratedTerminals, patterns).map{($0.0, $0.1, $1)}.map { element -> Production in
                let (_, terminal, nonTerminal) = element
                return Production(pattern: nonTerminal, production: [.terminal(terminal)])
            }
            
            return newProductions + [updatedProduction]
        }
    }
    
    static func decomposeProductions(productions: [Production]) -> [Production] {
        return productions.flatMap { production -> [Production] in
            guard production.generatedNonTerminals.count >= 3 else {
                return [production]
            }
            let newProductions = production.generatedNonTerminals.dropLast().pairs().enumerated().map { element -> Production in
                let (offset, (nonTerminal, next)) = element
                return Production(
                    pattern: NonTerminal(name: "\(production.pattern.name)-\(nonTerminal.name)-\(offset)"),
                    production: [.nonTerminal(nonTerminal), n("\(production.pattern.name)-\(next.name)-\(offset + 1)")]
                )
            }
            
            let lastProduction = Production(
                pattern: NonTerminal(name: "\(production.pattern.name)-\(production.generatedNonTerminals.dropLast().last!.name)-\(production.generatedNonTerminals.count-2)"),
                production: production.generatedNonTerminals.suffix(2).map{.nonTerminal($0)}
            )
            
            let firstProduction = Production(pattern: production.pattern, production: newProductions[0].production)
            let middleProductions = newProductions.dropFirst().collect(Array.init)
            return [firstProduction] + middleProductions + [lastProduction]
        }
    }
    
    static func eliminateEmpty(productions: [Production], start: NonTerminal) -> [Production] {
        let groupedProductions = Dictionary(grouping: productions, by: {$0.pattern})
        
        func generatesEmpty(_ nonTerminal: NonTerminal, path: Set<NonTerminal>) -> Bool {
            if path.contains(nonTerminal) {
                return false
            }
            
            let directProductions = groupedProductions[nonTerminal, default: []]
            return directProductions.contains { production -> Bool in
                if production.production.isEmpty {
                    return true
                }
                return production.generatedNonTerminals.count == production.production.count
                    && production.generatedNonTerminals.allSatisfy { pattern -> Bool in
                        generatesEmpty(pattern, path: path.union([nonTerminal]))
                    }
            }
        }
        
        func generatesNonEmpty(_ nonTerminal: NonTerminal, path: Set<NonTerminal>) -> Bool {
            if path.contains(nonTerminal) {
                return false
            }
            
            let directProductions = groupedProductions[nonTerminal, default: []]
            return directProductions.contains { production -> Bool in
                if !production.generatedTerminals.isEmpty {
                    return true
                }
                return production.generatedNonTerminals.contains { pattern -> Bool in
                    generatesNonEmpty(pattern, path: path.union([nonTerminal]))
                }
            }
        }
        
        let result = Dictionary(uniqueKeysWithValues: groupedProductions.keys.map { key -> (NonTerminal, (generatesEmpty: Bool, generatesNonEmpty: Bool)) in
            (key, (generatesEmpty: generatesEmpty(key, path: []), generatesNonEmpty: generatesNonEmpty(key, path: [])))
        })
        
        let updatedProductions = productions.flatMap { production -> [Production] in
            if production.production.isEmpty && production.pattern != start {
                return []
            }
            if production.isFinal {
                return [production]
            }
            let produced = production.production.reduce([[]]) { (partialResult, symbol) -> [[Symbol]] in
                if case .nonTerminal(let nonTerminal) = symbol {
                    let (empty, nonEmpty) = result[nonTerminal] ?? (false, true)
                    
                    if !nonEmpty {
                        return partialResult
                    } else if !empty {
                        return partialResult.map {$0 + [symbol]}
                    } else {
                        return partialResult + partialResult.map {$0 + [symbol]}
                    }
                } else {
                    return partialResult.map {$0 + [symbol]}
                }
            }
            return produced.compactMap { sequence -> Production? in
                guard !sequence.isEmpty || production.pattern == start else {
                    return nil
                }
                return Production(pattern: production.pattern, production: sequence)
            }
        }
        return updatedProductions
    }
    
    static func eliminateChainProductions(productions: [Production]) -> [Production] {
        let nonTerminalProductions = Dictionary(grouping: productions, by: {$0.pattern})
        
        func findNonChainProduction(from start: Production, visited: Set<NonTerminal>, path: [NonTerminal]) -> [(Production, [NonTerminal])] {
            if start.isFinal || start.generatedNonTerminals.count != 1 {
                return [(start, path)]
            } else if visited.contains(start.pattern) {
                return []
            }
            
            let nonTerminal = start.generatedNonTerminals[0]
            let reachableProductions = nonTerminalProductions[nonTerminal] ?? []
            
            return reachableProductions.flatMap{findNonChainProduction(from: $0, visited: visited.union([start.pattern]), path: path + [nonTerminal])}
        }
        
        return productions.flatMap { production -> [Production] in
            let nonChainProductions = findNonChainProduction(from: production, visited: [], path: [])
            return nonChainProductions.map { element -> Production in
                let (p, chain) = element
                return Production(pattern: production.pattern, production: p.production, chain: chain)
            }
        }
    }
    
    static func eliminateUnusedProductions(productions: [Production], start: NonTerminal) -> [Production] {
        let nonTerminalProductions = Dictionary(grouping: productions, by: {$0.pattern})
        
        func mark(nonTerminal: NonTerminal, visited: Set<NonTerminal>) -> Set<NonTerminal> {
            if visited.contains(nonTerminal) {
                return visited
            }
            
            let newVisited = visited.union([nonTerminal])
            let reachableProductions = nonTerminalProductions[nonTerminal] ?? []
            return reachableProductions.reduce(newVisited) { partialVisited, production -> Set<NonTerminal> in
                production.generatedNonTerminals.reduce(partialVisited) { partial, n -> Set<NonTerminal> in
                    mark(nonTerminal: n, visited: partial)
                }
            }
        }
        
        let reachableNonTerminals = mark(nonTerminal: start, visited: [])
        
        return productions.filter { production -> Bool in
            reachableNonTerminals.contains(production.pattern)
        }
    }
    
    /// Generates a context free grammar equal to the current grammar which is in Chomsky Normal Form.
    /// The grammar is converted by decomposing non-terminal productions of lengths greater than 2,
    /// introducing new non-terminals to replace terminals in mixed productions, and removing empty productions.
    ///
    /// Chomsky normal form is required for some parsers (like the CYK parser) to work.
    ///
    /// In chomsky normal form, all productions must have the following form:
    ///
    ///     A -> B C
    ///     D -> x
    ///     Start -> empty
    ///
    /// Note that empty productions are only allowed starting from the start non-terminal
    ///
    /// - Returns: Chomsky normal form of the current grammar
    public func chomskyNormalized() -> Grammar {
        // Generate weak Chomsky Normal Form by eliminating all productions generating a pattern of nonTerminals mixed with terminals
        let nonMixedProductions = Grammar.eliminateMixedProductions(productions: productions)
        
        // Decompose all productions with three or more nonTerminals
        let decomposedProductions = Grammar.decomposeProductions(productions: nonMixedProductions)
        
        // Remove empty productions
        let nonEmptyProductions = Grammar.eliminateEmpty(productions: decomposedProductions, start: start)
        
        // Remove chains
        let nonChainedProductions = Grammar.eliminateChainProductions(productions: nonEmptyProductions)
        
        // Remove duplicates
        let uniqueProductions = nonChainedProductions.uniqueElements().collect(Array.init)
        
        // Remove unreachable productions
        let reachableProductions = Grammar.eliminateUnusedProductions(productions: uniqueProductions, start: start)
        //let reachableProductions = uniqueProductions
        
        let initialNonTerminals = productions.flatMap{[$0.pattern] + $0.generatedNonTerminals}.collect(Set.init)
        let generatedNonTerminals = reachableProductions.flatMap{[$0.pattern] + $0.generatedNonTerminals}.collect(Set.init)
        let newNonTerminals = generatedNonTerminals.subtracting(initialNonTerminals)
        
        return Grammar(productions: reachableProductions, start: start, utilityNonTerminals: self.utilityNonTerminals.union(newNonTerminals))
    }
}
//
//  EBNFImporter.swift
//  Covfefe
//
//  Created by Palle Klewitz on 14.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

/// A grammar describing the Backus-Naur form
var ebnfGrammar: Grammar {
    
    let syntax = "syntax" ~~> n("optional-whitespace") <|> n("newlines") <|> n("rule") <|> n("rule") <+> n("newlines") <|> n("syntax") <+> n("newlines") <+> n("rule") <+> (n("newlines") <|> [[]])
    let rule = "rule" ~~> n("optional-whitespace") <+> n("rule-name-container") <+> n("optional-whitespace") <+> n("assignment-operator") <+> n("optional-whitespace") <+> n("expression") <+> n("optional-whitespace") <+> t(";") <+> n("optional-whitespace")
    
    let optionalWhitespace = "optional-whitespace" ~~> [[]] <|> n("whitespace") <+> [n("optional-whitespace")]
    let whitespace = "whitespace" ~~> SymbolSet.whitespace <|> n("comment")
    let newlines = "newlines" ~~> t("\n") <|> t("\n") <+> n("optional-whitespace") <+> n("newlines")
    
    let comment = "comment" ~~> t("(") <+> t("*") <+> n("comment-content") <+> t("*") <+> t(")") <|> t("(") <+> t("*") <+> t("*") <+> t("*") <+> t(")")
    let commentContent = "comment-content" ~~> n("comment-content") <+> n("comment-content-char") <|> [[]]
    // a comment cannot contain a * followed by a ) or a ( followed by a *
    let commentContentChar = try! "comment-content-char" ~~> rt("[^*(]") <|> n("comment-asterisk") <+> rt("[^)]") <|> n("comment-open-parenthesis") <+> rt("[^*]") <|> n("comment")
    let commentAsterisk = "comment-asterisk" ~~> n("comment-asterisk") <+> t("*") <|> t("*")
    let commentOpenParenthesis = "comment-open-parenthesis" ~~> n("comment-open-parenthesis") <+> t("(") <|> t("(")
    
    let assignmentOperator = "assignment-operator" ~~> t("=")
    
    let ruleNameContainer = "rule-name-container" ~~> n("delimiting-rule-name-char") <+> n("rule-name") <+> n("delimiting-rule-name-char") <|> n("delimiting-rule-name-char")
    let ruleName = "rule-name" ~~> n("rule-name") <+> n("rule-name-char") <|> [[]]
    let ruleNameChar = "rule-name-char" ~~> n("delimiting-rule-name-char") <|> n("whitespace")
    let delimitingRuleNameChar = try! "delimiting-rule-name-char" ~~> rt("[a-zA-Z0-9-_]")
    
    let expression = "expression" ~~> n("concatenation") <|> n("alternation")
    let alternation = "alternation" ~~> n("expression") <+> n("optional-whitespace") <+> t("|") <+> n("optional-whitespace") <+> n("concatenation")
    let concatenation = "concatenation" ~~> n("expression-element") <|> n("concatenation") <+> n("optional-whitespace") <+> t(",") <+> n("optional-whitespace") <+> n("expression-element")
    let expressionElement = "expression-element" ~~> n("literal") <|> n("rule-name-container") <|> n("expression-group") <|> n("expression-repetition") <|> n("expression-optional") <|> n("expression-multiply")
    
    let expressionGroup = "expression-group" ~~> t("(") <+> n("optional-whitespace") <+> n("expression") <+> n("optional-whitespace") <+> t(")")
    let expressionRepetition = "expression-repetition" ~~> t("{") <+> n("optional-whitespace") <+> n("expression") <+> n("optional-whitespace") <+> t("}")
    let expressionOptional = "expression-optional" ~~> t("[") <+> n("optional-whitespace") <+> n("expression") <+> n("optional-whitespace") <+> t("]")
    let expressionMultiply = "expression-multiply" ~~> n("number") <+> n("optional-whitespace") <+> t("*") <+> n("optional-whitespace") <+> n("expression-element")
    
    let literal = "literal" ~~> t("'") <+> n("string-1") <+> t("'") <|> t("\"") <+> n("string-2") <+> t("\"") <|> n("range-literal")
    let string1 = "string-1" ~~> n("string-1") <+> n("string-1-char") <|> [[]]
    let string2 = "string-2" ~~> n("string-2") <+> n("string-2-char") <|> [[]]
    
    let rangeLiteral = "range-literal" ~~> n("single-char-literal") <+> n("optional-whitespace") <+> t(".") <+> t(".") <+> t(".") <+> n("optional-whitespace") <+> n("single-char-literal")
    let singleCharLiteral = "single-char-literal" ~~> t("'") <+> n("string-1-char") <+> t("'") <|> t("\"") <+> n("string-2-char") <+> t("\"")
    
    // no ', \, \r or \n
    let string1char = try! "string-1-char" ~~> rt("[^'\\\\\r\n]") <|> n("string-escaped-char") <|> n("escaped-single-quote")
    let string2char = try! "string-2-char" ~~> rt("[^\"\\\\\r\n]") <|> n("string-escaped-char") <|> n("escaped-double-quote")
    
    let digit = try! "digit" ~~> rt("[0-9]")
     let number = "number" ~~> n("digit") <|> n("number") <+> n("digit")
    
    let stringEscapedChar = "string-escaped-char" ~~> n("unicode-scalar") <|> n("carriage-return") <|> n("line-feed") <|> n("tab-char") <|> n("backslash")
    let unicodeScalar = "unicode-scalar" ~~> t("\\") <+> t("u") <+> t("{") <+>  n("unicode-scalar-digits") <+> t("}")
    let unicodeScalarDigits = "unicode-scalar-digits" ~~> [n("hex-digit")] <+> (n("hex-digit") <|> [[]]) <+> (n("hex-digit") <|> [[]]) <+> (n("hex-digit") <|> [[]]) <+> (n("hex-digit") <|> [[]]) <+> (n("hex-digit") <|> [[]]) <+> (n("hex-digit") <|> [[]]) <+> (n("hex-digit") <|> [[]])
    let hexDigit = try! "hex-digit" ~~> rt("[0-9a-fA-F]")
    
    let carriageReturn = "carriage-return" ~~> t("\\") <+> t("r")
    let lineFeed = "line-feed" ~~> t("\\") <+> t("n")
    let tabChar = "tab-char" ~~> t("\\") <+> t("t")
    let backslash = "backslash" ~~> t("\\") <+> t("\\")
    let singleQuote = "escaped-single-quote" ~~> t("\\") <+> t("'")
    let doubleQuote = "escaped-double-quote" ~~> t("\\") <+> t("\"")
    
    var productions: [Production] = []
    productions.append(contentsOf: syntax)
    productions.append(rule)
    productions.append(contentsOf: optionalWhitespace)
    productions.append(contentsOf: whitespace)
    productions.append(contentsOf: comment)
    productions.append(contentsOf: commentContent)
    productions.append(contentsOf: commentContentChar)
    productions.append(contentsOf: commentAsterisk)
    productions.append(contentsOf: commentOpenParenthesis)
    productions.append(contentsOf: newlines)
    productions.append(assignmentOperator)
    productions.append(contentsOf: ruleNameContainer)
    productions.append(contentsOf: ruleName)
    productions.append(contentsOf: ruleNameChar)
    productions.append(delimitingRuleNameChar)
    productions.append(contentsOf: expression)
    productions.append(alternation)
    productions.append(contentsOf: concatenation)
    productions.append(contentsOf: expressionElement)
    productions.append(expressionGroup)
    productions.append(expressionRepetition)
    productions.append(expressionOptional)
    productions.append(expressionMultiply)
    productions.append(contentsOf: literal)
    productions.append(contentsOf: string1)
    productions.append(contentsOf: string2)
    productions.append(contentsOf: string1char)
    productions.append(contentsOf: string2char)
    productions.append(digit)
    productions.append(contentsOf: number)
    productions.append(rangeLiteral)
    productions.append(contentsOf: singleCharLiteral)
    productions.append(contentsOf: stringEscapedChar)
    productions.append(unicodeScalar)
    productions.append(contentsOf: unicodeScalarDigits)
    productions.append(hexDigit)
    productions.append(carriageReturn)
    productions.append(lineFeed)
    productions.append(tabChar)
    productions.append(backslash)
    productions.append(singleQuote)
    productions.append(doubleQuote)
    
    return Grammar(productions: productions, start: "syntax")
}

public extension Grammar {
    
    /// Creates a new grammar from a specification in Extended Backus-Naur Form (EBNF)
    ///
    ///     pattern1 = alternative1 | alternative2;
    ///        pattern2 = 'con', 'catenation';
    ///
    /// - Parameters:
    ///   - bnfString: String describing the grammar in EBNF
    ///   - start: Start non-terminal
    init(ebnf ebnfString: String, start: String) throws {
        let grammar = ebnfGrammar
        let parser = EarleyParser(grammar: grammar)
        let syntaxTree = try parser
            .syntaxTree(for: ebnfString)
            .explode{["expression"].contains($0)}
            .first!
            .filter{!["optional-whitespace", "newlines"].contains($0)}!
        
        let ruleDeclarations = syntaxTree.allNodes(where: {$0.name == "rule"})
        
        func ruleName(from container: SyntaxTree<NonTerminal, Range<String.Index>>) -> String {
            return container.leafs
                .reduce("") { partialResult, range -> String in
                    partialResult.appending(ebnfString[range])
            }
        }
        
        func character(fromCharacterExpression characterExpression: ParseTree) throws -> Character {
            guard let child = characterExpression.children?.first else {
                fatalError()
            }
            switch child {
            case .leaf(let range):
                return ebnfString[range.lowerBound]
                
            case .node(key: "string-escaped-char", children: let children):
                guard let child = children.first else {
                    fatalError()
                }
                switch child {
                case .leaf:
                    fatalError()
                    
                case .node(key: "unicode-scalar", children: let children):
                    let hexString: String = children.dropFirst(3).dropLast().flatMap {$0.leafs}.map {ebnfString[$0]}.joined()
                    // Grammar guarantees that hexString is always a valid hex integer literal
                    let charValue = Int(hexString, radix: 16)!
                    guard let scalar = UnicodeScalar(charValue) else {
                        throw LiteralParsingError.invalidUnicodeScalar(charValue)
                    }
                    return Character(scalar)
                    
                case .node(key: "carriage-return", children: _):
                    return "\r"
                    
                case .node(key: "line-feed", children: _):
                    return "\n"
                    
                case .node(key: "tab-char", children: _):
                    return "\t"
                    
                case .node(key: "backslash", children: _):
                    return "\\"
                    
                default:
                    fatalError()
                }
                
            case .node(key: "escaped-single-quote", children: _):
                return "'"
                
            case .node(key: "escaped-double-quote", children: _):
                return "\""
                
            default:
                fatalError()
            }
        }
        
        func string(fromStringExpression stringExpression: ParseTree, knownString: String = "") throws -> String {
            if let children = stringExpression.children, children.count == 2 {
                let char = try character(fromCharacterExpression: children[1])
                return try string(fromStringExpression: children[0], knownString: "\(char)\(knownString)")
            } else {
                return knownString
            }
        }
        
        func terminal(fromLiteral literal: ParseTree) throws -> Terminal {
            guard let children = literal.children else {
                fatalError()
            }
            if children.count == 3 {
                let stringNode = children[1]
                return try Terminal(string: string(fromStringExpression: stringNode))
            } else if children.count == 1 {
                let rangeExpression = children[0]
                guard rangeExpression.root == "range-literal" else {
                    fatalError()
                }
                guard let children = rangeExpression.children, children.count == 5 else {
                    fatalError()
                }
                let lowerBound = try character(fromCharacterExpression: children[0].children![1])
                let upperBound = try character(fromCharacterExpression: children[4].children![1])
                
                guard lowerBound <= upperBound else {
                    throw LiteralParsingError.invalidRange(lowerBound: lowerBound, upperBound: upperBound, description: "lowerBound must be less than or equal to upperBound")
                }
                
                return Terminal(range: lowerBound ... upperBound)
            }
            
            fatalError()
        }
        
        func makeProductions(from expression: SyntaxTree<NonTerminal, Range<String.Index>>, named name: String) throws -> (productions: [Production], additionalRules: [Production]) {
            guard let type = expression.root?.name else {
                return ([], [])
            }
            guard let children = expression.children else {
                return ([], [])
            }
            switch type {
            case "alternation":
                let (lhs, lhsAdd) = try makeProductions(from: children[0], named: "\(name)-a0")
                let (rhs, rhsAdd) = try makeProductions(from: children[2], named: "\(name)-a1")
                return ((lhs + rhs).map {Production(pattern: NonTerminal(name: name), production: $0.production)}, lhsAdd + rhsAdd)
                
            case "concatenation":
                if children.count == 3 {
                    let (lhsProductions, lhsAdd) = try makeProductions(from: children[0], named: "\(name)-c0")
                    let (rhsProductions, rhsAdd) = try makeProductions(from: children[2], named: "\(name)-c1")
                    
                    return (crossProduct(lhsProductions, rhsProductions).map { arg -> Production in
                        let (lhs, rhs) = arg
                        return Production(pattern: NonTerminal(name: name), production: lhs.production + rhs.production)
                    }, lhsAdd + rhsAdd)
                } else if children.count == 1 {
                    return try makeProductions(from: children[0], named: name)
                } else {
                    fatalError()
                }
                
            case "expression-element":
                guard children.count == 1 else {
                    return ([], [])
                }
                switch children[0].root!.name {
                case "literal":
                    let t = try terminal(fromLiteral: children[0])
                    if t.isEmpty {
                        return ([Production(pattern: NonTerminal(name: name), production: [])], [])
                    } else {
                        return ([Production(pattern: NonTerminal(name: name), production: [.terminal(t)])], [])
                    }
                    
                case "rule-name-container":
                    let nonTerminalName = ruleName(from: children[0])
                    return ([Production(pattern: NonTerminal(name: name), production: [n(nonTerminalName)])], [])
                    
                case "expression-group":
                    guard let group = children[0].children else {
                        fatalError()
                    }
                    assert(group.count == 3)
                    return try makeProductions(from: group[1], named: name)
                    
                case "expression-repetition":
                    guard let group = children[0].children else {
                        fatalError()
                    }
                    assert(group.count == 3)
                    let subruleName = "\(name)-r"
                    let (subRules, additionalRules) = try makeProductions(from: group[1], named: subruleName)
                    let repetitionRules = subRules.map { rule in
                        Production(pattern: NonTerminal(name: subruleName), production: [n(subruleName)] + rule.production)
                    }
                    return ([Production(pattern: NonTerminal(name: name), production: [n(subruleName)])], additionalRules + subRules + repetitionRules)
                    
                case "expression-optional":
                    guard let group = children[0].children else {
                        fatalError()
                    }
                    assert(group.count == 3)
                    let subruleName = "\(name)-o"
                    let (productions, additionalProductions) = try makeProductions(from: group[1], named: subruleName)
                    let optionalProductions = productions.map {
                        Production(pattern: $0.pattern, production: [])
                    }
                    let subproduction = Production(pattern: NonTerminal(name: name), production: [n(subruleName)])
                    return ([subproduction], additionalProductions + productions + optionalProductions)
                    
                case "expression-multiply":
                    guard let group = children[0].children else {
                        fatalError()
                    }
                    let multiplicityExpression = group[0].leafs
                    let multiplicityRange = multiplicityExpression.first!.lowerBound ..< multiplicityExpression.last!.upperBound
                    let multiplicity = Int(ebnfString[multiplicityRange])!
                    
                    let subruleName = "\(name)-m"
                    let (subrules, additionalExpressions) = try makeProductions(from: group[2], named: subruleName)
                    
                    let repeatedSubrules = repeatElement(subrules, count: multiplicity).reduce([]) { (acc, subrules) -> [Production] in
                        if acc.isEmpty {
                            return subrules.map { rule in
                                return Production(pattern: NonTerminal(name: name), production: rule.production)
                            }
                        } else {
                            return crossProduct(acc, subrules).map { arg in
                                let (lhs, rhs) = arg
                                return Production(pattern: NonTerminal(name: name), production: lhs.production + rhs.production)
                            }
                        }
                    }
                    return (repeatedSubrules, additionalExpressions)
                    
                default:
                    fatalError()
                }
                
            default:
                fatalError()
            }
        }
        
        let (productions, helperRules): ([Production], [Production]) = try ruleDeclarations.reduce(into: ([], [])) { acc, ruleDeclaration in
            guard let children = ruleDeclaration.children, children.count == 4 else {
                return
            }
            let name = ruleName(from: children[0])
            let (productions, additionalRules) = try makeProductions(from: children[2], named: name)
            acc.0.append(contentsOf: productions)
            acc.1.append(contentsOf: additionalRules)
        }
        
        if (productions + helperRules).contains(where: { (production: Production) -> Bool in
            production.generatedNonTerminals.contains("EOL")
        }) && !(productions + helperRules).contains(where: { (production: Production) -> Bool in
            production.pattern == "EOL"
        }) {
            self.init(productions: productions + helperRules + ["EOL" ~~> t("\n")], start: NonTerminal(name: start), utilityNonTerminals: helperRules.map {$0.pattern}.collect(Set.init))
        } else {
            self.init(productions: productions + helperRules, start: NonTerminal(name: start), utilityNonTerminals: helperRules.map {$0.pattern}.collect(Set.init))
        }
        
    }
}
//
//  File.swift
//
//
//  Created by Palle Klewitz on 23.05.20.
//  Copyright (c) 2020 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

let abnfGrammar: Grammar = {
    let abnfStr = """
    rule-list = [whitespace], [{rule, whitespace}], rule, [whitespace], [eof-comment] | [whitespace], [eof-comment];

    rule = init-rule | incremental-alternation;
    init-rule = [whitespace], nonterminal, [whitespace], '=', [whitespace], alternation;
    incremental-alternation = [whitespace], nonterminal, [whitespace], '=', '/', [whitespace], alternation;

    alternation = alternation, [whitespace], "/", [whitespace], concatenation | concatenation;
    concatenation = concatenation, whitespace, atom | atom;
    atom = optional | sequence-group | repetition | nonterminal | terminal;
    optional = "[", [whitespace], alternation, [whitespace], "]";
    sequence-group = "(", [whitespace], alternation, [whitespace], ")";
    repetition = variable-repetition | specific-repetition;
    variable-repetition = (closed-range | partial-range-to | partial-range-from | unlimited-range), repeated-atom;
    repeated-atom = optional | sequence-group | nonterminal | terminal;
    specific-repetition = integer-literal, repeated-atom;

    closed-range = integer-literal, '*', integer-literal;
    partial-range-to = '*', integer-literal;
    partial-range-from = integer-literal, '*';
    unlimited-range = '*';

    nonterminal = ("a" ... "z" | "A" ... "Z" | "_" | "-"), [{"a" ... "z" | "A" ... "Z" | "0" ... "9" | "_" | "-"}];
    terminal = string-literal | charcode-literal | charcode-range-literal;

    string-literal = '"', [{string-content}], '"';
    charcode-literal = hex-literal | dec-literal;
    charcode-range-literal = hex-range-literal | dec-range-literal;
    hex-literal = "%", "x", hex-int-seq;
    dec-literal = "%", "d", dec-int-seq;
    hex-range-literal = "%", "x", hex-int, "-", hex-int;
    dec-range-literal = "%", "d", dec-int, "-", dec-int;

    hex-int-seq = hex-int | hex-int-seq, ".", hex-int;
    dec-int-seq = dec-int | dec-int-seq, ".", dec-int;

    integer-literal = {digit};
    hex-int = {hex-digit};
    hex-digit = "0" ... "9" | "a" ... "f" | "A" ... "F";
    dec-int = {digit};
    digit = "0" ... "9";
    whitespace = {" " | "\\t" | "\\n" | "\\r", "\\n" | comment};
    comment = ";", [{any-except-linebreak}], "\\n";
    eof-comment = ";", [{any-except-linebreak}];
    string-content = "\\u{01}" ... "\\u{21}" | "\\u{23}" ... "\\u{FFFF}" | escape-sequence;
    any-except-linebreak = "\\u{01}" ... "\\u{09}" | "\\u{0B}" ... "\\u{0C}" | "\\u{0E}" ... "\\u{FFFF}";
    """
    
    return try! Grammar(ebnf: abnfStr, start: "rule-list")
}()


/// Errors specific to the import of ABNF grammars
public enum ABNFImportError: Error {
    /// The grammar contains a range expression with a lower bound higher than the upper bound
    case invalidRange(line: Int, column: Int)
    /// The grammar contains a charcode that is not a valid unicode scalar
    case invalidCharcode(line: Int, column: Int)
    /// The grammar contains a charcode range with a lower bound higher than the upper bound
    case invalidCharacterRange(line: Int, column: Int)
}

public extension Grammar {
    private static let coreRules: [Production] = [
        Production(pattern: NonTerminal(name: "ALPHA"), production: [.terminal(Terminal(range: "A" ... "Z"))]),
        Production(pattern: NonTerminal(name: "ALPHA"), production: [.terminal(Terminal(range: "a" ... "z"))]),
        Production(pattern: NonTerminal(name: "DIGIT"), production: [.terminal(Terminal(range: "0" ... "9"))]),
        Production(pattern: NonTerminal(name: "HEXDIG"), production: [.terminal(Terminal(range: "0" ... "9"))]),
        Production(pattern: NonTerminal(name: "HEXDIG"), production: [.terminal(Terminal(range: "a" ... "f"))]),
        Production(pattern: NonTerminal(name: "HEXDIG"), production: [.terminal(Terminal(range: "A" ... "F"))]),
        Production(pattern: NonTerminal(name: "DQUOTE"), production: [.terminal("\"")]),
        Production(pattern: NonTerminal(name: "SP"), production: [.terminal(" ")]),
        Production(pattern: NonTerminal(name: "HTAB"), production: [.terminal("\u{09}")]),
        Production(pattern: NonTerminal(name: "WSP"), production: [.nonTerminal("SP")]),
        Production(pattern: NonTerminal(name: "WSP"), production: [.nonTerminal("HTAB")]),
        Production(pattern: NonTerminal(name: "LWSP"), production: [.nonTerminal("WSP")]),
        Production(pattern: NonTerminal(name: "LWSP"), production: [.nonTerminal("CRLF"), .nonTerminal("WSP")]),
        Production(pattern: NonTerminal(name: "VCHAR"), production: [.terminal(Terminal(range: "\u{21}" ... "\u{7e}"))]),
        Production(pattern: NonTerminal(name: "CHAR"), production: [.terminal(Terminal(range: "\u{01}" ... "\u{7e}"))]),
        Production(pattern: NonTerminal(name: "OCTET"), production: [.terminal(Terminal(range: "\u{00}" ... "\u{ff}"))]),
        Production(pattern: NonTerminal(name: "CTL"), production: [.terminal(Terminal(range: "\u{00}" ... "\u{1f}"))]),
        Production(pattern: NonTerminal(name: "CTL"), production: [.terminal("\u{7f}")]),
        Production(pattern: NonTerminal(name: "CR"), production: [.terminal("\u{0d}")]),
        Production(pattern: NonTerminal(name: "LF"), production: [.terminal("\u{0a}")]),
        Production(pattern: NonTerminal(name: "CRLF"), production: [.nonTerminal("CR"), .nonTerminal("LF")]),
        Production(pattern: NonTerminal(name: "BIT"), production: [.terminal("0"), .terminal("1")]),
    ]
    
    
    /// Creates a grammar from the production rules defined in the provided ABNF grammar.
    /// - Parameters:
    ///   - abnf: ABNF grammar
    ///   - start: Starting symbol
    /// - Throws: Syntax error if the abnf string is not in ABNF format. ABNFImportError, when the grammar has semantic issues.
    init(abnf: String, start: String) throws {
        // Strip lines containing only comments
        let abnf = abnf
            .replacingOccurrences(of: "\n\\s*;[^\n]*\n", with: "\n\n", options: .regularExpression)
            .replacingOccurrences(of: "\n\\s*\n", with: "\n", options: .regularExpression)
        
        func flatten(parseTree: ParseTree, nodeName: String) -> [ParseTree] {
            switch parseTree {
            case .node(key: NonTerminal(name: nodeName), children: let children):
                return children.flatMap {
                    flatten(parseTree: $0, nodeName: nodeName)
                }
            default:
                return [parseTree]
            }
        }
        
        func parse(nonTerminal: ParseTree) -> NonTerminal {
            let nt = nonTerminal.leafs.map {abnf[$0]}.joined()
            return NonTerminal(name: nt)
        }
        
        func parse(stringLiteral: ParseTree) -> String {
            guard case .node(key: "string-literal", children: let children) = stringLiteral else {
                fatalError("Invalid parse tree")
            }
            return children.dropFirst().dropLast().flatMap {$0.leafs}.map {abnf[$0]}.joined()
        }
        
        func parse(hexInt: ParseTree) throws -> Character {
            let hexDigits = hexInt.leafs.map {abnf[$0]}.joined()
            
            guard let scalar = UInt32(hexDigits, radix: 16), let unichar = UnicodeScalar(scalar) else {
                let errorBound = hexInt.leafs[0].lowerBound
                throw ABNFImportError.invalidCharcode(
                    line: abnf[...errorBound].filter {$0.isNewline}.count,
                    column: abnf.distance(
                        from: abnf[...errorBound].lastIndex(where: {$0.isNewline}) ?? abnf.startIndex,
                        to: errorBound
                    )
                )
            }
            return Character(unichar)
        }
        
        func parse(decInt: ParseTree) throws -> Character {
            let hexDigits = decInt.leafs.map {abnf[$0]}.joined()
            
            guard let scalar = UInt32(hexDigits), let unichar = UnicodeScalar(scalar) else {
                let errorBound = decInt.leafs[0].lowerBound
                throw ABNFImportError.invalidCharcode(
                    line: abnf[...errorBound].filter {$0.isNewline}.count,
                    column: abnf.distance(
                        from: abnf[...errorBound].lastIndex(where: {$0.isNewline}) ?? abnf.startIndex,
                        to: errorBound
                    )
                )
            }
            return Character(unichar)
        }
        
        func parse(decimalIntegerSequence: ParseTree) throws -> String {
            let integers = decimalIntegerSequence.allNodes(where: {$0.name == "dec-int"})
            return try String(integers.map(parse(decInt:)))
        }
        
        func parse(hexIntegerSequence: ParseTree) throws -> String {
            let integers = hexIntegerSequence.allNodes(where: {$0.name == "hex-int"})
            return try String(integers.map(parse(hexInt:)))
        }
        
        func parse(charcodeLiteral: ParseTree) throws -> String {
            guard case .node(key: "charcode-literal", children: let children) = charcodeLiteral, children.count == 1, let firstChild = children.first else {
                fatalError("Invalid parse tree")
            }
            switch firstChild {
            case .node(key: "hex-literal", children: let hexLiteralChildren):
                return try parse(hexIntegerSequence: hexLiteralChildren[2])
                
            case .node(key: "dec-literal", children: let decLiteralChildren):
                return try parse(decimalIntegerSequence: decLiteralChildren[2])
                
            default:
                fatalError("Invalid parse tree")
            }
        }
        
        func parse(hexRangeLiteral: ParseTree) throws -> Terminal {
            guard case .node(key: "hex-range-literal", children: let children) = hexRangeLiteral, children.count == 5 else {
                fatalError("Invalid parse tree")
            }
            let lowerBound = try parse(hexInt: children[2])
            let upperBound = try parse(hexInt: children[4])
            
            if lowerBound > upperBound {
                let errorBound = hexRangeLiteral.leafs[0].lowerBound
                throw ABNFImportError.invalidCharacterRange(
                    line: abnf[...errorBound].filter {$0.isNewline}.count,
                    column: abnf.distance(
                        from: abnf[...errorBound].lastIndex(where: {$0.isNewline}) ?? abnf.startIndex,
                        to: errorBound
                    )
                )
            }
            
            return Terminal(range: lowerBound ... upperBound)
        }
        
        func parse(decimalRangeLiteral: ParseTree) throws -> Terminal {
            guard case .node(key: "dec-range-literal", children: let children) = decimalRangeLiteral, children.count == 5 else {
                fatalError("Invalid parse tree")
            }
            let lowerBound = try parse(decInt: children[2])
            let upperBound = try parse(decInt: children[4])
            
            if lowerBound > upperBound {
                let errorBound = decimalRangeLiteral.leafs[0].lowerBound
                throw ABNFImportError.invalidCharacterRange(
                    line: abnf[...errorBound].filter {$0.isNewline}.count,
                    column: abnf.distance(
                        from: abnf[...errorBound].lastIndex(where: {$0.isNewline}) ?? abnf.startIndex,
                        to: errorBound
                    )
                )
            }
            
            return Terminal(range: lowerBound ... upperBound)
        }
        
        func parse(charcodeRangeLiteral: ParseTree) throws -> Terminal {
            guard case .node(key: "charcode-range-literal", children: let children) = charcodeRangeLiteral, children.count == 1, let firstChild = children.first else {
                fatalError("Invalid parse tree")
            }
            switch firstChild {
            case .node(key: "hex-range-literal", children: _):
                return try parse(hexRangeLiteral: firstChild)
                
            case .node(key: "dec-range-literal", children: _):
                return try parse(decimalRangeLiteral: firstChild)
                
            default:
                fatalError("Invalid parse tree")
            }
        }
        
        func parse(terminal: ParseTree) throws -> Terminal {
            guard case .node(key: "terminal", children: let children) = terminal, children.count == 1, let firstChild = children.first else {
                fatalError("Invalid parse tree")
            }
            switch firstChild {
            case .node(key: "charcode-literal", children: _):
                return try Terminal(string: parse(charcodeLiteral: firstChild))
                
            case .node(key: "charcode-range-literal", children: _):
                return try parse(charcodeRangeLiteral: firstChild)
                
            case .node(key: "string-literal", children: _):
                return Terminal(string: parse(stringLiteral: firstChild))
                
            default:
                fatalError("Invalid parse tree")
            }
        }
        
        func parse(optional: ParseTree, alternationIndex: Int, concatenationIndex: Int, ruleName: String) throws -> ([Symbol], Set<Production>) {
            guard case .node(key: "optional", children: let children) = optional, children.count == 3 else {
                fatalError("Invalid parse tree")
            }
            let subRuleName = "\(ruleName)-a\(alternationIndex)-c\(concatenationIndex)"
            let (subAlternations, additionalRules) = try parse(alternation: children[1], ruleName: subRuleName)
            return (
                [.nonTerminal(NonTerminal(name: subRuleName))],
                additionalRules
                    .union(subAlternations.map {
                        Production(pattern: NonTerminal(name: subRuleName), production: $0)
                    })
                    .union([
                        Production(pattern: NonTerminal(name: subRuleName), production: [])
                    ])
            )
        }
        
        func parse(sequenceGroup: ParseTree, alternationIndex: Int, concatenationIndex: Int, ruleName: String) throws -> ([Symbol], Set<Production>) {
            guard case .node(key: "sequence-group", children: let children) = sequenceGroup, children.count == 3 else {
                fatalError("Invalid parse tree")
            }
            let subRuleName = "\(ruleName)-a\(alternationIndex)-c\(concatenationIndex)"
            let (subAlternations, additionalRules) = try parse(alternation: children[1], ruleName: subRuleName)
            return (
                [.nonTerminal(NonTerminal(name: subRuleName))],
                additionalRules
                    .union(subAlternations.map {
                        Production(pattern: NonTerminal(name: subRuleName), production: $0)
                    })
            )
        }
        
        func parse(integerLiteral: ParseTree) -> Int {
            guard case .node(key: "integer-literal", children: _) = integerLiteral else {
                fatalError("Invalid parse tree")
            }
            guard let integer = Int(integerLiteral.leafs.map {abnf[$0]}.joined()) else {
                fatalError("Invalid parse tree")
            }
            return integer
        }
        
        func parse(range: ParseTree) throws -> (Int?, Int?) {
            switch range {
            case .node(key: "closed-range", children: let children):
                let (lowerBound, upperBound) = (parse(integerLiteral: children[0]), parse(integerLiteral: children[2]))
                if lowerBound > upperBound {
                    throw ABNFImportError.invalidRange(
                        line: abnf[...range.leafs[0].lowerBound].filter {$0.isNewline}.count,
                        column: abnf.distance(
                            from: abnf[...range.leafs[0].lowerBound].lastIndex(where: {$0.isNewline}) ?? abnf.startIndex,
                            to: range.leafs[0].lowerBound
                        )
                    )
                }
                return (lowerBound, upperBound)
            case .node(key: "partial-range-to", children: let children):
                return (nil, parse(integerLiteral: children[1]))
            case .node(key: "partial-range-from", children: let children):
                return (parse(integerLiteral: children[0]), nil)
            case .node(key: "unlimited-range", children: _):
                return (nil, nil)
            default:
                fatalError("Invalid parse tree")
            }
        }
        
        func parse(variableRepetition: ParseTree, alternationIndex: Int, concatenationIndex: Int, ruleName: String) throws -> ([Symbol], Set<Production>) {
            guard case .node(key: "variable-repetition", children: let children) = variableRepetition, let range = children.first, let atom = children.last else {
                fatalError("Invalid parse tree")
            }
            
            let subRuleName = "\(ruleName)-a\(alternationIndex)-c\(concatenationIndex)"
            let (subrule, additionalRules) = try parse(atom: atom, alternationIndex: alternationIndex, concatenationIndex: concatenationIndex, ruleName: subRuleName)
            
            switch try parse(range: range) {
            case (.none, .none):
                let repeatingRule = [
                    Production(pattern: NonTerminal(name: subRuleName), production: []),
                    Production(pattern: NonTerminal(name: subRuleName), production: [.nonTerminal(NonTerminal(name: subRuleName))] + subrule),
                ]
                return (
                    [.nonTerminal(NonTerminal(name: subRuleName))],
                    additionalRules.union(repeatingRule)
                )
                
            case (.some(let lowerBound), .none):
                let repeatingRule = [
                    Production(pattern: NonTerminal(name: subRuleName), production: Array(repeatElement(subrule, count: lowerBound).joined())),
                    Production(pattern: NonTerminal(name: subRuleName), production: [.nonTerminal(NonTerminal(name: subRuleName))] + subrule),
                ]
                return (
                    [.nonTerminal(NonTerminal(name: subRuleName))],
                    additionalRules.union(repeatingRule)
                )
                
            case (.none, .some(let upperBound)):
                let optionalRange = repeatElement(NonTerminal(name: subRuleName), count: upperBound)
                
                let optionalRule = [
                    Production(pattern: NonTerminal(name: subRuleName), production: []),
                    Production(pattern: NonTerminal(name: subRuleName), production: subrule),
                ]
                return (
                    optionalRange.map(Symbol.nonTerminal),
                    additionalRules.union(optionalRule)
                )
                
            case (.some(let lowerBound), .some(let upperBound)):
                let baseRepetitions = Array(repeatElement(subrule, count: lowerBound).joined())
                let optionalRange = repeatElement(NonTerminal(name: subRuleName), count: upperBound - lowerBound)
                
                let optionalRule = [
                    Production(pattern: NonTerminal(name: subRuleName), production: []),
                    Production(pattern: NonTerminal(name: subRuleName), production: subrule),
                ]
                return (
                    baseRepetitions + optionalRange.map(Symbol.nonTerminal),
                    additionalRules.union(optionalRule)
                )
            }
        }
        
        func parse(specificRepetition: ParseTree, alternationIndex: Int, concatenationIndex: Int, ruleName: String) throws -> ([Symbol], Set<Production>) {
            guard case .node(key: "specific-repetition", children: let children) = specificRepetition, let countLiteral = children.first, let atom = children.last else {
                fatalError("Invalid parse tree")
            }
            let count = parse(integerLiteral: countLiteral)
            let (subrule, additionalRules) = try parse(atom: atom, alternationIndex: alternationIndex, concatenationIndex: concatenationIndex, ruleName: ruleName)
            return (
                Array(repeatElement(subrule, count: count).joined()),
                additionalRules
            )
        }
        
        func parse(repetition: ParseTree, alternationIndex: Int, concatenationIndex: Int, ruleName: String) throws -> ([Symbol], Set<Production>) {
            guard case .node(key: "repetition", children: let children) = repetition, children.count == 1, let firstChild = children.first else {
                fatalError("Invalid parse tree")
            }
            switch firstChild {
            case .node(key: "variable-repetition", children: _):
                return try parse(variableRepetition: firstChild, alternationIndex: alternationIndex, concatenationIndex: concatenationIndex, ruleName: ruleName)
                
            case .node(key: "specific-repetition", children: _):
                return try parse(specificRepetition: firstChild, alternationIndex: alternationIndex, concatenationIndex: concatenationIndex, ruleName: ruleName)
                
            default:
                fatalError("Invalid parse tree")
            }
        }
        
        func parse(atom: ParseTree, alternationIndex: Int, concatenationIndex: Int, ruleName: String) throws -> ([Symbol], Set<Production>) {
            guard case .node(key: let key, children: let children) = atom, children.count == 1, let firstChild = children.first, ["atom", "repeated-atom"].contains(key) else {
                fatalError("Invalid parse tree")
            }
            switch firstChild {
            case .node(key: "optional", children: _):
                return try parse(optional: firstChild, alternationIndex: alternationIndex, concatenationIndex: concatenationIndex, ruleName: ruleName)
                
            case .node(key: "sequence-group", children: _):
                return try parse(sequenceGroup: firstChild, alternationIndex: alternationIndex, concatenationIndex: concatenationIndex, ruleName: ruleName)

            case .node(key: "repetition", children: _):
                return try parse(repetition: firstChild, alternationIndex: alternationIndex, concatenationIndex: concatenationIndex, ruleName: ruleName)

            case .node(key: "nonterminal", children: _):
                return ([Symbol.nonTerminal(parse(nonTerminal: firstChild))], [])

            case .node(key: "terminal", children: _):
                return try ([Symbol.terminal(parse(terminal: firstChild))], [])
                
            default:
                fatalError("Invalid parse tree")
            }
        }
        
        func parse(concatenation: ParseTree, atIndex index: Int, ruleName: String) throws -> ([Symbol], Set<Production>) {
            let atoms = flatten(parseTree: concatenation, nodeName: "concatenation")
            return try atoms.enumerated()
                .map {try parse(atom: $1, alternationIndex: index, concatenationIndex: $0, ruleName: ruleName)}
                .reduce(into: ([], [])) { acc, atom in
                    acc.0 += atom.0
                    acc.1.formUnion(atom.1)
                }
        }
        
        func parse(alternation: ParseTree, ruleName: String) throws -> ([[Symbol]], Set<Production>) {
            let concatenations = flatten(parseTree: alternation, nodeName: "alternation")
                .filter {$0.root != nil}
            return try concatenations.enumerated()
                .map {try parse(concatenation: $1, atIndex: $0, ruleName: ruleName)}
                .reduce(into: ([], [])) { acc, cat in
                    acc.0.append(cat.0)
                    acc.1.formUnion(cat.1)
                }
        }
        
        func parse(initRule rule: ParseTree) throws -> ([Production], Set<Production>) {
            guard case .node(key: "init-rule", children: let children) = rule, children.count == 3 else {
                fatalError("Invalid parse tree")
            }
            let pattern = parse(nonTerminal: children[0])
            let (alternations, utilityProds) = try parse(alternation: children[2], ruleName: pattern.name)
            return (
                alternations.map {
                    Production(pattern: pattern, production: $0)
                },
                utilityProds
            )
        }
        
        func parse(incrementalRule rule: ParseTree) throws -> ([Production], Set<Production>) {
            guard case .node(key: "incremental-alternation", children: let children) = rule, children.count == 4 else {
                fatalError("Invalid parse tree")
            }
            let pattern = parse(nonTerminal: children[0])
            let (alternations, utilityProds) = try parse(alternation: children[3], ruleName: pattern.name)
            return (
                alternations.map {
                    Production(pattern: pattern, production: $0)
                },
                utilityProds
            )
        }
        
        func parse(rule: ParseTree) throws -> ([Production], Set<Production>) {
            guard case .node(key: "rule", children: let children) = rule, let firstChild = children.first, children.count == 1 else {
                fatalError("Invalid parse tree")
            }
            switch firstChild {
            case .node(key: "init-rule", children: _):
                return try parse(initRule: firstChild)
            case .node(key: "incremental-alternation", children: _):
                return try parse(incrementalRule: firstChild)
            default:
                fatalError("Invalid parse tree")
            }
        }
        
        let parser = EarleyParser(grammar: abnfGrammar)
        guard let tree = try parser.syntaxTree(for: abnf).filter({$0.name != "whitespace"}) else {
            self = Grammar(productions: [], start: NonTerminal(name: start))
            return
        }
        let ruleExpressions = tree.allNodes(where: {$0.name == "rule"})
        let allProductions = try ruleExpressions.map(parse(rule:))
        let (visibleRules, hiddenRules): ([[Production]], [Set<Production>]) = unzip(allProductions)
        
        self = Grammar(
            productions: Grammar.coreRules + Array(visibleRules.joined()) + Array(hiddenRules.joined()),
            start: NonTerminal(name: start),
            utilityNonTerminals: Set(hiddenRules.joined().map {$0.pattern})
        )
    }
}
//
//  SyntaxTree.swift
//  Covfefe
//
//  Created by Palle Klewitz on 07.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

/// A tree which can store different types of values in its leafs
///
/// - leaf: A leaf
/// - node: A node with a key and an arbitrary list of elements
public enum SyntaxTree<Element, LeafElement> {
    /// A leaf storing a leaf element
    case leaf(LeafElement)
    
    /// A node with a key and an arbitrary list of elements
    indirect case node(key: Element, children: [SyntaxTree<Element, LeafElement>])
}

public extension SyntaxTree {
    
    /// Generates a new syntax tree by applying the transform function to every key of the tree
    ///
    /// - Parameter transform: Transform function
    /// - Returns: A tree generated by applying the transform function to every key
    func map<Result>(_ transform: (Element) throws -> Result) rethrows -> SyntaxTree<Result, LeafElement> {
        switch self {
        case .leaf(let leaf):
            return .leaf(leaf)
            
        case .node(key: let key, children: let children):
            return try .node(key: transform(key), children: children.map{try $0.map(transform)})
        }
    }
    
    /// Generates a new syntax tree by applying the transform function to every leaf of the tree
    ///
    /// - Parameter transform: Transform function
    /// - Returns: A tree generated by applying the transform function to every leaf value
    func mapLeafs<Result>(_ transform: (LeafElement) throws -> Result) rethrows -> SyntaxTree<Element, Result> {
        switch self {
        case .leaf(let leaf):
            return try .leaf(transform(leaf))
            
        case .node(key: let key, children: let children):
            return try .node(key: key, children: children.map{try $0.mapLeafs(transform)})
        }
    }
    
    /// All leafs of the tree
    var leafs: [LeafElement] {
        switch self {
        case .leaf(let leaf):
            return [leaf]
            
        case .node(key: _, children: let children):
            return children.flatMap{$0.leafs}
        }
    }
    
    /// Filters the tree by removing all nodes and their corresponding subtrees if the given predicate is false
    ///
    /// - Parameter predicate: Predicate to filter the tree
    /// - Returns: A tree generated by filtering out nodes for which the predicate returned false
    func filter(_ predicate: (Element) throws -> Bool) rethrows -> SyntaxTree<Element, LeafElement>? {
        switch self {
        case .leaf(let element):
            return .leaf(element)
            
        case .node(key: let key, children: let children) where try predicate(key):
            return try .node(key: key, children: children.compactMap{try $0.filter(predicate)})
            
        case .node(key: _, children: _):
            return nil
        }
    }
    
    /// Explodes nodes and passes all child nodes to the parent node if the given closure returns true
    ///
    /// - Parameter shouldExplode: Determines if a node should be exploded
    /// - Returns: A tree generated by exploding nodes determined by the given predicate
    func explode(_ shouldExplode: (Element) throws -> Bool) rethrows -> [SyntaxTree<Element, LeafElement>] {
        switch self {
        case .leaf:
            return [self]
            
        case .node(key: let key, children: let children) where try shouldExplode(key):
            return try children.flatMap{try $0.explode(shouldExplode)}
            
        case .node(key: let key, children: let children):
            return try [.node(key: key, children: children.flatMap{try $0.explode(shouldExplode)})]
        }
    }
    
    /// Compresses the tree by exploding nodes which have exactly one child node
    ///
    /// - Returns: Tree generated by compressing the current tree
    func compressed() -> SyntaxTree<Element, LeafElement> {
        switch self {
        case .node(key: _, children: let children) where children.count == 1:
            let child = children[0]
            if case .leaf = child {
                return self
            } else {
                return child.compressed()
            }
            
        case .node(key: let key, children: let children):
            return .node(key: key, children: children.map{$0.compressed()})
            
        default:
            return self
        }
    }
    
    /// Returns all nodes which match the given predicate.
    ///
    ///
    /// - Parameter predicate: Predicate to match
    /// - Returns: A collection of nodes which match the given predicate
    func allNodes(where predicate: (Element) throws -> Bool) rethrows -> [SyntaxTree<Element, LeafElement>] {
        switch self {
        case .leaf:
            return []
            
        case .node(key: let key, children: let children) where try predicate(key):
            return try [self] + children.flatMap{try $0.allNodes(where: predicate)}
            
        case .node(key: _, children: let children):
            return try children.flatMap{try $0.allNodes(where: predicate)}
        }
    }
    
    
}

extension SyntaxTree: CustomDebugStringConvertible {
    public var debugDescription: String {
        switch self {
        case .leaf(let value):
            return "leaf (value: \(value))"
            
        case .node(key: let key, children: let children):
            let childrenDescription = children.map{$0.debugDescription}.joined(separator: "\n").replacingOccurrences(of: "\n", with: "\n\t")
            return """
            node (key: \(key)) {
                \(childrenDescription)
            }
            """
        }
    }
}

extension SyntaxTree: CustomStringConvertible {
    public var description: String {
        var id = 0
        let uniqueKeyTree = self.map { element -> (Int, Element) in
            let uniqueElement = (id, element)
            id += 1
            return uniqueElement
        }.mapLeafs { leaf -> (Int, LeafElement) in
            let uniqueLeaf = (id, leaf)
            id += 1
            return uniqueLeaf
        }
        
        
        func generateDescription(_ tree: SyntaxTree<(Int, Element), (Int, LeafElement)>) -> String {
            switch tree {
            case .leaf(let leaf):
                let (id, leafElement) = leaf
                let leafDescription = "\(leafElement)"
                    .literalEscaped
                    .replacingOccurrences(of: "\"", with: "\\\"")
                return "node\(id) [label=\"\(leafDescription)\" shape=box]"
                
            case .node(key: let key, children: let children):
                let (id, element) = key
                let childrenDescriptions = children.map(generateDescription).filter{!$0.isEmpty}.joined(separator: "\n")
                let childrenPointers = children.compactMap{ node -> Int? in
                    if let id = node.root?.0 {
                        return id
                    } else if let id = node.leaf?.0 {
                        return id
                    } else {
                        return nil
                    }
                }.map{"node\(id) -> node\($0)"}.joined(separator: "\n")
                
                var result = "node\(id) [label=\"\(element)\"]"
                if !childrenPointers.isEmpty {
                    result += "\n\(childrenPointers)"
                }
                if !childrenDescriptions.isEmpty {
                    result += "\n\(childrenDescriptions)"
                }
                
                return result
            }
        }
        
        func allLeafIDs(_ tree: SyntaxTree<(Int, Element), (Int, LeafElement)>) -> [Int] {
            switch tree {
            case .leaf(let leaf):
                return [leaf.0]
                
            case .node(key: _, children: let children):
                return children.flatMap(allLeafIDs)
            }
        }
        
        return """
        digraph {
            \(generateDescription(uniqueKeyTree).replacingOccurrences(of: "\n", with: "\n\t"))
            {
                rank = same
                \(allLeafIDs(uniqueKeyTree).map(String.init).map{"node\($0)"}.joined(separator: "\n\t\t"))
            }
        }
        """
    }
}

/// Determines if two syntax trees are equal to each other.
///
/// This function returns true, if both trees have the same structure, equal keys in equal nodes and equal leafs
///
/// - Parameters:
///   - lhs: First tree to compare
///   - rhs: Second tree to compare
/// - Returns: A boolean value indicating whether the provided trees are equal to each other
public func == <Element: Equatable, LeafElement: Equatable>(lhs: SyntaxTree<Element, LeafElement>, rhs: SyntaxTree<Element, LeafElement>) -> Bool {
    switch (lhs, rhs) {
    case (.leaf, .leaf):
        return true
        
    case (.node(key: let lKey, children: let lChildren), .node(key: let rKey, children: let rChildren)):
        return lKey == rKey && lChildren.count == rChildren.count && !zip(lChildren, rChildren).map(==).contains(false)
        
    default:
        return false
    }
}

public extension SyntaxTree {
    
    /// Creates a new syntax tree node with a given key and a list of children
    ///
    /// - Parameters:
    ///   - key: Root key
    ///   - children: Children of the root node
    init(key: Element, children: [SyntaxTree<Element, LeafElement>]) {
        self = .node(key: key, children: children)
    }
    
    /// Creates a new syntax tree with a given root key and no children
    ///
    /// - Parameter key: Root key
    init(key: Element) {
        self = .node(key: key, children: [])
    }
    
    /// Creates a new syntax tree with a given leaf value
    ///
    /// - Parameter value: Leaf value
    init(value: LeafElement) {
        self = .leaf(value)
    }
}

public extension SyntaxTree where LeafElement == () {
    /// Creates an empty tree
    init() {
        self = .leaf(())
    }
}

public extension SyntaxTree {
    
    /// Returns the root key of the tree or nil if no root key exists
    var root: Element? {
        guard case .node(key: let root, children: _) = self else {
            return nil
        }
        return root
    }
    
    /// Returns the value stored in the current node if the current node is a leaf. Otherwise, nil is returned
    var leaf: LeafElement? {
        guard case .leaf(let leaf) = self else {
            return nil
        }
        return leaf
    }
    
    /// Returns the direct children of the root node
    var children: [SyntaxTree<Element, LeafElement>]? {
        switch self {
        case .leaf:
            return nil
            
        case .node(key: _, children: let children):
            return children
        }
    }
}
//
//  Grammar.swift
//  Covfefe
//
//  Created by Palle Klewitz on 07.08.17.
//  Copyright (c) 2017 - 2020 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

/// A syntax error which was generated during parsing or tokenization
public struct SyntaxError: Error {
    
    /// The reason for the syntax error
    ///
    /// - emptyNotAllowed: An empty string was provided but the grammar does not allow empty productions
    /// - unknownToken: The tokenization could not be completed because no matching token was found
    /// - unmatchedPattern: A pattern was found which could not be merged
    /// - unexpectedToken: A token was found that was not expected
    public enum Reason {
        /// An empty string was provided but the grammar does not allow empty productions
        case emptyNotAllowed
        
        /// The tokenization could not be completed because no matching token was found
        case unknownToken
        
        /// A pattern was found which could not be merged
        case unmatchedPattern
        
        /// A token was found that was not expected
        case unexpectedToken
    }
    
    /// Range in which the error occurred
    public let range: Range<String.Index>
    
    /// Reason for the error
    public let reason: Reason
    
    /// The context around the error
    public let context: [NonTerminal]
    
    /// The string for which the parsing was unsuccessful.
    public let string: String
    
    /// The line in which the error occurred.
    ///
    /// The first line of the input string is line 0.
    public var line: Int {
        if string.count == 0 {
            return 0
        }
        return string[...range.lowerBound].filter { (char: Character) in
            char.isNewline
        }.count
    }
    
    public var column: Int {
        if string.count == 0 {
            return 0
        }
        let lastNewlineIndex = string[...range.lowerBound].lastIndex(where: {$0.isNewline}) ?? string.startIndex
        return string.distance(from: lastNewlineIndex, to: range.lowerBound)
    }
    
    /// Creates a new syntax error with a given range and reason
    ///
    /// - Parameters:
    ///   - range: String range in which the syntax error occurred
    ///   - string: String which was unsuccessfully parsed
    ///   - reason: Reason why the syntax error occurred
    ///   - context: Non-terminals which were expected at the location of the error.
    public init(range: Range<String.Index>, in string: String, reason: Reason, context: [NonTerminal] = []) {
        self.range = range
        self.string = string
        self.reason = reason
        self.context = context
    }
}

extension SyntaxError: CustomStringConvertible {
    public var description: String {
        let main = "Error: \(reason) at L\(line):\(column): '\(string[range])'"
        if !context.isEmpty {
            return "\(main), expected: \(context.map{$0.description}.joined(separator: " | "))"
        } else {
            return main
        }
    }
}

extension SyntaxError.Reason: CustomStringConvertible {
    public var description: String {
        switch self {
        case .emptyNotAllowed:
            return "Empty string not accepted"
        case .unknownToken:
            return "Unknown token"
        case .unmatchedPattern:
            return "Unmatched pattern"
        case .unexpectedToken:
            return "Unexpected token"
        }
    }
}


/// A context free or regular grammar
/// consisting of a set of productions
///
/// In context free grammars, the left side of productions
/// (in this framework also referred to as pattern) is always
/// a single non-terminal.
///
/// Grammars might be ambiguous. For example, the grammar
///
///        <expr> ::= <expr> '+' <expr> | 'a'
///
/// can recognize the expression `a+a+a+a` in 5 different ways:
/// `((a+a)+a)+a`, `(a+(a+a))+a`, `a+(a+(a+a))`, `a+((a+a)+a)`, `(a+a)+(a+a)`.
public struct Grammar {
    
    /// Productions for generating words of the language generated by this grammar
    public var productions: [Production]
    
    /// Root non-terminal
    ///
    /// All syntax trees of words in this grammar must have a root containing this non-terminal.
    public var start: NonTerminal
    
    /// Non-terminals generated by normalizing the grammar.
    let utilityNonTerminals: Set<NonTerminal>
    
    /// Creates a new grammar with a given set of productions and a start non-terminal
    ///
    /// - Parameters:
    ///   - productions: Productions for generating words
    ///   - start: Root non-terminal
    public init(productions: [Production], start: NonTerminal) {
        self.init(productions: productions, start: start, utilityNonTerminals: [])
        
        // assertNonFatal(unreachableNonTerminals.isEmpty, "Grammar contains unreachable non-terminals (\(unreachableNonTerminals))")
        // assertNonFatal(unterminatedNonTerminals.isEmpty, "Grammar contains non-terminals which can never reach terminals (\(unterminatedNonTerminals))")
    }
    
    /// Creates a new grammar with a given set of productions, a start non-terminal and
    /// a set of non-terminals which have been created for normalization
    ///
    /// - Parameters:
    ///   - productions: Productions for generating words
    ///   - start: Root non-terminal
    ///   - normalizationNonTerminals: Non-terminals generated during normalization
    init(productions: [Production], start: NonTerminal, utilityNonTerminals: Set<NonTerminal>) {
        self.productions = productions
        self.start = start
        self.utilityNonTerminals = utilityNonTerminals
    }
}


extension Grammar: CustomStringConvertible {
    
    /// Returns a Backus-Naur form representation of the grammar.
    ///
    /// Production rules are encoded in the following form:
    /// `pattern ::= production-result`, where the pattern is always a single non-terminal and the production-result
    /// is a list of alternative results separated by `|` (or just one single result). The production result is a concatenation
    /// of terminal and non-terminal symbols. Terminals are delimited by single or double quotation marks; non-terminals
    /// are delimited by angle brackets (`<`, `>`). Concatenations consist of one or multiple symbols separated by zero or more
    /// whitespace characters.
    ///
    /// Example:
    ///
    ///        <non-terminal-pattern> ::= <produced-non-terminal-pattern> | 'terminal' <concatenated-non-terminal>
    public var bnf: String {
        let groupedProductions = Dictionary(grouping: self.productions) { production in
            production.pattern
        }
        return groupedProductions.sorted(by: {$0.key.name < $1.key.name}).map { entry -> String in
            let (pattern, productions) = entry
            
            let productionString = productions.map { production in
                if production.production.isEmpty {
                    return "\"\""
                }
                return production.production.map { symbol -> String in
                    switch symbol {
                    case .nonTerminal(let nonTerminal):
                        return "<\(nonTerminal.name)>"
                        
                    case .terminal(.string(let string, _)) where string.contains("\""):
                        let escapedValue = string.singleQuoteLiteralEscaped
                        return "'\(escapedValue)'"
                        
                    case .terminal(.string(let string, _)):
                        let escapedValue = string.doubleQuoteLiteralEscaped
                        return "\"\(escapedValue)\""
                        
                    case .terminal(.regularExpression(let expression, _)) where expression.pattern.contains("\""):
                        let escapedValue = expression.pattern.singleQuoteLiteralEscaped
                        return "'\(escapedValue)'"
                        
                    case .terminal(.regularExpression(let expression, _)):
                        let escapedValue = expression.pattern.doubleQuoteLiteralEscaped
                        return "\"\(escapedValue)\""
                        
                    case .terminal(.characterRange(let range, _)):
                        let lowerString: String
                        let upperString: String
                        
                        if range.lowerBound == "'" {
                            lowerString = "\"'\""
                        } else {
                            lowerString = "'\(range.lowerBound)'"
                        }
                        
                        if range.upperBound == "'" {
                            upperString = "\"'\""
                        } else {
                            upperString = "'\(range.upperBound)'"
                        }
                        
                        return "\(lowerString) ... \(upperString)"
                    }
                }.joined(separator: " ")
            }.joined(separator: " | ")
            
            return "<\(pattern.name)> ::= \(productionString)"
        }.joined(separator: "\n")
    }
    
    
    /// Returns a Extended Backus-Naur form representation of the grammar.
    ///
    /// Production rules are encoded in the following form:
    /// `pattern = production-result;`, where the pattern is always a single non-terminal and the production-result
    /// is a list of alternative results separated by `|` (or just one single result). The production result is a concatenation
    /// of terminal and non-terminal symbols. Terminals are delimited by single or double quotation marks; non-terminals
    /// are not delimited by a special character. Concatenations consist of one or multiple symbols separated by a comma.
    ///
    /// Example:
    ///
    ///        non-terminal pattern = produced non-terminal | 'terminal', concatenated non-terminal;
    public var ebnf: String {
        let groupedProductions = Dictionary(grouping: self.productions) { production in
            production.pattern
        }
        return groupedProductions.sorted(by: {$0.key.name < $1.key.name}).map { entry -> String in
            let (pattern, productions) = entry
            
            let productionString = productions.map { production in
                if production.production.isEmpty {
                    return "\"\""
                }
                return production.production.map { symbol -> String in
                    switch symbol {
                    case .nonTerminal(let nonTerminal):
                        return nonTerminal.name
                        
                    case .terminal(.string(let string, _)) where string.contains("\""):
                        let escapedValue = string.singleQuoteLiteralEscaped
                        return "'\(escapedValue)'"
                        
                    case .terminal(.string(let string, _)):
                        let escapedValue = string.doubleQuoteLiteralEscaped
                        return "\"\(escapedValue)\""
                        
                    case .terminal(.regularExpression(let expression, _)) where expression.pattern.contains("\""):
                        let escapedValue = expression.pattern.singleQuoteLiteralEscaped
                        return "'\(escapedValue)'"
                        
                    case .terminal(.regularExpression(let expression, _)):
                        let escapedValue = expression.pattern.doubleQuoteLiteralEscaped
                        return "\"\(escapedValue)\""
                        
                    case .terminal(.characterRange(let range, _)):
                        let lowerString: String
                        let upperString: String
                        
                        if range.lowerBound == "'" {
                            lowerString = "\"'\""
                        } else {
                            lowerString = "'\(range.lowerBound)'"
                        }
                        
                        if range.upperBound == "'" {
                            upperString = "\"'\""
                        } else {
                            upperString = "'\(range.upperBound)'"
                        }
                        
                        return "\(lowerString) ... \(upperString)"
                    }
                }.joined(separator: ", ")
            }.joined(separator: " | ")
            
            return "\(pattern.name) = \(productionString);"
        }.joined(separator: "\n")
    }
    
    /// Returns a Augmented Backus-Naur form representation of the grammar.
    ///
    /// Production rules are encoded in the following form:
    /// `pattern = production-result`, where the pattern is always a single non-terminal and the production-result
    /// is a list of alternative results separated by `/` (or just one single result). The production result is a concatenation
    /// of terminal and non-terminal symbols.
    ///
    /// Example:
    ///
    ///        non-terminal-pattern = produced non-terminal / "terminal" concatenated non-terminal;
    public var abnf: String {
        let groupedProductions = Dictionary(grouping: self.productions) { production in
            production.pattern
        }
        return groupedProductions.sorted(by: {$0.key.name < $1.key.name}).map { entry -> String in
            let (pattern, productions) = entry
            
            let productionString = productions.map { production in
                if production.production.isEmpty {
                    return "\"\""
                }
                return production.production.map { symbol -> String in
                    switch symbol {
                    case .nonTerminal(let nonTerminal):
                        return nonTerminal.name
   
                    case .terminal(.string(let string, _)):
                        if let scalar = string.unicodeScalars.first, string.unicodeScalars.count == 1 {
                            return "%x\(String(scalar.value, radix: 16))"
                        }
                        let escapedValue = string.doubleQuoteLiteralEscaped
                        return "\"\(escapedValue)\""
                        
                    case .terminal(.regularExpression):
                        fatalError("Regular expressions cannot be expressed in standard ABNF")
                        
                    case .terminal(.characterRange(let range, _)):
                        let lowerBound = String(range.lowerBound.unicodeScalars.first!.value, radix: 16)
                        let upperBound = String(range.upperBound.unicodeScalars.first!.value, radix: 16)
                        
                        return "%x\(lowerBound)-\(upperBound)"
                    }
                }.joined(separator: " ")
            }.joined(separator: " / ")
            
            return "\(pattern.name) = \(productionString);"
        }.joined(separator: "\n")
    }
    
    public var description: String {
        return bnf
    }
        
}

public extension Grammar {
    
    /// Returns true, if the grammar is in chomsky normal form.
    ///
    /// A grammar is in chomsky normal form if all productions satisfy one of the following conditions:
    ///
    /// - A production generates exactly one terminal symbol
    /// - A production generates exactly two non-terminal symbols
    /// - A production generates an empty string and is generated from the start non-terminal
    ///
    /// Certain parsing algorithms, such as the CYK parser, require the recognized grammar to be in Chomsky normal form.
    var isInChomskyNormalForm: Bool {
        return productions.allSatisfy { production -> Bool in
            (production.isFinal && production.production.count == 1)
            || (!production.isFinal && production.generatedNonTerminals.count == 2 && production.generatedTerminals.count == 0)
            || (production.production.isEmpty && production.pattern == start)
        }
    }
}

extension Grammar: Equatable {
    public static func == (lhs: Grammar, rhs: Grammar) -> Bool {
        return lhs.start == rhs.start && Set(lhs.productions) == Set(rhs.productions)
    }
}

extension Grammar: Codable {}
//
//  CYKParser.swift
//  Covfefe
//
//  Created by Palle Klewitz on 15.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation


/// A parser based on the CYK algorithm.
///
/// The parser can parse non-deterministic and deterministic grammars.
/// It requires O(n^3) runtime.
///
/// For ambiguous grammars, the runtime for parses may increase,
/// as some expressions have exponentially many possible parse trees depending on expression length.
/// This exponential growth can be avoided by only generating a single parse tree with `syntaxTree(for:)`.
///
/// For unambiguous grammars, the Earley parser should be used instead, as it has linear or quadratic runtime.
///
/// The original CYK parsing can only recognize grammars in Chomsky normal form.
/// This parser automatically transforms the recognized grammar into Chomsky normal form
/// and transforms the parse tree back to the original grammar. Nullable items are ommited in the
/// parse tree of the CYK parser.
public struct CYKParser: AmbiguousGrammarParser {
    
    /// The grammar which the parser recognizes
    public let grammar: Grammar
    
    /// The parser requires the grammar to be in chomsky normal form
    private let normalizedGrammar: Grammar
    
    // In CYK parsing, no intelligent tokenizing is possible. A standard tokenizer is used instead.
    private let tokenizer: Tokenizer
    
    /// Initializes a CYK parser which recognizes the given grammar.
    ///
    /// The parser can parse non-deterministic and deterministic context free languages in O(n^3).
    ///
    /// - Parameter grammar: The grammar which the parser recognizes.
    public init(grammar: Grammar) {
        self.grammar = grammar
        self.normalizedGrammar = grammar.chomskyNormalized()
        self.tokenizer = DefaultTokenizer(grammar: grammar)
    }
    
    /// Generates an error from a CYK table if the grammar cannot be used to generate a given word.
    ///
    /// - Parameter cykTable: Table containing unfinished syntax trees
    /// - Returns: An error pointing towards the first invalid token in the string.
    private func generateError(_ cykTable: Array<[[SyntaxTree<Production, Range<String.Index>>]]>, string: String) -> SyntaxError {
        let memberRows = (0..<cykTable.count).map { columnIndex -> Int? in
            (0 ..< (cykTable.count - columnIndex)).reduce(nil) { maxIndex, rowIndex -> Int? in
                if cykTable[rowIndex][columnIndex].contains(where: { tree -> Bool in
                    tree.root?.pattern == normalizedGrammar.start
                }) {
                    return rowIndex
                }
                return maxIndex
            }
        }
        
        if let firstMember = memberRows[0] {
            return SyntaxError(range: cykTable[0][firstMember+1][0].leafs.first!, in: string, reason: .unmatchedPattern)
        } else {
            return SyntaxError(range: cykTable[0][0][0].leafs.first!, in: string, reason: .unmatchedPattern)
        }
    }
    
    /// Reintroduces chain productions which have been eliminated during normalization
    ///
    /// - Parameter tree: Syntax tree without chain productions
    /// - Returns: Syntax tree with chain productions added.
    private func unfoldChainProductions(_ tree: SyntaxTree<Production, Range<String.Index>>) -> ParseTree {
        switch tree {
        case .leaf(let leaf):
            return .leaf(leaf)
            
        case .node(key: let production, children: let children):
            guard let chain = production.nonTerminalChain else {
                return .node(key: production.pattern, children: children.map(unfoldChainProductions))
            }
            let newNode = chain.reversed().reduce(children.map(unfoldChainProductions)) { (childNodes, nonTerminal) -> [ParseTree] in
                [SyntaxTree.node(key: nonTerminal, children: childNodes)]
            }
            return .node(key: production.pattern, children: newNode)
        }
    }

    private func syntaxTree(for string: String, ignoreAmbiguousItems: Bool) throws -> [ParseTree] {
        let tokens = try self.tokenizer.tokenize(string)
        if tokens.isEmpty {
            if normalizedGrammar.productions.contains(where: { production -> Bool in
                production.pattern == normalizedGrammar.start
                    && production.generatesEmpty(in: normalizedGrammar)
            }) {
                return [SyntaxTree.node(key: normalizedGrammar.start, children: [SyntaxTree.leaf(string.startIndex ..< string.endIndex)])]
            } else {
                throw SyntaxError(range: string.startIndex ..< string.endIndex, in: string, reason: .emptyNotAllowed)
            }
        }
        
        let terminalProductions = normalizedGrammar.productions.filter{$0.isFinal}.filter {!$0.production.isEmpty}
        let startTrees = tokens.map { alternatives -> [SyntaxTree<Production, Range<String.Index>>] in
            alternatives.flatMap { token -> [SyntaxTree<Production, Range<String.Index>>] in
                terminalProductions.filter { production in
                    production.generatedTerminals[0] == token.terminal
                }.map { production -> SyntaxTree<Production, Range<String.Index>> in
                    SyntaxTree.node(key: production, children: [SyntaxTree.leaf(token.range)])
                }
            }
        }
        
        let nonTerminalProductions = Dictionary(grouping: normalizedGrammar.productions.filter{!$0.isFinal}) { production -> NonTerminalString in
            NonTerminalString(characters: production.generatedNonTerminals)
        }
        
        var cykTable = [[[SyntaxTree<Production, Range<String.Index>>]]](repeating: [], count: startTrees.count)
        cykTable[0] = startTrees
        
        for row in 1 ..< cykTable.count {
            let upperBound = cykTable.count - row
            
            cykTable[row] = (0..<upperBound).map { column -> [SyntaxTree<Production, Range<String.Index>>] in
                let cell = (1...row).flatMap { offset -> [SyntaxTree<Production, Range<String.Index>>] in
                    let ref1Row = row - offset
                    let ref2Col = column + row - offset + 1
                    let ref2Row = offset - 1
                    
                    return crossFlatMap(cykTable[ref1Row][column], cykTable[ref2Row][ref2Col]) { leftTree, rightTree -> [SyntaxTree<Production, Range<String.Index>>] in
                        let combinedString = NonTerminalString(characters: [leftTree.root!.pattern, rightTree.root!.pattern])
                        let possibleProductions = nonTerminalProductions[combinedString, default: []]
                        return possibleProductions.map { pattern -> SyntaxTree<Production, Range<String.Index>> in
                            return SyntaxTree(key: pattern, children: [leftTree, rightTree])
                        }
                    }
                }
                if ignoreAmbiguousItems {
                    return cell.unique(by: {$0.root!.pattern}).collect(Array.init)
                } else {
                    return cell
                }
            }
        }
        
        // If a given word is not a member of the language generated by this grammar
        // an error will be computed that returns the first and largest structure
        // in the syntax tree that the parser was unable to process.
        let syntaxTrees = cykTable[cykTable.count-1][0].filter { tree -> Bool in
            tree.root?.pattern == normalizedGrammar.start
        }
        if syntaxTrees.isEmpty {
            throw generateError(cykTable, string: string)
        }
        return syntaxTrees.map{unfoldChainProductions($0).explode(normalizedGrammar.utilityNonTerminals.contains)[0]}
    }
    
    /// Creates a syntax tree which explains how a word was derived from a grammar
    ///
    /// - Parameter string: Input word, for which a parse tree should be generated
    /// - Returns: A syntax tree explaining how the grammar can be used to derive the word described by the given tokenization
    /// - Throws: A syntax error if the word is not in the language recognized by the parser
    public func syntaxTree(for string: String) throws -> ParseTree {
        return try self.syntaxTree(for: string, ignoreAmbiguousItems: true)[0]
    }
    
    /// Generates all syntax trees explaining how a word can be derived from a grammar.
    ///
    /// This function should only be used for ambiguous grammars and if it is necessary to
    /// retrieve all parse trees, as it comes with an additional cost in runtime.
    ///
    /// For unambiguous grammars, this function should return the same results as `syntaxTree(for:)`.
    ///
    /// - Parameter string: Input word, for which all parse trees should be generated
    /// - Returns: All syntax trees which explain how the input was derived from the recognized grammar
    /// - Throws: A syntax error if the word is not in the language recognized by the parser
    public func allSyntaxTrees(for string: String) throws -> [ParseTree] {
        return try self.syntaxTree(for: string, ignoreAmbiguousItems: false)
    }
}
//
//  Utility.swift
//  Covfefe
//
//  Created by Palle Klewitz on 07.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

extension Sequence {
    @available(*, unavailable, renamed: "allSatisfy")
    func allMatch(_ predicate: (Element) throws -> Bool) rethrows -> Bool {
        return try !self.contains(where: {try !predicate($0)})
    }
    
    func unique<Property: Hashable>(by property: @escaping (Element) -> Property) -> AnySequence<Element> {
        return sequence(state: (makeIterator(), [])) { (state: inout (Iterator, Set<Property>)) -> Element? in
            while let next = state.0.next() {
                guard !state.1.contains(property(next)) else {
                    continue
                }
                state.1.insert(property(next))
                return next
            }
            return nil
        }.collect(AnySequence.init)
    }
}

fileprivate extension IteratorProtocol {
    mutating func skip(_ count: Int) {
        for _ in 0 ..< count {
            _ = self.next()
        }
    }
    
    func peek() -> Element? {
        var iterator = self
        return iterator.next()
    }
}

extension Sequence {
    func strided(_ stride: Int, start: Int? = nil) -> AnySequence<Element> {
        var iterator = self.makeIterator()
        iterator.skip(start ?? 0)
        return sequence(state: iterator) { (iterator: inout Iterator) -> Element? in
            let next = iterator.next()
            iterator.skip(stride - 1)
            return next
            }.collect(AnySequence.init)
    }
    
    // Improves code readability by transforming e.g. Set(a.map{...}.filter{...}) to a.map{...}.filter{...}.collect(Set.init)
    // so the order of reading equals the order of evaluation
    func collect<Result>(_ collector: (Self) throws -> Result) rethrows -> Result {
        return try collector(self)
    }
    
    func pairs() -> AnySequence<(Element, Element)> {
        return sequence(state: self.makeIterator()) { (iterator: inout Iterator) -> (Element, Element)? in
            guard let first = iterator.next(), let second = iterator.peek() else {
                return nil
            }
            return (first, second)
        }.collect(AnySequence.init)
    }

    func prefixes() -> AnySequence<[Element]> {
        return sequence(state: (self.makeIterator(), [])) { (state: inout (Iterator, [Element])) -> [Element]? in
            guard let next = state.0.next() else {
                return nil
            }
            state.1.append(next)
            return state.1
        }.collect(AnySequence.init)
    }
}

extension Sequence where Element: Hashable {
    func uniqueElements() -> AnySequence<Element> {
        return unique(by: {$0})
    }
}

extension Sequence where Element: Sequence {
    func combinations() -> [[Element.Element]] {
        func combine(_ iterator: Iterator, partialResult: [[Element.Element]]) -> [[Element.Element]] {
            var iterator = iterator
            guard let next = iterator.next() else {
                return partialResult
            }
            return combine(iterator, partialResult: crossProduct(partialResult, next).map{$0 + [$1]})
        }
        return combine(makeIterator(), partialResult: [[]])
    }
}

func crossProduct<S1: Sequence, S2: Sequence>(_ lhs: S1, _ rhs: S2) -> AnySequence<(S1.Element, S2.Element)> {
    return sequence(
        state: (
            lhsIterator: lhs.makeIterator(),
            lhsElement: Optional<S1.Element>.none,
            rhsIterator: rhs.makeIterator(),
            rhsIteratorBase: rhs.makeIterator()
        ),
        next: { (state: inout (lhsIterator: S1.Iterator, lhsElement: S1.Element?, rhsIterator: S2.Iterator, rhsIteratorBase: S2.Iterator)) -> (S1.Element, S2.Element)? in
            guard let lhsElement = state.lhsElement ?? state.lhsIterator.next() else {
                return nil
            }
            state.lhsElement = lhsElement
            if let rhsElement = state.rhsIterator.next() {
                return (lhsElement, rhsElement)
            } else {
                state.rhsIterator = state.rhsIteratorBase
                
                guard let lhsNewElement = state.lhsIterator.next(), let rhsElement = state.rhsIterator.next() else {
                    return nil
                }
                state.lhsElement = lhsNewElement
                return (lhsNewElement, rhsElement)
            }
        }
    ).collect(AnySequence.init)
}

func crossMap<S1: Sequence, S2: Sequence, ElementOfResult>(_ lhs: S1, _ rhs: S2, transform: (S1.Element, S2.Element) throws -> ElementOfResult) rethrows -> [ElementOfResult] {
    var result: [ElementOfResult] = Array()
    result.reserveCapacity(lhs.underestimatedCount * rhs.underestimatedCount)
    for e1 in lhs {
        for e2 in rhs {
            try result.append(transform(e1, e2))
        }
    }
    return result
}

func crossFlatMap<S1: Sequence, S2: Sequence, ElementOfResult>(_ lhs: S1, _ rhs: S2, transform: (S1.Element, S2.Element) throws -> [ElementOfResult]) rethrows -> [ElementOfResult] {
    var result: [ElementOfResult] = Array()
    result.reserveCapacity(lhs.underestimatedCount * rhs.underestimatedCount)
    for e1 in lhs {
        for e2 in rhs {
            try result.append(contentsOf: transform(e1, e2))
        }
    }
    return result
}

func unzip<A, B, SequenceType: Sequence>(_ sequence: SequenceType) -> (AnySequence<A>, AnySequence<B>) where SequenceType.Element == (A, B) {
    return (sequence.lazy.map{$0.0}.collect(AnySequence.init), sequence.lazy.map{$0.1}.collect(AnySequence.init))
}

func unzip<A, B, SequenceType: Sequence>(_ sequence: SequenceType) -> ([A], [B]) where SequenceType.Element == (A, B) {
    return (sequence.map{$0.0}, sequence.map{$0.1})
}

func assertNonFatal(_ predicate: @autoclosure () -> Bool, _ message: String, file: String = #file, function: String = #function, line: Int = #line) {
#if DEBUG
    if !predicate() {
        print("[WARNING: \(file):\(function):\(line)] \(message)")
    }
#endif
}

extension Sequence {
    func partition(_ isInFirstPartition: (Element) throws -> Bool) rethrows -> ([Element], [Element]){
        return try reduce(into: ([],[])) { (partitions: inout ([Element], [Element]), element: Element) in
            if try isInFirstPartition(element) {
                partitions.0.append(element)
            } else {
                partitions.1.append(element)
            }
        }
    }
}

enum Either<A, B> {
    case first(A)
    case second(B)
}

extension Either {
    func map<ResultA, ResultB>(_ transformFirst: (A) throws -> ResultA, _ transformSecond: (B) throws -> ResultB) rethrows -> Either<ResultA, ResultB> {
        switch self {
        case .first(let a):
            return try .first(transformFirst(a))
            
        case .second(let b):
            return try .second(transformSecond(b))
        }
    }
    
    func combine<Result>(_ transformFirst: (A) throws -> Result, _ transformSecond: (B) throws -> Result) rethrows -> Result {
        switch self {
        case .first(let a):
            return try transformFirst(a)
            
        case .second(let b):
            return try transformSecond(b)
        }
    }
}

#if swift(>=4.1)
#else
extension Sequence {
    func compactMap<ElementOfResult>(_ transform: (Element) throws -> ElementOfResult?) rethrows -> [ElementOfResult] {
        return try flatMap(transform)
    }
}
#endif
//
//  Parser.swift
//  Covfefe
//
//  Created by Palle Klewitz on 19.09.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation


/// A syntax tree with non-terminal keys and string range leafs.
public typealias ParseTree = SyntaxTree<NonTerminal, Range<String.Index>>

/// A parser which can check if a word is in a language
/// and generate a syntax tree explaining how a word was derived from a grammar
public protocol Parser {
    /// Creates a syntax tree which explains how a word was derived from a grammar
    ///
    /// - Parameter string: Input word, for which a parse tree should be generated
    /// - Returns: A syntax tree explaining how the grammar can be used to derive the word described by the given tokenization
    /// - Throws: A syntax error if the word is not in the language recognized by the parser
    func syntaxTree(for string: String) throws -> ParseTree
}

public extension Parser {
    /// Returns true if the recognized language contains the given tokenization.
    ///
    /// - Parameter string: Word for which membership to the recognized grammar should be decided.
    /// - Returns: True, if the word is generated by the grammar, false if not.
    func recognizes(_ string: String) -> Bool {
        return (try? self.syntaxTree(for: string)) != nil
    }
}

/// A parser that can parse ambiguous grammars and retrieve every possible syntax tree
public protocol AmbiguousGrammarParser: Parser {
    /// Generates all syntax trees explaining how a word can be derived from a grammar.
    ///
    /// This function should only be used for ambiguous grammars and if it is necessary to
    /// retrieve all parse trees, as it comes with an additional cost in runtime.
    ///
    /// For unambiguous grammars, this function should return the same results as `syntaxTree(for:)`.
    ///
    /// - Parameter string: Input word, for which all parse trees should be generated
    /// - Returns: All syntax trees which explain how the input was derived from the recognized grammar
    /// - Throws: A syntax error if the word is not in the language recognized by the parser
    func allSyntaxTrees(for string: String) throws -> [ParseTree]
}
//
//  GrammarCheck.swift
//  Covfefe
//
//  Created by Palle Klewitz on 16.08.17.
//  Copyright (c) 2017 Palle Klewitz
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

import Foundation

public extension Grammar {
    
    /// Non-terminals which cannot be reached from the start non-terminal
    var unreachableNonTerminals: Set<NonTerminal> {
        let productionSet = productions.collect(Set.init)
        let reachableProductions = Grammar.eliminateUnusedProductions(productions: productions, start: start).collect(Set.init)
        return productionSet.subtracting(reachableProductions).map{$0.pattern}.collect(Set.init)
    }
    
    /// Nonterminals which can never produce a sequence of terminals
    /// because of infinite recursion.
    var unterminatedNonTerminals: Set<NonTerminal> {
        guard isInChomskyNormalForm else {
            return self.chomskyNormalized().unterminatedNonTerminals
        }
        let nonTerminalProductions = Dictionary(grouping: self.productions, by: {$0.pattern})
        return nonTerminalProductions.filter { _, prod -> Bool in
            return prod.allSatisfy {!$0.isFinal}
        }.keys.collect(Set.init)
    }
}


extension Connector {
    var dialects: [String] {
        var dialects = [rawValue]
        switch self {
        case .n:
            dialects.append("--")
        default:
            break
        }
        return dialects
    }
    var all: String {
        dialects.map({ "'\($0)'" }).joined(separator: "|")
    }
    static func canonical(_ s: String) -> String? {
        allCases.map(\.dialects).first(where: { $0.contains(s) })?.first
    }
}





















































/****************

    NARS-Swift

*****************/


public protocol Item: Equatable {
    var identifier: String { get }
    var priority: Double { get set }
}

public struct TermLink: Item {
    public var identifier: String { term.description }
    public var priority: Double = 0.9
    public let term: Term
}

public struct Belief: Item {
    public var identifier: String { judgement.identifier }
    public var priority: Double = 0.9
    public let judgement: Judgement
}

//public struct Task: Item {
//    public var identifier: String { sentence.description }
//    public var priority: Double = 0.9
//    public let sentence: Sentence
//}

// MARK: Concept

public struct Concept: Item {
    public var identifier: String { term.description }
    public var priority: Double = 0.9
    
    let term: Term
    
    private var _termLinks = Bag<TermLink>()
    internal var termLinks: WrappedBag<TermLink>
    //let tasks = Bag<TermLink>() // sentences
    private var _beliefs = Bag<Belief>()
    internal var beliefs: WrappedBag<Belief>

    init(term: Term) {
        self.term = term
        self.termLinks = WrappedBag(_termLinks)
        self.beliefs = WrappedBag(_beliefs)
    }
    
    // TODO: how much should the input change
    // before it is considered different?
    // for how long should we keep the cache?
    // after n seconds or instances of the same input
    // should we still permit the signal to go through?
    // implementing a debounce of sorts
//    internal var lastInput: Judgement!
//    internal var lastAccepted: Set<Judgement> = []
//    internal var lastQuestion: Question!
//    internal var lastAnswered: Set<Judgement> = []
}

extension Concept {
    // returns derived judgements if any
    func accept(_ j: Judgement, isSubject: Bool = true, derive: Bool) -> [Judgement] {
//        if j == lastInput { return Array(lastAccepted) }
//        lastInput = j

        var originalPriority: Double?
        
        var derived: [Judgement] = []

        var j = j

        // revision goes first
        if let b = beliefs.get(j.identifier) {
            originalPriority = b.priority
            var judgement: Judgement
            if j.evidenceOverlap(b.judgement) {
                judgement = choice(j1: j, j2: b.judgement)
            } else {
                if j.truthValue.rule == .conversion {
                    judgement = b.judgement
                } else if b.judgement.truthValue.rule == .conversion {
                    judgement = j
                } else {
                    judgement = revision(j1: j, j2: b.judgement)
                }
            }
            // wait to put back original belief to process another one
            if j != judgement {
                j = judgement
                derived.append(judgement)
            }
        }
        var jflipped: Judgement = j
        // store symmetrical statement
        if case .statement(let sub, let cop, let pre) = j.statement, (cop == .equivalence || cop == .similarity) {
            let flipped: Statement = .statement(pre, cop, sub)
            jflipped = Judgement(flipped, j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp)
            if beliefs.peek(jflipped.identifier) == nil {
                derived.append(jflipped)
            }
        }

        // store symmetrical compound
        if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
            if terms.count == 2 { // TODO: handle compounds with multiple terms
                let flipped: Statement = .compound(conn, terms.reversed())
                jflipped = Judgement(flipped, j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp)
                if beliefs.peek(jflipped.identifier) == nil {
                    derived.append(jflipped)
                }
            }
        }
        
        defer {
            switch j.statement {
            case .symbol: // TODO: is this accurate?
                termLinks.put(TermLink(j.statement, 0.9))
            case .compound(let c, _):
                if ![.c, .d, .n].contains(c) {
                    termLinks.put(TermLink(j.statement, 0.9))
                }
            case .statement(let subject, _, let predicate):
                if !j.statement.isTautology {
                    let term = isSubject ? predicate : subject
                    termLinks.put(TermLink(term, 0.9))
                }
            case .variable:
                break // TODO: is this accurate?
            case .operation:
                break // TODO: is this accurate?
            }

            var b = j + (originalPriority ?? 0.9)
            b.adjustPriority(derived)
//            if j.truthValue.rule != .conversion {
                beliefs.put(b) // store new belief
//            }
        }
        
        // return if no recursion
        guard derive else { return derived }

        /// apply two-premise rules
        twoPremiseRules:
        if var b = beliefs.get() {

            if b.judgement.statement == jflipped.statement {
                if let b1 = beliefs.get() {
                    beliefs.put(b)
                    b = b1 // use another belief
                } else {
                    beliefs.put(b)
                    break twoPremiseRules
                }
            }

            // apply rules
            let results = Rules.allCases
                .flatMap { r in
                    r.apply((b.judgement, j))
                }
                .compactMap { $0 }

            derived.append(contentsOf: results)
            
            // TODO: wait to put back
            // modify its "usefullness" value
            b.adjustPriority(results)
            beliefs.put(b) // put back another belief

//            lastAccepted = Set(derived)
            if !derived.isEmpty {
//                print("because...")
//                print("+++", j, "\n", "&&", b)
//                print("it follows...")
            }
        }
        
        derived = derived.removeDuplicates().filter {
            beliefs.peek($0.identifier) == nil
        }//&& $0.statement != j.statement }

        // TODO: process `values`
        // like rules but modifiable by the system
        // statements using variables
        
        return derived
    }
    
    
    //
    // TODO: account for tense in question answering
    //
    
    
    // returns relevant belief or derived judgements if any
    func answer(_ q: Question) -> [Judgement] {
        var result: [Judgement] = []
        switch q.statement {
        case .statement(let subject, let copula, let predicate):
            if case .variable(let v) = subject {
                if case .query = v {
                    // special
                    result = answer { s in
                        switch s {
                        case .symbol: fallthrough // TODO: is this accurate?
                        case .compound:
                            return predicate == s
                        case .statement(_, let c, let p):
                            return copula == c && predicate == p
                        case .variable:
                            return false // TODO: is this accurate?
                        case .operation:
                            return false // TODO: is this accurate?
                        }
                    }
                } // TODO: handle other cases
            } else if case .variable(let v) = predicate {
                if case .query = v {
                    // general
                    result = answer { s in
                        switch s {
                        case .symbol: fallthrough // TODO: is this accurate?
                        case .compound:
                            return subject == s
                        case .statement(let s, let c, _):
                            return subject == s && copula == c
                        case .variable:
                            return false // TODO: is this accurate?
                        case .operation:
                            return false // TODO: is this accurate?
                        }
                    }
                }
            } else { // TODO: handle other cases
                result = answer(q.statement)
                result = result.removeDuplicates()
            }
        default:
            return [] // TODO: handle other cases
        }
//        if q == lastQuestion &&
//            Set(result) == lastAnswered {
//            return []
//        }
//        lastQuestion = q
//        lastAnswered = Set(result)
        return result
    }
    
    // MARK: Private
    
    private func answer(_ s: Statement) -> [Judgement] {
        if let b = beliefs.get(s.description) {
            beliefs.put(b) // put back
            return [b.judgement]
        } else if let c = conversion(j1: s-*), let b = beliefs.get(c.statement.description) {
            beliefs.put(b) // put back
            let conv = conversion(j1: b.judgement)!
            beliefs.put(conv + 0.9)
            return [conv]
            
        } else if let b = beliefs.get() {
            beliefs.put(b) // put back
            // all other rules // backwards inference
            
            let r = Theorems.apply(b.judgement)
                .filter { beliefs.peek($0.description) == nil }

            if let answer = r.first(where: { $0.statement == s }) {
                return [answer]
            }
            
            return r +
             Rules.allCases
                .flatMap { r in
                    r.apply((s-*, b.judgement))
                }
                .compactMap { $0 }
        }
        return [] // no results found
    }
    
    private func answer(_ f: (Statement) -> Bool) -> [Judgement] {
        let winner = beliefs.items
            .filter { b in
                f(b.value.judgement.statement)
            }.map { b in
                b.value.judgement
            }.max { j1, j2 in
                let c = choice(j1: j1, j2: j2)
                return c.statement == j2.statement
            }
        return winner == nil ? [] : [winner!]
    }
}

extension AbstractBag where I == Concept {
    func consider(_ s: Sentence, derive: Bool) -> [Judgement] {
        switch s {
        case .judgement(let j): return consider(j, derive: derive)
        case .goal: return [] // TODO: finish implementation
        case .question(let q): return consider(q, derive: derive)
        case .cycle: return []
        }
    }
    func consider(_ j: Judgement, derive: Bool) -> [Judgement] {
        consider(j.statement, derive: derive) { c in
            switch j.statement {
            case .symbol: fallthrough // TODO: is this accurate?
            case .compound:
                return c.accept(j, isSubject: c.term == j.statement, derive: derive)
            case .statement(let subject, _, _):
                return c.accept(j, isSubject: c.term == subject, derive: derive)
            case .variable:
                return [] // TODO: is this accurate?
            case .operation:
                return [] // TODO: is this accurate?
            }
        }
    }
    func consider(_ q: Question, derive: Bool) -> [Judgement] {
        if case .statement = q.statement {
            return consider(q.statement, derive: derive) { c in c.answer(q) }
        } else {
            return considerVar(q.variableTerm, derive: derive) { c in c.answer(q) }
        }
    }
}

// MARK: Private

extension AbstractBag where I == Concept {
    private func consider(_ s: Statement, derive: Bool, _ f: (inout Concept) -> [Judgement]) -> [Judgement] {
        var derivedJudgements = [Judgement]()
        // TODO: consider overall concept
        // let overallConcept = get(s.description) ?? Concept(term: s)
        switch s {
        case .symbol: // TODO: is this accurate?
            var concept = get(s.description) ?? Concept(term: s)
            derivedJudgements.append(contentsOf: f(&concept))
            concept.adjustPriority(derivedJudgements)
            put(concept)
            return derivedJudgements
        case .compound(let c, let ts):
//            if c == .n, ts.count == 1 { // TODO: is this correct?
//                return consider(ts[0], derive: derive, f)
//            }
            if [.c, .d].contains(c) {
                let terms = Set(ts.flatMap{$0.terms})
                for t in terms {
                    if var concept = get(t.description) {
                        derivedJudgements.append(contentsOf: f(&concept))
                        concept.adjustPriority(derivedJudgements)
                        put(concept)
                    }
                }
                return derivedJudgements
            }
            var concept = get(s.description) ?? Concept(term: s)
            derivedJudgements.append(contentsOf: f(&concept))
            concept.adjustPriority(derivedJudgements)
            put(concept)
            return derivedJudgements
        case .statement(let subject, _, let predicate):
            var subjectConcept = get(subject.description) ?? Concept(term: subject)
            var predicateConcept = get(predicate.description) ?? Concept(term: predicate)
            derivedJudgements.append(contentsOf: f(&subjectConcept))

            subjectConcept.adjustPriority(derivedJudgements)

            derivedJudgements.append(contentsOf: f(&predicateConcept))
            predicateConcept.adjustPriority(derivedJudgements)
            if case .statement = subject {
                derivedJudgements.append(contentsOf: consider(subject, derive: derive, f))
            }
            if case .statement = predicate {
                derivedJudgements.append(contentsOf: consider(predicate, derive: derive, f))
            }
            put(subjectConcept)
            put(predicateConcept)
            return derivedJudgements
        case .variable:
            return [] // TODO: is this accurate?
        case .operation:
            return [] // TODO: is this accurate?
        }
    }

    private func considerVar(_ t: Term, derive: Bool, _ f: (inout Concept) -> [Judgement]) -> [Judgement] {
        guard var concept = get(t.description) else { return [] }
        defer { put(concept) } // put back
        return f(&concept)
    }
    
    func contains(_ j: Judgement) -> Bool {
        let identifier = j.identifier
        switch j.statement {
        case .symbol(let word):
            if let c = peek(word) {
                return c.beliefs.peek(identifier) != nil
            }
        case .compound(let c, let ts):
            if c == .n, ts.count == 1 {
                return contains(ts[0]-*)
            }
//            return !ts.map { contains($0-*) }.contains(false) // TODO: finish implementation
        case .statement(let s, _, let p):
            if let sc = peek(s.description), let pc = peek(p.description) {
                return sc.beliefs.peek(identifier) != nil && pc.beliefs.peek(identifier) != nil
            }
        case .variable(_):
            return false // TODO: finish implementation
        case .operation(_, _):
            return false // TODO: finish implementation
        }
        return false
    }
}

//import Dispatch

public protocol AbstractBag {
    associatedtype I: Item
    @discardableResult
    func put(_ item: I) -> I?
    func get() -> I?
    func get(_ identifier: String) -> I?
    func peek() -> I?
    func peek(_ identifier: String) -> I?
}


class SyncQueue {
   var writing = false
   func sync<I>(_ block: () -> I) -> I {
       while writing { /*wait*/ }
       writing = true
       defer { writing = false }
       return block()
   }
   func sync(_ block: () -> Void) {
       while writing { /*wait*/ }
       writing = true
       defer { writing = false }
       block()
   }
}

public final class Bag<I: Item>: AbstractBag {
    var buckets: [[I]]
    var items: [String: I] = [:]
    
    internal let levels: Int
    internal let capacity: Int
    internal var currentLevel: Int
    
   internal let queue = SyncQueue()//DispatchQueue(label: "ioqueue", qos: .background)
    
    public init(_ levels: Int = 100, _ capacity: Int = 10000) {
        buckets = Array(repeating: [], count: levels)
        self.levels = levels
        self.capacity = capacity
        self.currentLevel = levels - 1
    }
    
    @discardableResult
    public func put(_ item: I) -> I? {
       queue.sync {
            var item = item
            let oldItem = items[item.identifier]
            if let oldItem = oldItem {
                item.priority = max(oldItem.priority, item.priority)
                removeFromBucket(oldItem)
            }
            items[item.identifier] = item
            return addToBucket(item)
       }
    }
    
    public func get() -> I? {
       queue.sync {
            if items.isEmpty {
                return nil
            }
            currentLevel = selectNonEmptyLevel()
            if buckets[currentLevel].isEmpty {
                return nil
            }
            let item = buckets[currentLevel].removeFirst()
            items.removeValue(forKey: item.identifier)
            return item
       }
    }
    
    public func get(_ identifier: String) -> I? {
       queue.sync {
            if let item = items[identifier] {
                removeFromBucket(item)
                items.removeValue(forKey: item.identifier)
                return item
            }
            return nil
       }
    }
    
    public func peek() -> I? {
       queue.sync {
            if items.isEmpty {
                return nil
            }
            currentLevel = selectNonEmptyLevel()
            return buckets[currentLevel].first
       }
    }
    
    public func peek(_ identifier: String) -> I? {
       queue.sync {
            return items[identifier]
       }
    }

    private func getLevel(_ item: I) -> Int {
        let fl = item.priority * Double(buckets.count)
        let level = Int(fl.rounded(.down) - 1)
        return max(level, 0)
    }
    
    // TODO: add probabilistic distributor from OpenNARS
    private func selectNonEmptyLevel() -> Int {
        var selectedLevel = currentLevel
        var cache = Array(0..<levels)
        while buckets[selectedLevel].isEmpty && cache.count > 0 {
            /// https://stackoverflow.com/a/27541537
            let randomKey = Int.random(in: 0..<cache.count)
            selectedLevel = cache[randomKey]
            cache.swapAt(randomKey, cache.count - 1)
            cache.removeLast()
        }
        return selectedLevel
    }
    
    private func addToBucket(_ item: I) -> I? {
        var oldItem: I?
        if items.count > capacity {
            var level = 0
            while buckets[level].isEmpty, level < levels {
                level += 1
            }
            oldItem = buckets[level].removeFirst()
            if let removed = oldItem {
                items.removeValue(forKey: removed.identifier)
            }
        }
        let level = getLevel(item)
        buckets[level].append(item)
        return oldItem
    }
    
    private func removeFromBucket(_ item: I) {
        let level = getLevel(item)
        var items = buckets[level]
        items.removeAll(where: { $0.identifier == item.identifier })
        buckets[level] = items
    }
}


// MARK: - WrappedBag

/// Read access to wrapped Bag with writes to internal bag
public final class WrappedBag<I: Item>: AbstractBag {
    weak var wrapped: Bag<I>?
    var bag = Bag<I>()
    
   internal var queue = SyncQueue()//DispatchQueue(label: "wrappedqueue-\(I.self)", qos: .background)

    init(_ bag: Bag<I>) {
        wrapped = bag
    }
    
    public func reset() {
       queue.sync {
            bag = Bag<I>()
       }
    }
    
    @discardableResult
    public func put(_ item: I) -> I? {
       queue.sync {
            if item != wrapped?.peek(item.identifier) {
                bag.put(item) // items have diverged
            }
            return nil
       }
    }
    
    public func get() -> I? {
       queue.sync {
            bag.get() ?? wrapped?.peek()
       }
    }
    
    public func get(_ identifier: String) -> I? {
       queue.sync {
            bag.get(identifier) ?? wrapped?.peek(identifier)
       }
    }
    
    public func peek() -> I? {
       queue.sync {
            bag.peek() ?? wrapped?.peek()
       }
    }
    
    public func peek(_ identifier: String) -> I? {
       queue.sync {
            bag.peek(identifier) ?? wrapped?.peek(identifier)
       }
    }
}
//#if os(Linux)
//  import Glibc
//  typealias dispatch_time_t = UInt32
//#elseif os(Windows)
//  import WinSDK
//  typealias dispatch_time_t = UInt32
//  typealias useconds_t = UInt32
//#endif

//import Dispatch

public enum Sentence {
    case judgement(Judgement)
    case goal(Goal)
    case question(Question)
    
//    case pause(Int)
    case cycle(Int)
}

extension Sentence {
    /// default wait time in milliseconds (0.001s)
    /// neurons spike between 5ms and 1000ms
//    public static var pause: Sentence { .pause(defaultPause) }
    public static var defaultPause = 1000
    
    public static var cycle: Sentence { .cycle(1) }
}

public final class NARS: Item {
    public var identifier: String { name.description }
    public var priority: Double = 0.9
    
    public var name = Term.symbol("SELF") // TODO: inject via init
    
    public internal(set) var recent = Bag<Belief>(4,40) // TODO: use tense and therefore identifier for indexing
    public internal(set) var memory = Bag<Concept>()
    public internal(set) lazy var imagination = WrappedBag(memory)
    
    public var output: (String) -> Void
    
//    private var queue = DispatchQueue(label: "input", qos: .userInitiated)
//    private var iqueue = DispatchQueue(label: "imagination", qos: .userInitiated)
//    private var cycleQueue = DispatchQueue(label: "cycle", qos: .utility)
    
//    private var thinking = false

//    public var pendingTasks = Bag<Task>()
//    private var lastQuestion: Statement?

//    lazy var cycleItem = DispatchWorkItem { [weak self] in
//        while true {
//            guard let s = self, s.cycle else { continue }
//
//            let quietTime = s.lastPerformance.rawValue - DispatchWallTime.now().rawValue
//
//            // TODO: potentially get items from recent memory and process them in imagination
//            if quietTime > s.cycleLength, let c = s.imagination.get(), let b = c.beliefs.get() {
//
//                c.beliefs.put(b)
//                s.imagination.put(c)
//
//                let immediate = Rules.immediate(b.judgement)
//                let structural = Theorems.apply(b.judgement)
//
//                let results = (immediate + structural).filter { !s.imagination.contains($0) }
//
//                results.forEach { j in
//                    s.process(.judgement(j))
//                }
//            }
//        }
//    }
    
//    public var cycle = false {
//        didSet {
//            if cycle {
//                cycleQueue.resume()
//            } else {
//                cycleQueue.suspend()
//            }
//        }
//    }
    
//    private var factor = 4 // cycles per pause // TODO: dynamically adjust per system
//    fileprivate lazy var cycleLength = (Sentence.defaultPause / factor) * 1000000 // 0.001 second
    
//    fileprivate var lastPerformance = DispatchWallTime.now()
    
    let timeProviderMs: () -> UInt32
    
    public init(timeProviderMs: @escaping () -> UInt32, _ output: @escaping (String) -> Void = { print($0) }) {
        self.output = output
        self.timeProviderMs = timeProviderMs
//        if !cycle {
//            cycleQueue.suspend()
//        }
//        cycleQueue.async(execute: cycleItem)
    }
    
    public func reset() {
//        thinking = false
//        cycleQueue.suspend()
        inputBuffer.removeAll()
        recentBuffer.removeAll()
        imaginationBuffer.removeAll()
        
        memory = Bag<Concept>()
        imagination = WrappedBag(memory)
        recent = Bag<Belief>(4,40)
//        cycleQueue.resume()
//        _sleep(2)
    }
    
    public func perform(_ script: Sentence...) { // convenience
//        perform(script)
        inputBuffer.insert(contentsOf: script.reversed(), at: 0)
        for _ in 0 ..< script.count {
            inputCycle()
            // recentCycle()
        }
    }
    
    var inputBuffer: [Sentence] = []
    
    func inputCycle() {
        guard var s = inputBuffer.popLast() else {
            return
        }
        
        /// JUDGEMENT
        if case .judgement(let j) = s {
            // set time stamp if not yet set
            if j.timestamp == 0 {
                s = .judgement(.updateTimestamp(j, timeProviderMs))
            }
            // process in recent memory
            recentBuffer.insert(j, at: 0)
        }
        
        //
        // TODO: account for tense in question answering
        //
        
        /// QUESTION
        if case .question(let q) = s, case .statement(let sub, _, _) = q.statement {
            // check recent memory, then imagination
            if let answer = self.recent.peek(q.identifier)?.judgement // OR
                ?? self.imagination.consider(q, derive: false).first(where: { $0.statement == q.statement }) {
                // check main memory if the answer is already present
                if let c = self.memory.items[sub.description] {
                    if c.beliefs.items.contains(where: { $0.value.judgement.statement == answer.statement }) == false {
                        /// ANSWER
                        self.process(.judgement(answer), recurse: false, userInitiated: true)
                    }
                } else {
                    /// ANSWER
                    self.process(.judgement(answer), recurse: false, userInitiated: true)
               }
            }
        }
    
        /// SENTENCE
        self.process(s, userInitiated: true) // process in main memory
        
        
        /// PAUSE
//        if case .pause(let t) = s {
//            snooze(t)
//        }
        
        /// CYCLE
        if case .cycle(let n) = s {
//            thinking = true
            for _ in 0 ..< n {
                if imaginationBuffer.isEmpty {
                    mainCycle()
                } else {
                    imaginationCycle()
                }
            }
//            thinking = false
//            if cycle == false {
//                cycle = true
//                think(n * Sentence.defaultPause)
//                cycle = false
//            } else {
//                think(n * Sentence.defaultPause)
//            }
        }
    }
    
    func mainCycle() {
        // TODO: potentially get items from recent memory and process them in imagination
        //        if let r = recent.get() {
        //            recent.put(r)
        //            self.process(.judgement(b.judgement))
        //        }

        if let c = self.imagination.get(), let b = c.beliefs.get() {
            c.beliefs.put(b)
            self.imagination.put(c)
            
            let immediate = Rules.immediate(b.judgement)
            let structural = Theorems.apply(b.judgement)
            
            let results = (immediate + structural).filter { !self.imagination.contains($0) }

            results.forEach { j in
                self.process(.judgement(j))
            }
        }
    }
    
    var recentBuffer: [Judgement] = []
    
    func recentCycle() {
        guard let j = recentBuffer.popLast() else {
            return
        }
        let recent = self.process(recent: j)
        for el in recent { // add stable patterns from recent memory
            self.process(.judgement(el), recurse: false, userInitiated: true)
        }
    }
    
    var imaginationBuffer: [Sentence] = []
    
    func imaginationCycle() {
        guard let s = imaginationBuffer.popLast() else {
            return
        }
        self.process(s)
    }
    
    
//    public func perform(_ script: [Sentence]) {
//        // TODO: add buffer
//        script.forEach { s in
//
//            /// PROCESS
//            self.queue.async { // default processing queue
//
//                var s = s // for updating timstamp
//
////                var recent: [Judgement] = []
//
//                /// JUDGEMENT
//                if case .judgement(let j) = s {
//                    // set time stamp if not yet set
//                    if j.timestamp == 0 {
//                        s = .judgement(.updateTimestamp(j))
//                    }
//                    // process in recent memory
//                    DispatchQueue.global().async {
//                        let recent = self.process(recent: j)
//                        for el in recent { // add stable patterns from recent memory
//                            self.process(.judgement(el), recurse: false, userInitiated: true)
//                        }
//                    }
//
//                //
//                // TODO: account for tense in question answering
//                //
//
//                /// QUESTION
//                } else if case .question(let q) = s, case .statement(let sub, _, _) = q.statement {
//                    // check recent memory, then imagination
//                    if let answer = self.recent.peek(q.identifier)?.judgement // OR
//                        ?? self.imagination.consider(q, derive: false).first(where: { $0.statement == q.statement }) {
//                        // check main memory if the answer is already present
//                        if let c = self.memory.items[sub.description] {
//                            if c.beliefs.items.contains(where: { $0.value.judgement.statement == answer.statement }) == false {
//                                /// ANSWER
//                                self.process(.judgement(answer), recurse: false, userInitiated: true)
//                            }
//                        } else {
//                            /// ANSWER
//                            self.process(.judgement(answer), recurse: false, userInitiated: true)
//                       }
//                    }
//                }
//
//                /// SENTENCE
//                self.process(s, userInitiated: true) // process in main memory
//            }
//
//            /// PAUSE
//            if case .pause(let t) = s {
//                snooze(t)
//            }
//
//            /// CYCLE
//            if case .cycle(let n) = s {
//                if cycle == false {
//                    cycle = true
//                    think(n * Sentence.defaultPause)
//                    cycle = false
//                } else {
//                    think(n * Sentence.defaultPause)
//                }
//            }
//        }
//
//    }
//    func snooze(_ t: Int) {
//        thinking = true
//        let ms = 1000 // millisecond
//        _usleep(useconds_t(t * ms))
//        thinking = false
//    }
    
//    func think(_ t: Int) {
//        thinking = true
//        let deadline = dispatch_time_t(t) * 1000 * 1000
//        let start = DispatchWallTime.now().rawValue
//        while thinking, deadline > (start - DispatchWallTime.now().rawValue) {
//            /// cycle
//        }
//        thinking = false
//    }
    
    // TODO: remove -- this was for temporary profiling
//    public var lastCycle: [(UInt32,String)] = []
//    private var lastInput: Sentence!
}

// MARK: Private

extension NARS {
    fileprivate func process(recent j: Judgement) -> [Judgement] {
        guard recent.peek(j.identifier) == nil else {
//            print("}}", j)
            return []// no need to process what we already know
        }
//        print(">", j)
        
        var derived: [Belief] = [j + 0.9]
        
        Theorems.apply(j).forEach {
            if recent.peek($0.identifier) == nil {
                recent.put($0 + 0.9)
            }
        }
        
//        print("D", derived)
//        print("R", recent)
        var stable: [Judgement] = []
        
        while let b = recent.get() {
            derived.append(b)
//            print("L", b, j)
            // process temporal
            if b.judgement.truthValue.rule == nil, b.judgement.timestamp != ETERNAL, j.timestamp != ETERNAL {
//                print("K", b.judgement, j)
                // only process direct experiences
                Rules.allCases.flatMap { rs in
                    rs.variable_and_temporal.flatMap { r in
                        [rule_generator(r)((j, b.judgement)),
                         rule_generator(r)((b.judgement, j))] // switch order of premises
                    }
                }.forEach {
                    if var el = $0 {
//                        print(">>--", el)
//                        // set time stamp if not yet set
//                        if el.timestamp == 0 {
//                            let now = DispatchWallTime.now()
//                            if el.derivationPath.count == 1 { // also update derivationPath
//                                el = el.statement + (el.truthValue.f, el.truthValue.c, now.rawValue)
//                            } else {
//                                el.timestamp = now.rawValue
//                            }
//                        }
                        foo()
                        if el.tense != nil {
                            let tv = el.truthValue
                            let elc = tv.c / (tv.c + k)
//                            print("KK", el, el.derivationPath)
                            el = Judgement(el.statement, TruthValue(tv.f, elc, el.truthValue.rule), el.derivationPath, tense: nil, timestamp: ETERNAL)
                            foo()
                            
                            // add to main memory
                            // TODO: figure out how to accomplish evidence accumulation
                            // because as it stands, there is evidence overlap
                            // so choice rule will be used instead of revision
//                            process(.judgement(el), recurse: false, userInitiated: true)
                            stable.append(el)
                        }
                        
                        func foo() {
                            if let d = derived.first(where: { $0.judgement.identifier == el.identifier }) {
                                el = choice(j1: d.judgement, j2: el)
                            }
                            derived.append(el + 0.9)
                        }
                    }
                }
            }
            
            if b.judgement.statement != j.statement {
                
                Rules.allCases.flatMap { r in
                    r.apply((b.judgement, j))
                }.forEach {
                    if let el = $0 {
                        //                    if el.timestamp == 0 {
                        //                        let now = DispatchWallTime.now()
                        //                        if el.derivationPath.count == 1 { // also update derivationPath
                        //                            el = el.statement + (el.truthValue.f, el.truthValue.c, now.rawValue)
                        //                        } else {
                        //                            el.timestamp = now.rawValue
                        //                        }
                        //                    }
                        if let d = derived.first(where: { $0.judgement.identifier == el.identifier }) {
                            derived.append(choice(j1: d.judgement, j2: el) + 0.9)
                        } else {
                            derived.append(el + 0.9)
                        }
                    }
                }
                
            }
        }
        
        derived.forEach {
            recent.put($0)
        }
        
        return stable
    }
    
    fileprivate func process(_ input: Sentence, recurse: Bool = true, userInitiated: Bool = false) {
//        var recurse = recurse; recurse = true
//        let now = DispatchWallTime.now()
//        if lastCycle.count == 1 && lastCycle[0] == (0,"") {
//            lastCycle.remove(at: 0) // first cycle
//        }
//        let duration = lastInput == nil ? 0 : lastPerformance.rawValue - now.rawValue
        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + " \(input)"
//        lastCycle.append((duration, label))
        
//        lastInput = input
//        lastPerformance = now
            
        var input = input // set time stamp if not yet set
        if case .judgement(let j) = input, j.timestamp == 0 {
            input = .judgement(.updateTimestamp(j, timeProviderMs))
        }

        output(label)

        // memory or imagination
        let derivedJudgements: [Judgement] = {
            var derivedJudgements: [Judgement]
            if userInitiated {
                derivedJudgements = memory.consider(input, derive: recurse)
            } else {
                derivedJudgements = imagination.consider(input, derive: recurse)
            }
            derivedJudgements = derivedJudgements.filter({ j in
                if j.truthValue.confidence == 0 {
                    return false
                }
                if case .judgement(let judgement) = input, j == judgement || judgement.statement.isTautology {
                    return false
                }
                return true
            })
            derivedJudgements = Array(Set(derivedJudgements)) //TODO: use choice to additionally resolve duplicates
//        print(derivedJudgements)
//            print("processed \(input)\n\tderived \(derivedJudgements)")
            return derivedJudgements
        }()
        if derivedJudgements.isEmpty {
            if case .question = input {
//                output("\t(1)I don't know 🤷‍♂️")
                if userInitiated == false {
                    self.imagination.reset() //= self.memory.copy()
                }

//                if thinking {
                    output("thinking... \(input)")

//                    iqueue.async {
//                        // re-process question
//                        self.process(input)
//                    }
                    imaginationBuffer.insert(input, at: 0)
//                }
            }
            return
        }
        
        // helper
//        func imagine(recurse r: Bool = true) {
//            //print("dj \(derivedJudgements)")
//            derivedJudgements.forEach { j in
////                if thinking || cycle {
//                    process(.judgement(j), recurse: r)
////                }
//            }
//        }
        
        switch input {
        
        case .judgement:
            //  consider a judgement
            if !recurse { break } // return if no recursion is needed
            
            if userInitiated {
                derivedJudgements.forEach { j in
                    process(.judgement(j),
                            recurse: false, // determines if derived judgements are inserted
                            userInitiated: true) // will cause insertion into main memory
                }
            } else {
                let js: [Sentence] = derivedJudgements.reversed().map({.judgement($0)})
                imaginationBuffer.insert(contentsOf: js, at: 0)
//                imagine(recurse: false)
            }
            
        case .goal:
            break // TODO: finish implementation
        
        case .question(let question):
            /// consider a question
            if case .statement(let s, _, let p) = question.statement {
                if case .variable = s {
                    if let winner = derivedJudgements.first {
//                        let duration = lastPerformance.rawValue - DispatchWallTime.now().rawValue
                        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + "💡 \(winner)"
//                        lastCycle.append((duration, label))
                        output(".  💡 \(winner)")
                    } else {
                        output("\t(2)I don't know 🤷‍♂️")
                    }
                } else if case .variable = p {
                    if let winner = derivedJudgements.first {
//                        let duration = lastPerformance.rawValue - DispatchWallTime.now().rawValue
                        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + "💡 \(winner)"
//                        lastCycle.append((duration, label))
                        output(".  💡 \(winner)")
                    } else {
                        output("\t(2)I don't know 🤷‍♂️")
                    }
                } else if let winner = derivedJudgements.first, winner.statement == question.statement {
                    
                    if !userInitiated {
                        // cancel all in-flight activities
//                        thinking = false
                        
                        imaginationBuffer.removeAll()
                        
                        // process winning judgement
                        process(.judgement(winner),
                                     recurse: false, // determines if derived judgements are inserted
                                     userInitiated: true) // will cause insertion into main memory
                    } else {
//                        let duration = lastPerformance.rawValue - DispatchWallTime.now().rawValue
                        let label = (userInitiated ? "•" : ".") + (recurse && userInitiated ? "" : "  ⏱") + "💡 \(winner)"
//                        lastCycle.append((duration, label))
                    }
                    output(".  💡 \(winner)")
                    print("}}", winner.derivationPath)
                    break
                    
                } else if recurse { // switch to imagination flow
//                    if userInitiated && !thinking {
//                        iqueue.sync {
//                            self.imagination.reset() //= self.memory.copy()
//                        }
//                        self.thinking = true
//                    }
                    
//                    iqueue.async {
//                        imagine()
//                        // re-process question
//                        self.process(.question(question))
//                    }
                    
                    let js: [Sentence] = derivedJudgements.reversed().flatMap({[.question(question), .judgement($0)]})
                    imaginationBuffer.insert(contentsOf: js, at: 0)
                    
                } else {
                    output("\t(2)I don't know 🤷‍♂️")
                }
            }
        case .cycle:
            break // do nothing
        }
    }
}


// MARK: - Compatibility

//func _sleep(_ t: UInt32) {
//    #if os(Windows)
//      Sleep(t * 1000)
//    #else
//      sleep(t)
//    #endif
//}
//
//func _usleep(_ t: UInt32) {
//    #if os(Windows)
//      Sleep(t)
//    #else
//      usleep(t)
//    #endif
//}


// convenience initializer for Belief
public func +(_ j: Judgement, p: Double) -> Belief {
    Belief(j, p)
}


public func -*(_ s: Statement, _ fc: (Double, Double)) -> Judgement {
    s -* (fc.0, fc.1, ETERNAL)
}
public func -*(_ s: Statement, _ fc: (Double, Double)) -> Sentence {
    Sentence(s -* fc)
}
public func -*(_ s: Statement, _ fct: (Double, Double, UInt32)) -> Sentence {
    Sentence(s -* fct)
}
public func -*(_ s: Statement, _ f: Double) -> Sentence {
    Sentence(s -* (f, 0.9))
}
public func -*(_ s: Statement, _ t: UInt32) -> Sentence {
    Sentence(s -* (1, 0.9, t))
}

extension Statement {
    public static postfix func -*(_ s: Statement) -> Sentence {
        Sentence(s-*)
    }
}


extension Sentence {
    public static prefix func <<(_ s: Sentence) -> Sentence {
        s.addTense(.past)
    }
    public static prefix func ||(_ s: Sentence) -> Sentence {
        s.addTense(.present)
    }
    public static prefix func >>(_ s: Sentence) -> Sentence {
        s.addTense(.future)
    }
    
    private func addTense(_ tense: Tense) -> Sentence {
        switch self {
        case .judgement(let j):
            return .judgement(Judgement(j.statement, j.truthValue, j.derivationPath, tense: tense, timestamp: j.timestamp))
        case .question(let q):
            return .question(Question(q.statement, q.type, tense))
        default:
            return self
        }
    }
}

postfix operator -?
extension Statement {
    public static postfix func -?(_ s: Statement) -> Question { Question(s) }
    public static postfix func -?(_ s: Statement) -> Sentence { Sentence(s-?) }
}

postfix operator -!
extension Statement {
    public static postfix func -!(_ s: Statement) -> Goal { Goal(s) }
    public static postfix func -!(_ s: Statement) -> Sentence { Sentence(s-!) }
}

extension Item {
    // TODO: this needs to be handled properly
    mutating func adjustPriority(_ derivedJudgements: [Judgement]) {
        if let maxPriority = derivedJudgements.map({$0.truthValue.confidence}).max() {
            let newPriority = (priority + maxPriority) / 2
            priority = min(newPriority, 0.9)
        }
    }
}
//import Dispatch

extension Judgement {
    static func updateTimestamp(_ j: Judgement, _ timeProvider: () -> UInt32) -> Judgement {
        var j = j
        let now = timeProvider()
        if j.derivationPath.count == 1 { // also update derivationPath
            return Judgement(j.statement, j.truthValue, tense: j.tense, timestamp: now)
        } else {
            j.timestamp = now
            return j
        }
    }
}
//#if SWIFT_PACKAGE
//@_exported import NAL
//#endif

extension Question {
    public init(_ f: @autoclosure () -> Statement) {
        self.init(f(), Quest.truth, nil)
    }
    public var variableTerm: Term! {
        if case .statement(let s, _ , let p) = statement {
            if case .variable = s {
                return s
            } else if case .variable = p {
                return p
            } else {
                return nil
            }
        }
        return nil
    }
}

extension Goal {
    public init(_ f: @autoclosure () -> Statement) {
        let desireValue = DesireValue(1.0, 0.9) // TODO: what is the correct default?
        self.init(f(), desireValue)
    }
}

extension Sentence {
    public init(_ q: Question) {
        self = .question(q)
    }
    public init(_ j: Judgement) {
        self = .judgement(j)
    }
    public init(_ g: Goal) {
        self = .goal(g)
    }
}

extension TermLink {
    public init(_ term: Term, _ priority: Double) {
        self.term = term
        self.priority = priority
    }
}

extension Belief {
    public init(_ judgement: Judgement, _ priority: Double) {
        self.judgement = judgement
        self.priority = priority
    }
}

// MARK: Equatable

extension Bag: Equatable {
    public static func == (lhs: Bag<I>, rhs: Bag<I>) -> Bool {
        Set(lhs.items.keys) == Set(rhs.items.keys)
    }
}

extension WrappedBag: Equatable {
    public static func == (lhs: WrappedBag<I>, rhs: WrappedBag<I>) -> Bool {
        lhs.bag == rhs.bag && lhs.wrapped == rhs.wrapped
    }
}

extension Concept: Equatable {
    public static func == (lhs: Concept, rhs: Concept) -> Bool {
        lhs.term == rhs.term
        && lhs.termLinks == rhs.termLinks
        && lhs.beliefs == rhs.beliefs
    }
}

extension NARS: Equatable {
    public static func == (lhs: NARS, rhs: NARS) -> Bool {
        lhs.name == rhs.name
    }
}


/// Convenience

extension WrappedBag where I == Belief {
    /// convenience for iterating over both dictionaries
    var items: [String : I] { bag.items.merging(wrapped?.items ?? [:], uniquingKeysWith: max)}
}

extension Belief: Comparable {
    public static func < (lhs: Belief, rhs: Belief) -> Bool {
        let c = choice(j1: lhs.judgement, j2: rhs.judgement)
        return c.statement == rhs.judgement.statement
    }
}

// MARK: CustomStringConvertible

extension Concept: CustomStringConvertible {
    public var description: String {
        "\(term)".uppercased() + "\n.  \(termLinks)" + ".  \(beliefs)"
    }
}

extension TermLink: CustomStringConvertible {
    public var description: String {
        identifier + " \(priority)"
    }
}

extension Belief: CustomStringConvertible {
    public var description: String {
        "\(judgement)"
    }
}

extension Sentence: CustomStringConvertible {
    public var description: String {
        switch self {
        case .judgement(let judgement):
            return "\(judgement)"
        case .goal(let goal):
            return "\(goal)"
        case .question(let question):
            return "\(question)"
//        case .pause(let t):
//            return "💤 \(Double(t)/1000) seconds"
        case .cycle(let n):
            return "🔄 \(n) cycles"
        }
    }
}

extension Bag: CustomStringConvertible {
    public var description: String {
        let x = I.self == Concept.self ? "" : ".  "
        let o = items.values.reduce("", { $0 + "\($1)\n" + x })
        return String(o.dropLast(x.count))
    }
}

extension WrappedBag: CustomStringConvertible {
    public var description: String {
        let b = "\(bag)"
        let w = wrapped == nil ? "" : "\(wrapped!)"
        return b + "\n---\n" + w
    }
}


/// Local
///
// TODO: handle variables properly
// independent #x can be merged with independent #y
//
public func revision(j1: Judgement, j2: Judgement) -> Judgement {
    let (f1, c1) = (j1.truthValue.f, j1.truthValue.c)
    let (f2, c2) = (j2.truthValue.f, j2.truthValue.c)
    let f = ((f1 * c1) * (1 - c2) + (f2 * c2) * (1 - c1)) / (c1 * (1 - c2) + c2 * (1 - c1))
    let c = (c1 * (1 - c2) + c2 * (1 - c1)) / (c1 * (1 - c2) + c2 * (1 - c1) + (1 - c1) * (1 - c2))
    return Judgement(j1.statement, TruthValue(f, c, .revision), Judgement.mergeEvidence(j1, j2))
}

public func choice(j1: Judgement, j2: Judgement) -> Judgement {
    j1.statement == j2.statement ?
        (j1.truthValue.c > j2.truthValue.c) ? j1 : j2
    :
        (and(j1.truthValue.e, j1.statement.simplicity)
            >
            and(j2.truthValue.e, j2.statement.simplicity)) ? j1 : j2
}

/// Immediate

public func negation(j1: Judgement) -> Judgement {
    let f = 1 - j1.truthValue.f
    let c = j1.truthValue.c
    let cs = neg(j1.statement)
    let cj = cs + (f, c, ETERNAL)
    return Judgement(cs, TruthValue(f, c, .negation), Judgement.mergeEvidence(j1, cj), tense: j1.tense, timestamp: j1.timestamp)
}

public func conversion(j1: Judgement) -> Judgement? {
    guard case .statement(let s, let copula, let p) = j1.statement,
          copula == .inheritance || copula == .implication else {
        return nil // invalid statement
    }
    let (f, c) = (j1.truthValue.f, j1.truthValue.c)
    let c1 = f * c / (f * c + k)
    let cs = Term.statement(p, copula, s)
    let cj = cs + (1, c1, ETERNAL)
    return Judgement(cs, TruthValue(1, c1, .conversion), Judgement.mergeEvidence(j1, cj), tense: j1.tense, timestamp: j1.timestamp)
}

public func contraposition(j1: Judgement) -> Judgement? {
    guard case .statement(let s, let copula, let p) = j1.statement,
          copula == .implication else {
        return nil // invalid statement
    }
    let (f, c) = (j1.truthValue.f, j1.truthValue.c)
    let c1 = (1 - f) * c / ((1 - f) * (c + k))
    let cs = neg(p) => neg(s)
    let cj = cs + (0, c1, ETERNAL)
    return Judgement(cs, TruthValue(0, c1, .contraposition), Judgement.mergeEvidence(j1, cj), tense: j1.tense, timestamp: j1.timestamp)
}

private func neg(_ s: Statement) -> Statement {
    if case .compound(let conn, let terms) = s, conn == .n, terms.count == 1 {
        return terms[0] // double negative
    } else {
        return .compound(.n, [s])
    }
}

public extension Rules {
    var allRules: [Rule] {
        let rules = firstOrder + higherOrder + compositional + conditionalSyllogistic
        var permutations: [Rule] = []
        for r in rules {
            let (p1, p2, c, tf) = r
            var sp1: Statement!
            var sp2: Statement!
            if case .statement(let s, let copula, let p) = p1 {
                if copula == .similarity || copula == .equivalence {
                    sp1 = .statement(p, copula, s)
                }
            }
            if case .statement(let s, let copula, let p) = p2 {
                if copula == .similarity || copula == .equivalence {
                    sp2 = .statement(p, copula, s)
                }
            }
            if sp1 != nil {
                permutations.append((sp1, p2, c, tf))
            }
            if sp2 != nil {
                permutations.append((p1, sp2, c, tf))
            }
            if sp1 != nil && sp2 != nil {
                permutations.append((sp1, sp2, c, tf))
            }
        }
        return rules + permutations
    }

    var higherOrder: [Rule] {
        return firstOrder.map { (arg) in
            var (p1, p2, c, tf) = arg
            p1 = replaceCopulas(p1)
            p2 = replaceCopulas(p2)
            c = replaceCopulas(c)
            return (p1, p2, c, tf)
        }
    }
    
    var firstOrder: [Rule] {
        let S = Term.var("S")
        let P = Term.var("P")
        let M = Term.var("M")

        switch self {
        case .deduction:
            return [(M --> P,     S --> M, S --> P, tf),
                    (P --> M,     M --> S, P --> S, tfi)]
        case .induction:
            return [(M --> P,     M --> S, S --> P, tf),
                    (M --> P,     M --> S, P --> S, tfi)]
        case .abduction:
            return [(P --> M,     S --> M, S --> P, tf),
                    (P --> M,     S --> M, P --> S, tfi)]
        case .exemplification:
            return [(P --> M,     M --> S, S --> P, tf),
                    (M --> P,     S --> M, P --> S, tfi)]
        case .comparison:
            return [(M --> P,     M --> S, S <-> P, tf),
                    (P --> M,     S --> M, S <-> P, tfi)]
        case .analogy:
            return [(M --> P,     S <-> M, S --> P, tf),
                    (P --> M,     S <-> M, P --> S, tf),
                    (M <-> P,     S --> M, S --> P, tfi),
                    (M <-> P,     M --> S, P --> S, tfi)]
        case .resemblance:
            return [(M <-> P,     S <-> M, S <-> P, tf)]
            
        default:
            return [] // other rules are handled separately
        }
    }
    
    var compositional: [Rule] {
        let M = Term.var("M")
        let T1 = Term.var("T1")
        let T2 = Term.var("T2")
        
        switch self {
        case .intersection:
            return [ /// first order
                (M --> T1,    M --> T2,    M --> (T1 & T2), tf),
                (T1 --> M,    T2 --> M,    (T1 | T2) --> M, tf),
                /// higher order
                ( M => T1,    M => T2 ,    M => (T1 && T2), tf),
                ( T1 => M,    T2 => M ,    (T1 || T2) --> M, tf),
                /// conditional
                (      T1,          T2,    (T1 && T2), tf) // TODO: verify nothing else needs to be checked
            ]
        case .union:
            return [ /// first order
                (M --> T1,    M --> T2,    M --> (T1 | T2), tf),
                (T1 --> M,    T2 --> M,    (T1 & T2) --> M, tf),
                /// higher order
                ( M => T1,    M => T2 ,    M => (T1 || T2), tf),
                ( T1 => M,    T2 => M ,    (T1 && T2) --> M, tf),
                /// conditional
                (      T1,          T2,    (T1 || T2), tf) // TODO: verify nothing else needs to be checked
            ]
        case .difference:
            return [
                (M --> T1,    M --> T2,    M --> (T1 - T2), tf),
                (M --> T1,    M --> T2,    M --> (T2 - T1), tfi),
                (T1 --> M,    T2 --> M,    (T1 ~ T2) --> M, tf),
                (T1 --> M,    T2 --> M,    (T2 ~ T1) --> M, tfi)
            ]
        default:
            return []
        }
    }
    
    var conditionalSyllogistic: [Rule] {
        let S = Term.var("S")
        let P = Term.var("P")
        switch self {
        case .deduction:
            return [(S  => P,           S,       P, tf)]
        case .abduction:
            return [(P  => S,           S,       P, tf)]
        case .analogy:
            return [(      S,     S <=> P,       P, tf)]
        default:
            return []
        }
    }
    
    /// special set of rules handled separately during inference
    /// premises must be seen as based on the same implicit condition
    
    var conditional: [Rule] {
        let S = Term.var("S")
        let P = Term.var("P")
        let M = Term.var("M")
        let C = Term.var("C")
        let T1 = Term.var("T1")
        let T2 = Term.var("T2")
        switch self {
        case .deduction:
            return [
                ((C && S) => P,                 S,             C  => P, tf),
                ((C && S) => P,            M => S,        (C && M) => P, tf)
            ]
        case .abduction:
            return [
                ((C && S) => P,            C => P,                   S, tf),
                ((C && S) => P,     (C && M) => P,              M => S, tf)
            ]
        case .induction:
            return [
                (       C => P,                 S,     (C && S) => P, tf),
                ((C && M) => P,            M => S,     (C && S) => P, tf)
            ]
        case .intersection:
            return [
                (             T1,                T2,        (T1 && T2), tf) // TODO: verify nothing else needs to be checked
            ]
        default:
            return []
        }
    }
    
    var variable_and_temporal: [Rule] {
        let S = Term.var("S")
        let P = Term.var("P")
        switch self {
        case .induction:
            return [(P,  S,  S  => P, tf)]
        case .comparison:
            return [(S,  P,  S <=> P, tf)]
        default:
            return []
        }
    }
    
}

extension Theorems {
    public var rules: [Statement] {
        let S = Term.var("S")
        let P = Term.var("P")
        let S1 = Term.var("S1")
        let S2 = Term.var("S2")

        let T1 = Term.var("T1")
        let T2 = Term.var("T2")

        switch self {
        case .inheritance:
            return [
                (T1 & T2) --> (T1),
                (T1 - T2) --> (T1)
            ]
        case .similarity:
            return [
                -(-T1) <-> (T1)
            ]
        case .implication:
            return [
                (S <-> P) => (S --> P),
                (S <=> P) => (S => P),
                (S1 && S2) => (S1)
            ]
        case .equivalence:
            return [
                (S <-> P) <=> &&[(S --> P), (P --> S)],
                (S <=> P) <=> &&[(S  => P), (P  => S)],
                
                (S <-> P) <=> (.instance(S) <-> .instance(P)),
                (S <-> P) <=> (.property(S) <-> .property(P)),
                
                (S --> .instance(P)) <=> (S <-> .instance(P)),
                (.property(S) --> P) <=> (.property(S) <-> P)
            ]
        }
    }
}


// MARK: - Helpers

extension Rules {
    // utility
    private func replaceCopulas(_ statement: Statement) -> Statement {
        var statement = statement
        if case .statement(let s, let c, let p) = statement {
            if c == .inheritance {
                statement = .statement(s, .implication, p)
            }
            if c == .similarity {
                statement = .statement(s, .equivalence, p)
            }
        }
        return statement
    }
}


// Grammar

public typealias Statement = Term

public indirect enum Term: Hashable {
    case symbol(String) /// <word>
    case compound(Connector, [Term])
    case statement(Term, Copula, Term)
    case variable(Variable)
    case operation(String, [Term])
}

public enum Copula: String, CaseIterable {
    //// Primary
    case inheritance       =    "->" // NAL 1
    case similarity        =   "<–>"     // 2
    case implication       =    "=>"     // 5
    case equivalence       =   "<=>"     // 5
    //// Secondary
    case instance          =   "•–>"     // 2
    case property          =    "–>•"    // 2
    case insProp           =   "•->•"    // 2
    //// Temporal
    case predictiveImp     =   "/=>"     // 7
    case retrospectiveImp  =  "\\=>"     // 7 - note: second slash is bc escape char in Swift
    case concurrentImp     =   "|=>"     // 7
    case predictiveEq      =  "/<=>"     // 7
    case retrospectiveEq   =  "\\<=>"    // 7 - note: book describes it as optional
    case concurrentEq      =  "|<=>"     // 7
}

public enum Connector: String, CaseIterable {
    /// intensional set  Ω
    case intSet = "[]"
    /// extensional set U
    case extSet = "{}"
    
    /// extensional intersection
    case Ω = "⋂" /// intensional set
    /// intensional intersection
    case U = "⋃" /// extensional set
    /// extensional difference
    case l = "–"
    /// intensional difference
    case ø = "ø"
    
    /// product
    case x = "⨯"
    
    /// extensional image
    case e = "/"
    /// intensional image
    case i = "\\" /// -- two slashes are because swift
    
    /// negation
    case n = "¬"
    /// conjunction
    case c = "∧"
    /// disjunction
    case d = "∨"
    
    /// sequential conjunction
    case s = ","
    /// parallel conjunction
    case p = ";"
}

public enum Variable: Hashable {
    case independent(String)
    case dependent(String?, [String])
    case query(String?)
}

public struct Judgement: Hashable {
    public let statement: Statement
    public let truthValue: TruthValue
    
    public let tense: Tense?
    public let derivationPath: [String]
    
    public var timestamp: UInt32 = 0
    
    // TODO: need to add desireValue
    /*
     “In NARS, a desire-value is not only attached to every goal, but to every event, because an event may become a goal in the future (if it is not already a goal).
 */
}

public typealias DesireValue = TruthValue

public struct Goal: Hashable {
    public let statement: Statement
    public let desireValue: DesireValue
}

public enum Quest: Hashable {
    case truth
    case desire
}

public struct Question: Hashable {
    public let statement: Statement
    public let type: Quest
    
    public let tense: Tense?
}

public enum Tense: String, Hashable {
    case past    = "<<"
    case present = "||"
    case future  = ">>"
}


public typealias Theorem = (Statement) -> Statement?

public enum Theorems: CaseIterable {
    case inheritance
    case similarity
    case implication
    case equivalence
}

extension Theorems {
    public static func apply(_ j: Judgement) -> [Judgement] {
        let res: [[Statement]] = self.allCases.map {
            var results = $0.rules.compactMap { Term.match(t: $0, s: j.statement) }
            if case .statement(let s, let c, let p) = j.statement, c == .similarity || c == .equivalence {
                results.append(contentsOf: $0.rules.compactMap { Term.match(t: $0, s: .statement(p, c, s)) })
            }
            if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
                if terms.count == 2 { // TODO: handle compounds with multiple terms
                    results.append(contentsOf: $0.rules.compactMap { Term.match(t: $0, s: .compound(conn, terms.reversed())) })
                }
            }
            return results
        }

        let results: [[Judgement]] = res.flatMap{$0}.map { t in
            var rel = reliance
            if case .statement(let sub, let cop, _) = t, cop == .equivalence {
                rel = j.statement == sub ? 0.9 : 1.0
            }
            var results = Rules.strong.flatMap {
                $0.apply((j, t-*(1,rel, ETERNAL)))
            }.compactMap { $0 }
            
            if case .statement(let s, let c, let p) = j.statement, c == .similarity || c == .equivalence {
                results.append(contentsOf:
                    Rules.strong.flatMap {
                    $0.apply((Judgement(.statement(p, c, s), j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp), t-*(1,reliance, ETERNAL)))
                    }.compactMap { $0 }
               )
            }
            if case .compound(let conn, let terms) = j.statement, conn == .c || conn == .U || conn == .Ω {
                if terms.count == 2 { // TODO: handle compounds with multiple terms
                    results.append(contentsOf:
                        Rules.strong.flatMap {
                        $0.apply((Judgement(.compound(conn, terms.reversed()), j.truthValue, j.derivationPath, tense: j.tense, timestamp: j.timestamp), t-*(1,reliance, ETERNAL)))
                        }.compactMap { $0 }
                   )
                }
            }
            return results
        }
        
        let unique = results.flatMap({$0}).removeDuplicates()
        return unique
    }
}


// MARK: - Helper

extension Term {
    static func match(t: Statement, s: Statement) -> Statement? {
        var results = [Term]()
        let goal = t.terms.map({ $0.logic() === s.logic() }).reduce(success, ||)
        
        for sol in solve(goal) {
            //                print(sol)
            let ts = s.terms.flatMap({ $0.terms.map({ $0.logic() }) })
            
            let valid = sol.allSatisfy { (v, _) in
                !ts.contains { $0.equals(v) }
            }
            
            if valid {
                var result = t
                for item in sol {
                    result = result.replace(termName: item.LogicVariable.name, term: .from(logic: item.LogicTerm))
                }
                if result != t {
                    results.append(result)
                }
            }
        }
        
        return results.min(by: { $0.complexity < $1.complexity })
    }
}

public struct TruthValue: Hashable {
    public let frequency: Double
    public let confidence: Double
    
    public let rule: Rules! // for derived values
}

public extension TruthValue {
    var f: Double {frequency}
    var c: Double {confidence}
    var l: Double {lowerFrequency}
    var u: Double {upperFrequency}
    var wpos: Double {positiveEvidence}
    var wtot: Double {totalEvidence}
    var e: Double {expectation}
    
    var positiveEvidence: Double { k * f * c / (1 - c) }
    var totalEvidence: Double { k * c / (1 - c) }
    var lowerFrequency: Double { f * c }
    var upperFrequency: Double { 1 - c * (1 - f) }
    var expectation: Double { (l + u) / 2 }
}

extension TruthValue {
    static var tautology: TruthValue { TruthValue(1, 1) }
}

public typealias TruthFunction = (TruthValue, TruthValue) -> TruthValue

infix operator ~ // rule to truth function mapping
private func ~(_ r: (Rules, Bool), _ tf: @escaping TruthFunction) -> TruthFunction {
    { (tv1, tv2) in
        let (rule, inverse) = r
        let tv = inverse ? tf(tv2, tv1) : tf(tv1, tv2)
        return TruthValue(tv.f, tv.c, rule)
    }
}

extension TruthValue {
    static func truthFunction(_ r: Rules, _ i: Bool) -> TruthFunction {
        switch r {
        case .deduction:       return (r,i)~deduction
        case .induction:       return (r,i)~induction
        case .abduction:       return (r,i)~abduction
        case .exemplification: return (r,i)~exemplification
        case .comparison:      return (r,i)~comparison
        case .analogy:         return (r,i)~analogy
        case .resemblance:     return (r,i)~resemblance
            
        case .intersection:    return (r,i)~intersection
        case .union:           return (r,i)~union
        case .difference:      return (r,i)~difference
        
        default:               return (r,i)~{_,_ in .tautology }
        }
    }
    
    static var deduction: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, f2)
        let c = and(f1, f2, c1, c2)
        return TruthValue(f, c)
    }
    static var induction: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let positive = and(f2, c2, f1, c1) // w+
        let total = and(f2, c2, c1) // w
        let evidence = Evidence(positive, total)
        return TruthValue(evidence)
    }
    static var abduction: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let positive = and(f1, c1, f2, c2) // w+
        let total = and(f1, c1, c2) // w
        let evidence = Evidence(positive, total)
        return TruthValue(evidence)
    }
    static var exemplification: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let positive = and(f1, c1, f2, c2) // w+
        let total = and(f1, c1, f2, c2) // w
        let evidence = Evidence(positive, total)
        return TruthValue(evidence)
    }
}

extension TruthValue {
    static var comparison: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let positive = and(f1, c1, f2, c2) // w+
        let total = and(or(f1, f2), c1, c2) // w
        let evidence = Evidence(positive, total)
        return TruthValue(evidence)
    }
    static var analogy: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, f2)
        let c = and(f2, c1, c2)
        return TruthValue(f, c)
    }
    static var resemblance: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, f2)
        let c = and(or(f1, f2), c1, c2)
        return TruthValue(f, c)
    }
}

extension TruthValue {
    static var intersection: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, f2)
        let c = and(c1, c2)
        return TruthValue(f, c)
    }
    static var union: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = or(f1, f2)
        let c = and(c1, c2)
        return TruthValue(f, c)
    }
    static var difference: TruthFunction = { (tv1, tv2) in
        let (f1, f2) = (tv1.frequency, tv2.frequency)
        let (c1, c2) = (tv1.confidence, tv2.confidence)
        let f = and(f1, not(f2))
        let c = and(c1, c2)
        return TruthValue(f, c)
    }
}


/// Extended Boolean operators
/// bounded by the range from 0 to 1
public func not(_ x: Double) -> Double {
    1 - x
}
public func and(_ xs: Double...) -> Double {
    xs.reduce(1, { $0 * $1 })
}
public func or(_ xs: Double...) -> Double {
    1 - xs.reduce(1, { $0 * (1 - $1)})
}
/// Statement is a fundamental type
public typealias Rule = (Statement, Statement, Statement, TruthFunction)
public typealias Apply = (_ judgements: (Judgement, Judgement)) -> Judgement? // reduce operation

public typealias Infer = (Judgement) -> Judgement? /// Single-premise rules

public enum Rules: String, CaseIterable {
    // NAL-1
    case deduction
    case induction
    case abduction
    case exemplification
    // NAL-2
    case comparison
    case analogy
    case resemblance
    // Compositional
    case intersection
    case union
    case difference
    // Local
    case negation
    case conversion
    case contraposition
    case revision
}

public extension Rules {
    static let strong: [Rules] = [.deduction, .analogy, .resemblance]
    //TODO: should we add intersection, difference and union to the list?
    
    static func immediate(_ j: Judgement) -> [Judgement] {
        let immediate: [Infer] = [/*negation(j1:),*/ conversion(j1:), contraposition(j1:)]
        return immediate.compactMap { $0(j) }
    }
}

extension Rules {
    var tf: TruthFunction {
        TruthValue.truthFunction(self, false)
    }
    var tfi: TruthFunction { // inverse
        TruthValue.truthFunction(self, true)
    }
    public var apply: (_ judgements: (Judgement, Judgement)) -> [Judgement?] {
        { j in
            var (j1, j2) = j
          //  print("\n>>>", j)
            
            var t1 = j1.statement // test
            var t2 = j2.statement // test
//            print(p1, p2, j1, j2)
    //        print("=", commonTerms)
    //        return nil
            
            if case .compound(let conn, let ts1) = t1, conn == .n {
//                print("1.", t1, t2)
                if ts1[0] == t2 { // TODO: use similarity helper to account for symmetrical connectors and copulas
                    return [] // no conclusion can be reached if premises are just opposite of each other
                }
            }
            if case .compound(let conn, let ts2) = t2, conn == .n {
//                print("2.", t1, t2)
                if ts2[0] == t1 { // TODO: use similarity helper to account for symmetrical connectors and copulas
                    return [] // no conclusion can be reached if premises are just opposite of each other
                }
            }
             
            /// temporal
            
//            if j1.truthValue.rule == nil && j2.truthValue.rule == nil {
//
//            }
            
            /// variable elimination
//            print("before")
//            print("t1", t1)
//           print("t2", t2)
//           print("")
            /// independent
            t1 = variableEliminationIndependent(t1, t2)
            t2 = variableEliminationIndependent(t2, t1)
          //  print("after")
//             print("t1", t1)
//            print("t2", t2)
//            print("")
            /// dependent
            if let result = variableEliminationDependent(t1, t2, j1, j2, self) {
                return result
            } else if let result = variableEliminationDependent(t2, t1, j2, j1, self) {
                return result
            }
            
            /// original code
            
            j1 = Judgement(t1, j1.truthValue, j1.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
            j2 = Judgement(t2, j2.truthValue, j2.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
           
            var x: [Judgement?] = []
            // apply rules
            self.allRules.forEach { r in
                x.append(rule_generator(r)((j1, j2)))
            }
            // switch order of premises
            self.allRules.forEach { r in
                x.append(rule_generator(r)((j2, j1)))
            }
            
            // MARK: Variable introduction
            // “In all these rules a dependent variable is only introduced into a conjunction or intersection, and an independent variable into (both sides of) an implication or equivalence.”
            
            /// independent-variable introduction
            ///
            // TODO: introVarInner from CompositionalRules in OpenNARS
            ///
            if self == .induction || self == .comparison {
                x.append(contentsOf: variableIntroductionIndependent(t1, t2, j1, j2, self))
            }
            
            /// dependent-variable introduction
            
            if self == .intersection {
                x.append(contentsOf: variableIntroductionDependent(t1, t2, j1, j2, self))
            }
            
            ///
            // TODO: multi-variable introduction rules
            ///
            
            let unique = x.compactMap({$0}).removeDuplicates()
//            print("+++", x)
//            print("===", unique)
            return unique
        }
    }
}

// MARK: Rule application

private var checkOverlap = false // TODO: dirty trick to get dependent-variable introduction to work

public var rule_generator: (_ rule: Rule) -> Apply { { (arg) -> ((Judgement, Judgement)) -> Judgement? in
    // premise (p1) premise (p2) conclusion (c) truth-function (tf)
    var (p1, p2, c, tf) = arg

    return { (arg) in
        let (j1, j2) = arg

        if let j1t = j1.tense, let j2t = j2.tense, j1t != j2t {
            return nil // temporal order cannot be determined
        }
        
        /*
         * MARK: do temporal
         */
        
        // TODO: need to check copulas to ensure temporal order can be established
        guard let temporal = temporalReasoning(c) else {
            return nil // outside temporal window
        }

        c = temporal // introduce temporal copulas

        /*
         * MARK: appply logic
         */
        
        guard var result = logicReasoning(c) else {
            return nil // no conclusion
        }
        
        // TODO: come up with a better way
        result = determineOrder()
        result = accountForExemplification()
        
        //        print("here", result)

        // TODO: handle temporal compounds
        // TODO: check that compounds do not contain each other

        /*
         * MARK: check results
         */
        
        if let statement = validate(result), !statement.isTautology {
//            print("accepted", result)
            let truthValue = tf(j1.truthValue, j2.truthValue)
            let derivationPath = Judgement.mergeEvidence(j1, j2)
//            print("--")
//            print(j1, j2)
//            print("accepted", statement, truthValue, c)
            return Judgement(statement, truthValue, derivationPath, tense: j1.tense ?? j2.tense)
        }
        
        return nil // PROGRAM END
        
        
        // MARK: - helpers
        
        func temporalReasoning(_ t: Term) -> Term? {
            if j1.timestamp != ETERNAL, j2.timestamp != ETERNAL,
               case .statement(var cs, var cc, var cp) = t,
               cc == .implication || cc == .equivalence {
                let forward = j1.timestamp < j2.timestamp
                let delta = forward ? j2.timestamp - j1.timestamp : j1.timestamp - j2.timestamp
                
                let window = 50 * 1000000 // 50 ms
                let distance = 10
                // delta should be less than some param
                // otherwise rules should not apply
                // here we choose 1 second computed as
                // distance factor on either side of the window
                
                guard delta < window * 2 * distance else {
                    return nil
                }
                //                print("N", delta, window, forward, j1, j2)
                //                print(j1.timestamp, j2.timestamp)
                
                if delta < window {
                    //                    print("||", t)
                    cc = cc.concurrent
                } else if forward {
                    //                    print(">>", t)
                    cc = cc.predictive
                } else {
                    //                    print("<<", t)
                    cc = cc.retrospective
                }
                
                // use temporal conclusion
                return .statement(cs, cc, cp)
            }

            return t // use original conclusion
        }
        
        func logicReasoning(_ t: Term) -> Term? {
            var result = t
            var map: [String: Term] = [:]
            let test1: LogicGoal = (p1.logic() === j1.statement.logic())
            let test2: LogicGoal = (p2.logic() === j2.statement.logic())
            
            let substitution = solve(test1 && test2).makeIterator().next()
            
            // TODO: use LogicVariableFactory to avoid collision with terms
            // i.e. terms names "S" and "P" will fail a check below and produce no conclusion
            
            if let sol = substitution {
                //            print("\n---SOL---\n", sol, "\n")
                let ts = (p1.terms + p2.terms + c.terms).flatMap({ $0.terms.map({ $0.logic() }) })
                let valid = sol.allSatisfy { (v, _) in
                    ts.contains { $0.equals(v) }
                }
                
                if valid {
                    for item in sol {
                        //                print(item.LogicVariable.name)
                        //                print(type(of: item.LogicTerm))
                        result = result.replace(termName: item.LogicVariable.name, term: .from(logic: item.LogicTerm))
                        //                print("result\n", result)
                    }
                }
            }
            
//            print("}}}}", j1, j2, result)
            
            return (result == t) ? nil : result
        }
        
        func validate(_ term: Term) -> Term? {
            switch term {

            // TODO: difference connectors take exactly 2 terms

            case .compound(let connector, let terms):
                if terms.count == 0 {
                    return nil // empty compound
                }
                if terms.count == 1 {
                    if connector == .intSet || connector == .extSet {
                        if case .compound(let c, let ts) = terms[0] {
                            if ts.count == 1, c == .intSet || c == .extSet {
                                return nil // prevent nesting i.e. [{x}], {{x}}, [[x]], {[x]}
                            }
                        }
                        return term // instances and properties are allowed one component
                    }
//                    print("here", term)
                    if connector == .x || connector == .i || connector == .e {
                        return term
                    }
                    return nil
                }
                if checkOverlap && j1.evidenceOverlap(j2) {
                    return nil
                }
                if connector == .x || connector == .i || connector == .e {
                    return term
                }
                return connector.connect(terms)
                
            case .statement(let subject, let cop, let predicate):
                if let sub = validate(subject), let pre = validate(predicate) {
//                    if case .compound(let cs, _) = subject, case .compound(let cp, _) = predicate {
//                        if cs == .x && cp == .x {
//                            return nil
//                        }
//                    }
                    return .statement(sub, cop, pre)
                }
                return nil
                
            default:
                return term
            }
        }
        
        func determineOrder() -> Term {
            // TODO: get rid of this dirty trick and determine temporal order of the conclusion properly
            if case .statement(var cs, var cc, var cp) = result, cc == .equivalence || cc == .implication {
                if case .statement(let j1s, let j1c, let j1p) = j1.statement,
                   case .statement(let j2s, let j2c, let j2p) = j2.statement {
                    
                    if (j1c.isConcurrent && j2c.isConcurrent) {
                        // both concurrent
                        return .statement(cs, cc.concurrent, cp)
                    }
                                        
                    if (j1c.isPredictive && j2c.isPredictive) {
                        // both predicitve
                        return .statement(cs, cc.predictive, cp)
                    }
                    
                    if (j1c.isRetrospective && j2c.isRetrospective) {
                        // both retrospective
                        return .statement(cs, cc.retrospective, cp)
                    }
                    
                    if (j1c.isConcurrent && j2c.isPredictive)
                        || (j2c.isConcurrent && j1c.isPredictive) {
                        // one is concurrent, another is predictive
                        return .statement(cs, cc.predictive, cp)
                    }
                    
                    if (j1c.isConcurrent && j2c.isRetrospective)
                        || (j2c.isConcurrent && j1c.isRetrospective) {
                        // one is concurrent, another is retrospective
                        return .statement(cs, cc.retrospective, cp)
                    }
                    
                    // Complex
                    var list = [Term]()
                    
                    if j1c.isPredictive && j2c.isRetrospective {
                        list = [j1s, j1p]
                        if let idx = list.firstIndex(of: j2s) {
                            list.insert(j2p, at: idx)
                        }
                        if let idx = list.firstIndex(of: j2p) {
                            list.insert(j2s, at: idx+1)
                        }
                    } else if j2c.isPredictive && j1c.isRetrospective {
                        list = [j2s, j2p]
                        if let idx = list.firstIndex(of: j1s) {
                            list.insert(j1p, at: idx)
                        }
                        if let idx = list.firstIndex(of: j1p) {
                            list.insert(j1s, at: idx+1)
                        }
                    }
                    
                    if let cpi = list.firstIndex(of: cp), let csi = list.firstIndex(of: cs) {
                        if cpi > csi {
                            // predicate after subject
                            return .statement(cs, cc.predictive, cp)
                        } else {
                            // predicate before subject
                            return .statement(cs, cc.retrospective, cp)
                        }
                    }
                }
            }
//            if result == (("John" * "key_101") --> "hold") {
//                // set tense for the conclusion
//            }
            return result
        }
        
        func accountForExemplification() -> Term {
            let rule = tf(j1.truthValue, j2.truthValue).rule

            if rule == .exemplification {
                if case .statement(let rs, let rc, let rp) = result {
                    if rc.isPredictive {
                        return .statement(rs, rc.atemporal.retrospective, rp)
                    } else if rc.isRetrospective {
                        return .statement(rs, rc.atemporal.predictive, rp)
                    }
                }
            }
            
            return result
        }
    }
}
}


// MARK: - Variable introduction and elimination

private func variableEliminationIndependent(_ t1: Statement, _ t2: Statement) -> Statement {
    if case .statement(_, let cop1, _) = t1, cop1 == .implication || cop1 == .equivalence {
        return Term.match(t: t1, s: t2) ?? t1
    }
    return t1
}

private func variableEliminationDependent(_ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?]? {
    if case .compound(let conn, _) = t1, conn == .c || conn == .U || conn == .Ω {
        var x: [Judgement?] = []
        
        if let h = Term.match(t: t1, s: t2) {
            let tv = TruthValue.deduction(j1.truthValue, TruthValue(1, reliance))

            let res = r.allRules.flatMap { r in
                h.terms.flatMap { (t: Term) -> [Judgement?] in
                    let j = Judgement(t, tv, j1.derivationPath, tense: j1.tense, timestamp: j1.timestamp)
                    return [rule_generator(r)((j, j2)),
                            rule_generator(r)((j2, j))]
                }
            }
            
            for rs in res.compactMap({ $0 }) {
                x.append(contentsOf: Rules.allCases.flatMap { r in
                    r.apply((rs, j2))
                })
            }
        }
        
        return x.isEmpty ? nil : x
    }
    return nil
}

private func variableIntroductionIndependent(_ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?] {
    variableIntroduction(dependent: false, t1, t2, j1, j2, r)
}

private func variableIntroductionDependent(_ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?] {
    variableIntroduction(dependent: true, t1, t2, j1, j2, r)
}

private func variableIntroduction(dependent: Bool, _ t1: Statement, _ t2: Statement, _ j1: Judgement, _ j2: Judgement, _ r: Rules) -> [Judgement?] {
    var x: [Judgement?] = []
    if case .statement(_, let cop1, _) = t1, cop1 == .inheritance,
       case .statement(_, let cop2, _) = t2, cop2 == .inheritance {
        
        let common = Set(t1.terms).intersection(t2.terms)
        
        if !common.isEmpty {
            
            if dependent { checkOverlap = false }
            
            var vari = r.conditional.flatMap { r in
                [rule_generator(r)((j1, j2)),
                 rule_generator(r)((j2, j1))] // switch order of premises
            }.compactMap { $0 }
            
            if dependent == false { // Table 10.3
                vari.append(contentsOf: r.variable_and_temporal.flatMap { r in
                    [rule_generator(r)((j1, j2)),
                     rule_generator(r)((j2, j1))] // switch order of premises
                }.compactMap { $0 })
            }
            
            if dependent { checkOverlap = true }
            
            let rep: [Judgement] = vari.compactMap { j in
                var r = j.statement
                for (i, c) in common.enumerated() {
                    if dependent {
                        r = r.replace(termName: c.description, depVarName: "x\(i)")
                    } else {
                        r = r.replace(termName: c.description, indepVarName: "x\(i)")
                    }
                }
                if j.statement.description == r.description {
                    return nil // variable substitution was not successful
                }
                return Judgement(r, j.truthValue, j.derivationPath)
            }
            
            x.append(contentsOf: rep)
        }
    }
    return x
}
//  “The systems show different “personalities” when predicting the future,
//   and larger k corresponds to more conservative and risk-averse behavior.”

public let evidentialHorizon: Double = 1 // "personality parameter"

public let occamsRazor: Int = 1
public let reliance: Double = 0.9


public var k: Double { evidentialHorizon }
public var r: Int { occamsRazor }


public let ETERNAL: UInt32 = UInt32.max // DispatchTime.distantFuture

precedencegroup Copula { // priority
    higherThan: ComparisonPrecedence
}

infix operator -->    : Copula // "->"
infix operator <->    : Copula
infix operator  =>    : Copula
infix operator <=>    : Copula

infix operator •->    : Copula
infix operator  ->•   : Copula
infix operator •->•   : Copula

infix operator >>|=>  : Copula //  "/=>"  future
infix operator <<|=>  : Copula //  "\=>"   past
infix operator   |=>  : Copula //  "|=>"  present
infix operator >>|<=> : Copula //  "/<=>"
infix operator <<|<=> : Copula //  "\<=>"
infix operator   |<=> : Copula //  "|<=>"

// convenience initializer for Judgement
public func + (_ s: Statement, fc: (Double, Double, UInt32)) -> Judgement {
    Judgement(s, TruthValue(fc.0, fc.1), timestamp: fc.2)
}

postfix operator -*
public extension Statement {
    static postfix func -* (_ s: Statement) -> Judgement {
        switch s {
        case .symbol:
            return s + (1.0, 0.9, ETERNAL)
        case .compound:
            return s + (1.0, 0.9, ETERNAL) // TODO: is this accurate?
        case .statement(let subject, _, let predicate):
            return subject == predicate ?
            s + (1.0, 1.0, ETERNAL) // tautology
                :
            s + (1.0, 0.9, ETERNAL) // fact
        case .variable:
            return s + (1.0, 0.9, ETERNAL) // TODO: is this accurate?
        case .operation:
            return .NULL + (1.0, 0.9, ETERNAL)
        }
    }
}

infix operator -* : Copula
public func -* (_ s: Statement, _ fc: (Double, Double, UInt32)) -> Judgement {
    s + fc
}

postfix operator •->
prefix  operator ->•
public extension Term {
    static postfix func •-> (_ t: Term) -> Term { instance(t) }
    static prefix  func ->• (_ t: Term) -> Term { property(t) }
}

// NAL-1
public func -->  (_ s: Term, p: Term) -> Statement { .statement( s    , .inheritance ,    p ) }
// NAL-2
public func <->  (_ s: Term, p: Term) -> Statement { .statement( s    , .similarity  ,    p ) }
public func •->  (_ s: Term, p: Term) -> Statement { .statement( s•-> , .inheritance ,    p ) }
public func ->•  (_ s: Term, p: Term) -> Statement { .statement( s    , .inheritance , ->•p ) }
public func •->• (_ s: Term, p: Term) -> Statement { .statement( s•-> , .inheritance , ->•p ) }
// NAL-5
public func =>   (_ s: Term, p: Term) -> Statement { .statement( s    , .implication ,    p ) }
public func <=>  (_ s: Term, p: Term) -> Statement { .statement( s    , .equivalence ,    p ) }
// NAL-7
public func >>|=>   (_ s: Term, p: Term) -> Statement { .statement( s    , .predictiveImp    ,    p ) }
public func <<|=>   (_ s: Term, p: Term) -> Statement { .statement( s    , .retrospectiveImp ,    p ) }
public func   |=>   (_ s: Term, p: Term) -> Statement { .statement( s    , .concurrentImp    ,    p ) }

prefix operator >> //  "/=>"  future
prefix operator << //  "\=>"   past
prefix operator || //  "|=>"  present

public prefix func >> (_ s: Term) -> Statement { .NULL >>|=> s } /// it will rain
public prefix func << (_ s: Term) -> Statement { .NULL <<|=> s } /// it rained
public prefix func || (_ s: Term) -> Statement { .NULL   |=> s } /// it's raining

extension Statement {
    static prefix func - (_ s: Statement) -> Statement { .compound(.n, [s]) }
}

public func  & (_ lhs: Statement, _ rhs: Statement) -> Statement {  +[lhs, rhs] }
public func  | (_ lhs: Statement, _ rhs: Statement) -> Statement {  %[lhs, rhs] }
public func  - (_ lhs: Statement, _ rhs: Statement) -> Statement {  -[lhs, rhs] }
public func  ~ (_ lhs: Statement, _ rhs: Statement) -> Statement {  ~[lhs, rhs] }
public func  * (_ lhs: Statement, _ rhs: Statement) -> Statement {  *[lhs, rhs] }
public func && (_ lhs: Statement, _ rhs: Statement) -> Statement { &&[lhs, rhs] }
public func || (_ lhs: Statement, _ rhs: Statement) -> Statement { ||[lhs, rhs] }

prefix operator  * // product
prefix operator && // conjunction
prefix operator  % // intensional intersection
/* -- leave commented out --
prefix operator  + // extensional intersection
prefix operator  - // extensional difference
prefix operator  ~ // intensional difference
prefix operator || disjunction
-- already declared elsewhere -- */

extension Array where Element == Statement {
    public static prefix func  + (_ s: Array<Statement>) -> Statement { ç.Ω.connect(s) }
    public static prefix func  % (_ s: Array<Statement>) -> Statement { ç.U.connect(s) }
    public static prefix func  - (_ s: Array<Statement>) -> Statement { ç.l.connect(s) }
    public static prefix func  ~ (_ s: Array<Statement>) -> Statement { ç.ø.connect(s) }
    public static prefix func  * (_ s: Array<Statement>) -> Statement { ç.x.connect(s) }
    public static prefix func && (_ s: Array<Statement>) -> Statement { ç.c.connect(s) }
    public static prefix func || (_ s: Array<Statement>) -> Statement { ç.d.connect(s) }
}

extension Question {
    public init(_ statement: Statement, _ type: Quest, _ tense: Tense? = nil) {
        self.statement = statement
        self.type = type
        self.tense = tense
    }
}

extension Goal {
    public init(_ statement: Statement, _ desireValue: DesireValue) {
        self.statement = statement
        self.desireValue = desireValue
    }
}

extension TruthValue: Equatable {
    public static func ==(_ lhs: TruthValue, _ rhs: TruthValue) -> Bool {
        lhs.f == rhs.f && lhs.c == rhs.c // ignore rule
    }
}

extension Judgement: Equatable {
    public static func == (lhs: Judgement, rhs: Judgement) -> Bool {
        lhs.statement == rhs.statement && lhs.truthValue == rhs.truthValue
    }
}

extension Term: Comparable {
    public static func < (lhs: Term, rhs: Term) -> Bool {
        lhs.description < rhs.description
    }
}

//
// TODO: check all code
// TODO: Whenever a judgement is constructed from another judgement or a statement, make sure we keep temporal information
//

//prefix operator •
//
//public extension Copula {
//    func makeStatement(_ subject: Term, _ predicate: Term) -> Statement {
//        Statement(subject: subject, copula: self, predicate: predicate)
//    }
//    static func makeStatement(_ copula: Copula) -> (_ s: Term, _ p: Term) -> Statement {
//        { s, p in
//            copula.makeStatement(s, p)
//        }
//    }
//    init(_ copula: Copula) {
//        self = copula
//    }
//}

/*
extension Copula {
    var term: Term { .word(rawValue) }
}

extension Term {
    var copula: Copula? { Copula(rawValue: description) }
    var statement: Statement? {
        switch self {
//        case .compound(let connector, let terms):
//            // TODO: perform additional checks for number of terms and their types
//            if let copula = Copula(rawValue: connector.description) {
//                return Statement(terms[0], copula, terms[1])
//            }
//            return nil
        default: return nil
        }
    }
}
*/


// MARK: CustomStringConvertible

extension Term: CustomStringConvertible {
    public var description: String {
        switch self {
        case .symbol(let word):
            return word
        case .compound(let connector, let terms):
            if terms.count == 2 {
                return "(\(terms[0]) \(connector.rawValue) \(terms[1]))"
            } else if connector == .intSet || connector == .extSet {
                if connector == .intSet {
                    return "[\(terms.map{$0.description}.joined(separator: " "))]"
                } else {
                    return "{\(terms.map{$0.description}.joined(separator: " "))}"
                }
            } else if connector == .n {
                return connector.rawValue + "(\(terms[0].description))"
            } else {
                return "(\(connector.rawValue) \(terms.map{$0.description}.joined(separator: " ")))"
            }
        case .statement(let subject, let copula, let predicate):
            var s = "\(subject)"
            if case .statement = subject {
                s = "(\(subject))"
            }
            var p = "\(predicate)"
            if case .statement = predicate {
                p = "(\(predicate))"
            }
            return s + " " + copula.rawValue + " " + p
        case .variable(let variable):
            switch variable {
            case .independent(let word):
                return "#\(word)" //TODO: update to use `$`
            case .dependent(let word, let variables):
                if let w = word {
                    let independents = variables.map { "#\($0)" }
                    let list = independents.joined(separator: ", ")
                    return "#\(w)(\(list))"
                }
                return "#"
            case .query(let word):
                return (word == nil) ? "?" : "?\(word!)"
            }
        case .operation(let name, let terms):
            return name + terms.map{$0.description}.joined(separator: " ")
        }
    }
}

extension Evidence: CustomStringConvertible {
    public var description: String {
        "(\(positive), \(total))"
    }
}

extension TruthValue: CustomStringConvertible {
    public var description: String {
        let r = rule == nil ? "." : "\(rule!)"
        let f = "\(f)".count == 3 ? "\(f)0" : "\(f)"
        let c = "\(c)".count == 3 ? "\(c)0" : "\(c)"
        return "%\(f);\(c)%\(r)"
    }
}

extension Rules: CustomStringConvertible {
    public var description: String {
        switch self {
        case .conversion:
            return ".cnv"
        case .contraposition:
            return ".cnt"
        default:
            return "." + rawValue.prefix(3)
        }
    }
}

extension Question: CustomStringConvertible {
    public var description: String {
        "<\(statement)>?"
    }
}

extension Judgement: CustomStringConvertible {
    public var description: String {
        "<\(statement)>. " + "\(truthValue)"
    }
}

extension Goal: CustomStringConvertible {
    public var description: String {
        "<\(statement)>! " + "\(desireValue)"
    }
}

extension Tense: CustomStringConvertible {
    public var description: String {
        rawValue
    }
}

// MARK: Identifiable

extension Judgement {
    public var identifier: String { tense == nil ? statement.description : tense!.description + " " + statement.description }
}


extension Question {
    public var identifier: String { tense == nil ? statement.description : tense!.description + " " + statement.description }
}


// MARK: Utility

/// from https://stackoverflow.com/a/34699637
extension Array where Element == Bool {
    public var allValid: Bool { !contains(false) }
}


/// from https://stackoverflow.com/a/38036978
public func rounded(_ d: Double, _ x: Int = 100) -> Double {
    let result = (d * Double(x)).rounded() / Double(x)
    return result.isNaN ? 0 : result
}

func pow(_ x: Double, _ y: Int) -> Double {
    let isNegative = y < 0
    var res = 1.0
    for _ in 1...abs(y) {
        res *= x
    }
    return isNegative ? 1 / res : res
}






public extension Array where Element == Judgement {
    func removeDuplicates() -> [Judgement] {
        let unique = Dictionary(grouping: self) {
            $0.identifier
        }.values.compactMap {
            $0.max { j1, j2 in
                let j = choice(j1: j1, j2: j2)
                return j.statement == j2.statement
            }
        }
        return unique
    }
}

extension Judgement {
    public init(_ statement: Statement, _ truthValue: TruthValue, _ derivationPath: [String] = [], tense: Tense? = nil, timestamp: UInt32 = 0) {
        self.statement = statement
        self.truthValue = truthValue
        self.tense = tense
        self.timestamp = tense == nil ? ETERNAL : timestamp
        if derivationPath.isEmpty {
            let description = Judgement.sortedDescription(statement)
//            print("--", description)
            self.derivationPath = ["\(description)+\((truthValue.f, truthValue.c, timestamp))"]
        } else {
            self.derivationPath = derivationPath
        }
//        print(statement)
//        print(derivationPath)
    }
    
    private static func sortedDescription(_ statement: Statement) -> String {
        var st = ""
        switch statement {
        case .statement(let s, let c, let p):
            if c == .similarity {
                st = (s < p) ? "\(s) \(c.rawValue) \(p)" : "\(p) \(c.rawValue) \(s)"
            } else if c == .equivalence {
                let sub = sortedDescription(s)
                let pre = sortedDescription(p)
                st = (sub < pre) ? "(\(sub)) \(c.rawValue) (\(pre))" : "(\(pre)) \(c.rawValue) (\(sub))"
            } else {
                st = "\(statement)"
            }
        case .compound(let c, let terms):
            if c == .c || c == .d || c == .n {
                st = "\(c.rawValue) \(terms.map{"(\(sortedDescription($0)))"}.sorted().joined(separator: ", "))"
            } else {
                st = "\(statement)"
            }
        default:
            st = "\(statement)"
        }
        return st
    }
}

extension Judgement {
    static func mergeEvidence(_ j1: Judgement, _ j2: Judgement) -> [String] {
        if j1.derivationPath.isEmpty {
            return j2.derivationPath
        } else if j2.derivationPath.isEmpty {
            return j1.derivationPath
        } else {
            var tail: [String] = []
            if j1.derivationPath.count < j2.derivationPath.count {
                tail = Array(j2.derivationPath.suffix(from: j1.derivationPath.endIndex))
            } else if j2.derivationPath.count > j1.derivationPath.count {
                tail = Array(j1.derivationPath.suffix(from: j1.derivationPath.count))
            }
            return (zip(j1.derivationPath, j2.derivationPath).reduce([], { partialResult, next in
                partialResult + (next.0 == next.1 ? [next.0] : [next.0, next.1])
            }) + tail).suffix(100)
        }
    }
    
    public func evidenceOverlap(_ j2: Judgement) -> Bool {
        let sameRoot = derivationPath.first == j2.derivationPath.first
        let p1 = sameRoot ? Array(derivationPath.dropFirst()) : derivationPath
        let p2 = sameRoot ? Array(j2.derivationPath.dropFirst()) : j2.derivationPath

        if p1.isEmpty && p2.isEmpty {
            return true // judgements have the same root
        } else if p1.count == 1 && p2.count == 1 {
            if p1[0].hasSuffix("\(ETERNAL))") && p2[0].hasSuffix("\(ETERNAL))") {
                // judgements are both eternal
//                print("p1", p1)
//                print("p2", p2)
                if p1[0] == p2[0] // same path or one is a theorem which has E as its evidential base
                    || p1[0].hasSuffix("+(1.0, 1.0, \(ETERNAL))") || p2[0].hasSuffix("+(1.0, 1.0, \(ETERNAL))") {
                
//                    if p1[0].prefix(while: {$0 != "+"}) == p2[0].prefix(while: {$0 != "+"}) { // NO GOOD
                        
//                    || p1[0].hasPrefix("(swan <–> bird) <=> (swan -> bird ∧ bird -> swan)") || p1[0].hasPrefix("(bird <–> swan) <=> (bird -> swan ∧ swan -> bird)") {
//                    // TODO: do proper comparison taking into account symmetrical statements
//                    // so <bird <-> swan> should be same as <swan <-> bird>
                    return true // same path
                } else {
//                    if p1[0].hasPrefix(statement.description) && p2[0].hasPrefix(j2.statement.description) {
//                        return false // both statements are user inputs
//                    }
                    return false // different path
                }
            }
        }
        
        return !Set(p1).intersection(Set(p2)).isEmpty
    }
}

extension Copula {
    var atemporal: Copula {
        switch self {
        case .predictiveImp: fallthrough
        case .retrospectiveImp: fallthrough
        case .concurrentImp: return .implication
        case .predictiveEq: fallthrough
        case .retrospectiveEq: fallthrough
        case .concurrentEq: return .equivalence
        default: return self
        }
    }
    var concurrent: Copula {
        switch self {
        case .implication: return .concurrentImp
        case .equivalence: return .concurrentEq
        default: return self
        }
    }
    var predictive: Copula {
        switch self {
        case .implication: return .predictiveImp
        case .equivalence: return .predictiveEq
        default: return self
        }
    }
    var retrospective: Copula {
        switch self {
        case .implication: return .retrospectiveImp
        case .equivalence: return .retrospectiveEq
        default: return self
        }
    }
    var isConcurrent: Bool {
        self == .concurrentEq || self == .concurrentImp
    }
    var isPredictive: Bool {
        self == .predictiveEq || self == .predictiveImp
    }
    var isRetrospective: Bool {
        self == .retrospectiveImp || self == .retrospectiveEq
    }
}

public typealias ç = Connector /// shorthand

extension Connector {
    var term: Term { Term.symbol(rawValue) }
    
    public static func e_(_ r: Term, _ t1: Term, _ t2: Term) -> Term { connect(.compound(.x, [r]), .e, (t1 * t2)) }
    public static func i_(_ r: Term, _ t1: Term, _ t2: Term) -> Term { connect(.compound(.x, [r]), .i, (t1 * t2)) }

    internal func connect(_ ts: [Term]) -> Term! {
        var ts = ts
        if ts.count < 2 {
            return nil // invalid compound
        }
        if ts.count > 2, let tail = ts.popLast(), let head = connect(ts) {
            return ç.connect(head, self, tail)
        }
        return ç.connect(ts[0], self, ts[1])
    }
    
    internal static func connect(_ t1: Term, _ c: Connector, _ t2: Term) -> Term! {
        var con = c
        let t1t = (c == .c || c == .d) ? Set([Term.symbol(t1.description)]) : Set(t1.terms)
        let t2t = (c == .c || c == .d) ? Set([Term.symbol(t2.description)]) : Set(t2.terms)
        var res = t1t.union(t2t)
        
        if c == .c || c == .d {
            if case .compound(let c1, _) = t1, (c1 == .c || c1 == .d) {
                return nil
            }
            if case .compound(let c2, _) = t2, (c2 == .c || c2 == .d) {
                return nil
            }
        }
        
        guard case .compound = t1, case .compound = t2, (c != .c || c != .d) else {
            // at least one term is a simple term
            guard t1t.intersection(t2t).isEmpty else {
                return nil // terms should not contain each other
            }
            return validate(res) ? .compound(c, [t1, t2]) : nil
        }
        
        // TODO: should we be filtering terms by intensional/extension
        if [.intSet, .extSet, .Ω, .U, .l, .ø].contains(c) {
            if case .compound(let c1, _) = t1,
               case .compound(let c2, _) = t2 {
                if c1 == .x || c1 == .e || c1 == .i
                    || c2 == .x || c2 == .e || c2 == .i {
                    return nil
                }
            }
        }
        
        switch c {
        /// definition 7.1 -- intensional/extensional sets
        case .intSet: res = t1t.union(t2t)
        case .extSet: res = t1t.union(t2t)
        
        /// definition 7.6 -- extensional intersection
        case .Ω: res = t1t.intersection(t2t)
        /// definition 7.7 -- intensional intersection
        case .U: res = t1t.union(t2t)
            
        /// definition 7.8 -- extensional difference
        case .l: res = t1t.subtracting(t2t); con = .U
        /// definition 7.9 -- intensional difference
        case .ø: res = t1t.subtracting(t2t); con = .Ω
            
        /// definition 8.1 -- sequence
        case .x: return .compound(.x, t1.terms + t2.terms)

        /// first term is a relation // TODO: need to validate
        case .e: return .compound(.e, t1.terms + t2.terms)
        case .i: return .compound(.i, t1.terms + t2.terms)
            
        case .n: return nil // handled separately
        case .c: res = t1t.intersection(t2t) // -- extensional difference
        case .d: res = t1t.union(t2t) // -- intensional intersection
        
        case .s: return .compound(.s, t1.terms + t2.terms)
        case .p: return .compound(.p, t1.terms + t2.terms)
        }
        
        // MARK: Validation
        
        // intention/extension sets are allowed one component
        if res.count == 1, case .compound(let c, _) = res.first, c == .intSet || c == .extSet {
            return .compound(con, Array(res))
        }
        
        return validate(res) ? .compound(con, Array(res).sorted()) : nil
    }
    
    /// MARK: helpers
    
    private static func validate(_ s: Set<Term>) -> Bool {
        if s.count < 2 { return false }
        // check if terms contain each other
        let result = s.flatMap { Term.getTerms($0) }
        if result.count != Set(result).count {
            return false
        }
        return true
    }
}



extension Sequence where Element == Term {
    func toList() -> List {
        var list: List = .empty
        for term in self.reversed() {
            list = List.cons(term.logic(), list)
        }
        return list
    }
}


extension Term {
    func logic() -> LogicTerm {
        switch self {
        case .symbol:
            return LogicValue(self)
        case .compound(let c, let terms):
            return List.cons(LogicValue(c), terms.toList())
        case .statement(let s, let c, let p):
            return List.cons(LogicValue(c.atemporal), List.cons(s.logic(), List.cons(p.logic(), List.empty)))
            
        case .variable:
            //        switch vari {
            //        case .independent(let name):
            //            return List.cons(LogicValue("var-ind"), List.cons(LogicVariable(named: name), List.empty))
            //        case .dependent(let name, let vars):
            //            var ll: List = .empty
            //            for v in vars.reversed() {
            //                ll = List.cons(LogicVariable(named: v), ll)
            //            }
            //            ll = List.cons(LogicValue("var-ind"), ll)
            //            return List.cons(LogicValue("var-dep"), List.cons(LogicVariable(named: name ?? "x()"), ll))
            //        }
            return LogicVariable(named: self.description) // TODO: handle nested variables
//            return LogicVariable(named: vari.name ?? "x")
            
        case .operation(let op, let terms):
            return List.cons(LogicValue(op), terms.toList())
        }
    }
        
    static func from(logic: LogicTerm) -> Term {
        if let value = logic as? LogicValue<Term> {
            return value.extract()
        }
        if let variable = logic as? LogicVariable {
            if let vari = Variable(variable.name) {
                return .variable(vari)
            }
        }
        if let list = logic as? List {
            if case .cons(let head, let tail) = list {
                if let value = head as? LogicValue<Connector> { // compound
                    return .compound(value.extract(), process(list: tail))
                }
                
                if let value = head as? LogicValue<Copula> { // statement
                    let terms = process(list: tail)
                    return .statement(terms[0], value.extract(), terms[1])
                }
                
                if let value = head as? LogicValue<String> { // operation
                    return .operation(value.extract(), process(list: tail))
                }
            }
        }
        // helper
        func process(list: LogicTerm) -> [Term] {
            var terms: [Term] = []
            if case .cons(let head, let tail) = list as? List {
                terms.append(Term.from(logic: head))
                terms.append(contentsOf: process(list: tail))
            }
            return terms
        }
        
        return .NULL // DEFAULT
    }
}
//
//  LogicKit.swift
//  LogicKit
//
//  Created by Dimitri Racordon on 07.02.17.
//  Copyright © 2017 University of Geneva. All rights reserved.
//
//  https://github.com/kyouko-taiga/SwiftKanren

protocol LogicTerm {

    // We can't make the LogicTerm conform to Equatable, as we need to use within
    // heterogeneous collections. Hence we can't have a safe requirements
    // (see WWDC 2015 - session 408). Similarly, we can't require conforming
    // types to implement the global equality operator (==), as the various
    // overloads would become ambiguous without a self requirement.
    func equals(_ other: LogicTerm) -> Bool

}

extension LogicTerm where Self: Equatable {

    func equals(_ other: LogicTerm) -> Bool {
        if other is Self {
            return (other as! Self) == self
        }

        return false
    }

}



struct LogicVariable: LogicTerm {

    let name: String

    init(named name: String) {
        self.name = name
    }

}

extension LogicVariable: Hashable {

    func hash(into hasher: inout Hasher) {
        hasher.combine(name)
    }

    static func == (left: LogicVariable, right: LogicVariable) -> Bool {
        return left.name == right.name
    }

}

extension LogicVariable: CustomStringConvertible {

    var description: String {
        return self.name
    }

}


class LogicVariableFactory {

    fileprivate var state: State
    private var LogicVariables = [String: LogicVariable]()

    fileprivate init(_ state: State) {
        self.state = state
    }

    subscript(name: String) -> LogicVariable {
        if let LogicVariable = self.LogicVariables[name] {
            return LogicVariable
        }

        self.LogicVariables[name] = LogicVariable(named: self.state.nextUnusedName)
        self.state = self.state.withNextNewName()
        return self.LogicVariables[name]!
    }

}


struct LogicValue<T: Equatable>: LogicTerm {

    fileprivate let wrapped: T

    init(_ val: T) {
        self.wrapped = val
    }

    func equals(_ other: LogicTerm) -> Bool {
        if let rhs = (other as? LogicValue<T>) {
            return rhs.wrapped == self.wrapped
        }

        return false
    }

    func extract() -> T {
        return wrapped
    }
}

extension LogicValue: Equatable {

    static func ==(lhs: LogicValue, rhs: LogicValue) -> Bool {
        return lhs.wrapped == rhs.wrapped
    }

}

extension LogicValue: CustomStringConvertible {

    var description: String {
        return String(describing: self.wrapped)
    }

}


struct Unassigned: LogicTerm, CustomStringConvertible {

    private static var LogicVariables = [LogicVariable: Int]()
    private static let unicodeSubscripts = [
        "\u{2080}", "\u{2081}", "\u{2082}", "\u{2083}", "\u{2084}",
        "\u{2085}", "\u{2086}", "\u{2087}", "\u{2088}", "\u{2089}"]

    private var id: Int

    fileprivate init(_ LogicVariable: LogicVariable) {
        if Unassigned.LogicVariables[LogicVariable] == nil {
            Unassigned.LogicVariables[LogicVariable] = Unassigned.LogicVariables.count
        }
        self.id = Unassigned.LogicVariables[LogicVariable]!
    }

    func equals(_ other: LogicTerm) -> Bool {
        return false
    }

    var description: String {
        var suffix = ""
        if self.id == 0 {
            suffix = Unassigned.unicodeSubscripts[0]
        } else {
            var number = self.id
            while number > 0 {
                suffix = Unassigned.unicodeSubscripts[number % 10] + suffix
                number /= 10
            }
        }

        return "_" + suffix
    }

}


enum List: LogicTerm {

    case empty, cons(LogicTerm, LogicTerm)

    func equals(_ other: LogicTerm) -> Bool {
        guard let rhs = other as? List else {
            return false
        }

        switch (self, rhs) {
        case (.empty, .empty):
            return true
        case (.cons(let lh, let lt), .cons(let rh, let rt)):
            return lh.equals(rh) && lt.equals(rt)
        default:
            return false
        }
    }

}


struct Map: LogicTerm {

    typealias StorageType = [String: LogicTerm]

    fileprivate let storage: StorageType

    init() {
        self.storage = [:]
    }

    init<S: Sequence>(_ items: S) where S.Iterator.Element == (key: String, value: LogicTerm) {
        var storage = StorageType()
        for (key, value) in items {
            storage[key] = value
        }
        self.storage = storage
    }

    var keys: Dictionary<String, LogicTerm>.Keys {//LazyMapCollection<StorageType, String> {
        return self.storage.keys
    }

    var values: Dictionary<String, LogicTerm>.Values {//LazyMapCollection<StorageType, LogicTerm> {
        return self.storage.values
    }

    subscript(key: String) -> LogicTerm? {
        return self.storage[key]
    }

    func with(key: String, value: LogicTerm) -> Map {
        var newStorage = self.storage
        newStorage[key] = value
        return Map(newStorage)
    }

}

extension Map: Equatable {

    static func == (left: Map, right: Map) -> Bool {
        let leftKeys = left.storage.keys.sorted()
        let rightKeys = right.storage.keys.sorted()

        guard leftKeys == rightKeys else {
            return false
        }

        for (leftKey, rightKey) in zip(leftKeys, rightKeys) {
            guard left.storage[leftKey]!.equals(right.storage[rightKey]!) else {
                return false
            }
        }

        return true
    }

}

extension Map: Sequence {

    func makeIterator() -> StorageType.Iterator {
        return self.storage.makeIterator()
    }

}

extension Map: Collection {

    var startIndex: StorageType.Index {
        return self.storage.startIndex
    }

    var endIndex: StorageType.Index {
        return self.storage.endIndex
    }

    func index(after: StorageType.Index) -> StorageType.Index {
        return self.storage.index(after: after)
    }

    subscript(index: StorageType.Index) -> StorageType.Element {
        return self.storage[index]
    }

}

extension Map: ExpressibleByDictionaryLiteral {

    init(dictionaryLiteral elements: (String, LogicTerm)...) {
        self.init(elements.map { (key: $0.0, value: $0.1) })
    }

}

extension Map: CustomStringConvertible {

    var description: String {
        return String(describing: self.storage)
    }

}


struct Substitution {

    init() {
        
    }
    
    fileprivate var storage = [LogicVariable: LogicTerm]()

    typealias Association = (LogicVariable: LogicVariable, LogicTerm: LogicTerm)

    subscript(_ key: LogicTerm) -> LogicTerm {
        // If the the given key isn't a LogicVariable, we can just give it back.
        guard let k = key as? LogicVariable else {
            return key
        }

        if let rhs = self.storage[k] {
            // Continue walking in case the rhs is another LogicVariable.
            return self[rhs]
        }

        // We give back the LogicVariable if is not associated.
        return key
    }

    func extended(with association: Association) -> Substitution {

        // NOTE: William Byrd's PhD thesis doesn't specify what is the
        // expected behaviour when extending a substitution map with an
        // already existing key.

        // TODO: Check for introduced circularity.

        var result = self
        result.storage[association.LogicVariable] = association.LogicTerm
        return result
    }

    func unifying(_ u: LogicTerm, _ v: LogicTerm) -> Substitution? {
        let walkedU = self[u]
        let walkedV = self[v]

        // LogicTerms that walk to equal values always unify, but add nothing to
        // the substitution.
        if walkedU.equals(walkedV) {
            return self
        }

        // Unifying a logic LogicVariable with some other LogicTerm creates a new entry
        // in the substitution.
        if walkedU is LogicVariable {
            return self.extended(with: (LogicVariable: walkedU as! LogicVariable, LogicTerm: walkedV))
        } else if walkedV is LogicVariable {
            return self.extended(with: (LogicVariable: walkedV as! LogicVariable, LogicTerm: walkedU))
        }

        // If the walked values of u and of v are lists, then unifying them
        // boils down to unifying their elements.
        if (walkedU is List) && (walkedV is List) {
            return self.unifyingLists(walkedU as! List, walkedV as! List)
        }

        // If the walked values of u and of v are maps, then unifying them
        // boils down to unifying their elements.
        if (walkedU is Map) && (walkedV is Map) {
            return self.unifyingMaps(walkedU as! Map, walkedV as! Map)
        }

        return nil
    }

    private func unifyingLists(_ u: List, _ v: List) -> Substitution? {
        switch (u, v) {
        case (.empty, .empty):
            // Empty lists always unify, but add nothing to the substitution.
            return self

        case (.cons(let uh, let ut), .cons(let vh, let vt)):
            // Unifying non-empty lists boils down to unifying their head,
            // before recursively unifying their tails.
            return self.unifying(uh, vh)?.unifying(ut, vt)

        default:
            // Unifying a non-empty list with an empty list always fail.
            return nil
        }
    }

    private func unifyingMaps(_ u: Map, _ v: Map) -> Substitution? {
        let leftKeys = u.keys.sorted()
        let rightKeys = v.keys.sorted()

        // Unifying dictionaries with different keys always fail.
        guard leftKeys == rightKeys else {
            return nil
        }

        // Unifying dictionaires boils down to unifying the values associated,
        // with each of their respective keys.
        var result: Substitution? = self
        for (leftKey, rightKey) in zip(leftKeys, rightKeys) {
            result = result?.unifying(u[leftKey]!, v[rightKey]!)
        }
        return result
    }

    func reified() -> Substitution {
        var result = Substitution()
        for LogicVariable in self.storage.keys {
            let walked = self.deepWalk(LogicVariable)
            if let v = walked as? LogicVariable {
                result = result.extended(with: (LogicVariable: LogicVariable, LogicTerm: Unassigned(v)))
            } else {
                result = result.extended(with: (LogicVariable: LogicVariable, LogicTerm: walked))
            }
        }
        return result
    }

    private func deepWalk(_ value: LogicTerm) -> LogicTerm {
        // If the given value is a list, we have to "deep" walk its elements.
        if let l = value as? List {
            switch l {
            case .empty:
                return l
            case .cons(let h, let t):
                return List.cons(self.deepWalk(h), self.deepWalk(t))
            }
        }

        // If the given value is a map, we have to "deep" walk its values.
        if let m = value as? Map {
            var reifiedMap = Map()
            for item in m {
                reifiedMap = reifiedMap.with(key: item.key, value: self.deepWalk(item.value))
            }
            return reifiedMap
        }

        // If the the given value isn't a LogicVariable, we can just give it back.
        guard let key = value as? LogicVariable else {
            return value
        }

        if let rhs = self.storage[key] {
            // Continue walking in case the rhs is another LogicVariable.
            return self.deepWalk(rhs)
        }

        // We give back the LogicVariable if is not associated.
        return value
    }

}

extension Substitution: Sequence {

    func makeIterator() -> AnyIterator<Association> {
        var it = self.storage.makeIterator()

        return AnyIterator {
            if let (LogicVariable, LogicTerm) = it.next() {
                return (LogicVariable: LogicVariable, LogicTerm: self[LogicTerm])
            }

            return nil
        }
    }

}


/// A struct containing a substitution and the name of the next unused logic
/// LogicVariable.
struct State {

    fileprivate let substitution: Substitution
    fileprivate var nextUnusedName: String {
        return "$" + String(describing: self.nextId)
    }

    private let nextId: Int

    init(substitution: Substitution = Substitution(), nextId: Int = 0) {
        self.substitution = substitution
        self.nextId = nextId
    }

    fileprivate func with(newSubstitution: Substitution) -> State {
        return State(substitution: newSubstitution, nextId: self.nextId)
    }

    fileprivate func withNextNewName() -> State {
        return State(substitution: self.substitution, nextId: self.nextId + 1)
    }

}


enum Stream {

    case empty
    indirect case mature(head: State, next: Stream)
    case immature(thunk: () -> Stream)

    // mplus
    fileprivate func merge(_ other: Stream) -> Stream {
        switch self {
        case .empty:
            return other

        case .mature(head: let state, next: let next):
            return .mature(head: state, next: next.merge(other))

        case .immature(thunk: let thunk):
            return .immature {
                return other.merge(thunk())
            }
        }
    }

    // bind
    fileprivate func map(_ LogicGoal: @escaping LogicGoal) -> Stream {
        switch self {
        case .empty:
            return .empty

        case .mature(head: let head, next: let next):
            return LogicGoal(head).merge(next.map(LogicGoal))

        case .immature(thunk: let thunk):
            return .immature {
                return thunk().map(LogicGoal)
            }
        }
    }

    // pull
    fileprivate func realize() -> Stream {
        switch self {
        case .empty:
            return .empty

        case .mature(head: _, next: _):
            return self

        case .immature(thunk: let thunk):
            return thunk().realize()
        }
    }

}

extension Stream: Sequence {

    func makeIterator() -> AnyIterator<Substitution> {
        var it = self

        return AnyIterator {

            // Realize the iterated stream here, so that we its state is
            // computed as lazily as possible (i.e. when the iterator's next()
            // method is called).

            switch it.realize() {
            case .empty:
                // Return nothing for empty stream, ending the sequence.
                return nil

            case .mature(head: let state, next: let successor):
                // Return the realized substitution and advance the iterator.
                it = successor
                return state.substitution

            case .immature(thunk: _):
                assertionFailure("realize shouldn't produce immature streams")
            }

            return nil
        }
    }

}


/// Represents a function that encapsulates a logic program and which, given a
/// state, returns a stream of states for each way the program can succeed.
typealias LogicGoal = (State) -> Stream


infix operator ≡   : ComparisonPrecedence
infix operator === : ComparisonPrecedence

/// Creates a LogicGoal that unify two LogicTerms.
///
/// The LogicGoal takes an existing state and returns (as a lazy stream) either a
/// state with bindings for the LogicVariables in u and v (using unification), or
/// nothing at all if u and v cannot be unified.
func ≡ (u: LogicTerm, v: LogicTerm) -> LogicGoal {
    return { state in
        if let s = state.substitution.unifying(u, v) {
            return .mature(head: state.with(newSubstitution: s), next: .empty)
        }

        return .empty
    }
}

/// Alternative for ≡(_:_:)
func === (u: LogicTerm, v: LogicTerm) -> LogicGoal {
    return u ≡ v
}


/// Takes a LogicGoal constructor and returns a LogicGoal with fresh LogicVariables.
///
/// This function takes a *LogicGoal constructor* (i.e. a function), which accepts
/// a single LogicVariable as parameter, and returns a new LogicGoal for which the
/// LogicVariable is fresh.
func fresh(_ constructor: @escaping (LogicVariable) -> LogicGoal) -> LogicGoal {
    return { state in
        constructor(LogicVariable(named: state.nextUnusedName))(state.withNextNewName())
    }
}


/// Takes a LogicGoal constructor and returns a LogicGoal with fresh LogicVariables.
///
/// This function takes a *LogicGoal constructor* (i.e. a function), which accepts
/// a LogicVariable factory as parameter, and returns a new LogicGoal for which all the
/// LogicVariables generated by the factory are fresh.
func freshn(_ constructor: @escaping (LogicVariableFactory) -> LogicGoal) -> LogicGoal {
    return { state in
        let factory = LogicVariableFactory(state)
        return constructor(factory)(factory.state)
    }
}


/// Constructs a disjunction of LogicGoals.
func || (left: @escaping LogicGoal, right: @escaping LogicGoal) -> LogicGoal {
    return { state in
        left(state).merge(right(state))
    }
}


/// Constructs a conjunction of LogicGoals.
func && (left: @escaping LogicGoal, right: @escaping LogicGoal) -> LogicGoal {
    return { state in
        left(state).map(right)
    }
}


/// Takes a LogicGoal constructor and returns a LogicGoal with substitution.
///
/// This function takes a *LogicGoal constructor* (i.e. a function), which accepts
/// a substitution as parameter, and returns a new LogicGoal.
func inEnvironment (_ constructor: @escaping (Substitution) -> LogicGoal) -> LogicGoal {
    return { state in
        let reified = state.substitution.reified()
        return constructor(reified)(state)
    }
}


/// Takes a LogicGoal and returns a thunk that wraps it.
func delayed(_ LogicGoal: @escaping LogicGoal) -> LogicGoal {
    return { state in
        .immature { LogicGoal(state) }
    }
}


/// Executes a logic program (i.e. a LogicGoal) with an optional initial state.
func solve(withInitialState state: State? = nil, _ program: LogicGoal) -> Stream {
    return program(state ?? State())
}


/// A LogicGoal that always succeeds.
let success = (LogicValue(true) === LogicValue(true))


/// A LogicGoal that always fails.
let failure = (LogicValue(false) === LogicValue(true))


/// Creates a LogicGoal that tests if a LogicTerm is an instance of a `Value<T>`
/// in the current substitution.
func isValue<T : Equatable>(_ LogicTerm: LogicTerm, _ type: T.Type) -> LogicGoal {
    return inEnvironment { substitution in
        if substitution [LogicTerm] is LogicValue<T> {
          return success
        } else {
          return failure
        }
    }
}


/// Creates a LogicGoal that tests if a LogicTerm is an instance of a `LogicVariable`
/// in the current substitution.
func isLogicVariable(_ LogicTerm: LogicTerm) -> LogicGoal {
    return inEnvironment { substitution in
        if substitution [LogicTerm] is LogicVariable {
          return success
        } else {
          return failure
        }
    }
}


/// Creates a LogicGoal that tests if a LogicTerm is an instance of a `List`
/// in the current substitution.
func isList(_ LogicTerm: LogicTerm) -> LogicGoal {
    return inEnvironment { substitution in
        if substitution [LogicTerm] is List {
          return success
        } else {
          return failure
        }
    }
}


/// Creates a LogicGoal that tests if a LogicTerm is an instance of a `Map`
/// in the current substitution.
func isMap(_ LogicTerm: LogicTerm) -> LogicGoal {
    return inEnvironment { substitution in
        if substitution [LogicTerm] is Map {
          return success
        } else {
          return failure
        }
    }
}

extension Term {
    public var isTautology: Bool {
        switch self {
        case .symbol:
            return false
        case .compound:
            return false // TODO: is this accurate?
        case .statement(let subject, _, let predicate):
            return /*copula == .inheritance &&*/ subject == predicate
//                Set(subject.terms).intersection(Set(predicate.terms)).isEmpty == false
        case .variable:
            return false
        case .operation:
            return false
        }
    }
}

extension Term {
    public static let º = Term.symbol("º") // image placeholder
    public static let NULL = Term.symbol("NULL")
    public static let SELF = Term.symbol("SELF")
    
    public static func word(_ w: String) -> Term { .symbol(w) }
    public static func `var`(_ s: String) -> Term { .variable(.independent(s)) }
    public static func instance(_ t: Term) -> Term { .compound(ç.extSet, [t]) }
    public static func property(_ t: Term) -> Term { .compound(ç.intSet, [t]) }
    
    public var terms: [Term] {
        switch self {
        case .symbol:
            return [self]
        case .compound(let c, let terms):
            if terms.count == 1, c == .intSet || c == .extSet {
                return [self]
            }
            return terms
        case .statement(let subject, _, let predicate):
            return [subject, predicate]
        case .variable:
            return [self] //TODO: Do we need to recurse into dependent variables?
        case .operation(_, let terms):
            return terms
        }
    }
    
    public var complexity: Double {
        switch self {
        case .symbol:
            return 1
        case .compound(_, let terms):
            return 1 + terms
                .map { $0.complexity }
                .reduce(0, +)
        case .statement(let subject, _, let predicate):
            return 1 + (subject.terms + predicate.terms)
                .map { $0.complexity }
                .reduce(0, +)
        case .variable:
            return 0
        case .operation(_, let terms):
            return 1 + terms
                .map { $0.complexity }
                .reduce(0, +)
        }
    }
    
    public var simplicity: Double {
        rounded(1 / pow(complexity, occamsRazor))
    }
    
    public static func getTerms(_ t: Term) -> [Term] {
        if t.terms.count == 1 {
            return t.terms
        }
        return t.terms.flatMap { getTerms($0) }
    }
}


// MARK: Replace

extension Term {
    func replace(varName: String, termName: String) -> Term {
        switch self {
        case .symbol:
            return self
        case .compound(let conn, let terms):
            return .compound(conn, terms.map{$0.replace(varName: varName, termName: termName)})
        case .statement(let sub, let cop, let pre):
            return .statement(sub.replace(varName: varName, termName: termName), cop, pre.replace(varName: varName, termName: termName))
        case .variable(let vari):
            switch vari {
            case .independent(let str):
                if str == varName {
                    return .symbol(termName)
                }
                return self
            case .dependent(let str, _):
                if str == varName {
                    return .symbol(termName)
                }
                return self
            default: // TODO: how to handle dependent vars?
                return self
            }
        case .operation(let name, let terms):
            return .operation(name, terms.map{$0.replace(varName: varName, termName: termName)})
        }
    }
    
    func replace(termName: String, indepVarName: String) -> Term {
        switch self {
        case .symbol(let str):
            if str == termName {
                return .variable(.independent(indepVarName))
            }
            return self
        case .statement(let sub, let cop, let pre):
            return .statement(sub.replace(termName: termName, indepVarName: indepVarName), cop, pre.replace(termName: termName, indepVarName: indepVarName))
        default: // TODO: properly handle all cases
            return self
        }
    }
    
    func replace(termName: String, depVarName: String) -> Term {
        switch self {
        case .symbol(let str):
            if str == termName {
                return .variable(.dependent(depVarName, []))
            }
            return self
        case .compound(let conn, let terms):
            return .compound(conn, terms.map{$0.replace(termName: termName, depVarName: depVarName)})
        case .statement(let sub, let cop, let pre):
            return .statement(sub.replace(termName: termName, depVarName: depVarName), cop, pre.replace(termName: termName, depVarName: depVarName))
        default: // TODO: properly handle all cases
            return self
        }
    }
    
    func replace(termName: String, term: Term) -> Term {
        switch self {
        case .symbol(let str):
            if str == termName {
                return term
            }
            return self
        case .compound(let conn, let terms):
            return .compound(conn, terms.map{$0.replace(termName: termName, term: term)})
        case .statement(let sub, let cop, let pre):
            return .statement(sub.replace(termName: termName, term: term), cop, pre.replace(termName: termName, term: term))
        case .variable:
            if description == termName {
                return term
            }
            return self
        default: // TODO: properly handle all cases
            return self
        }
    }
}

// MARK: ExpressibleByStringLiteral

extension Term: ExpressibleByStringLiteral {
    /// handles simple cases for use in testing and playgrounds
    public init(stringLiteral value: String) {
        self = {
            if value.first == "{" {
                return .instance(.init(stringLiteral: value.word))
            }

            if value.first == "[" {
                return .property(.init(stringLiteral: value.word))
            }

            if value.first == "?" {
                let word = value.dropFirst()
                let name = (word.count == 0) ? nil : String(word)
                return .variable(.query(name))
            }

            let words = value.words

            if words.count == 1 {
                return .symbol(words[0])
            }

            return .NULL
        }()
    }
}


// MARK: Replacements for Foundation methods

extension String {
    var word: String {
        var word: String = ""
        for c in self {
            if !["{", "}", "[", "]"].contains(c) {
                word.append(c)
            }
        }
        return word
    }
    
    var words: [String] {
        var words: [String] = []
        var word: String = ""
        for c in self {
            if c == " " {
                if !word.isEmpty {
                    words.append(word)
                    word = ""
                }
            } else {
                word.append(c)
            }
        }
        if !word.isEmpty {
            words.append(word)
        }
        return words
    }
}

extension Variable {
    var name: String? {
        switch self {
        case .independent(let string):
            return string
        case .dependent(let optional, _):
            return optional
        case .query(let optional):
            return optional
        }
    }
}

extension Variable {
    public init?(_ string: String) {
        if string.hasPrefix("?") {
            let name = string.suffix(from: string.index(string.startIndex, offsetBy: 1))
            self = .query(name.isEmpty ? nil : String(name))
            return
        }
        if string.hasPrefix("#") {
            let name = string.suffix(from: string.index(string.startIndex, offsetBy: 1))
            if name.isEmpty {
                self = .dependent(nil, [])
                return
            } else {
                // TODO: parse independent vars list
                if let idx = name.firstIndex(of: "(") {
                    let trimmed = name.prefix(upTo: idx)
                    self = .dependent(String(trimmed), [])
                    return
                }
                self = .independent(String(name))
                return
            }
        }
        return nil
    }
}

struct Evidence {
    let positive: Double
    let total: Double
}

struct FrequencyInterval {
    let lower: Double
    let upper: Double
}

extension TruthValue {
    public func hash(into hasher: inout Hasher) {
        hasher.combine(frequency)
        hasher.combine(confidence)
    }
}

extension Evidence {
    init(_ positive: Double, _ total: Double) {
        self.positive = positive
        self.total = total
    }
    init(_ positive: Int, _ total: Int) {
        self.positive = Double(positive)
        self.total = Double(total)
    }
    var negative: Double { total - positive }
    var frequency: Double { positive / total }
    var confidence: Double { total / (total + evidentialHorizon) }
    var lowerFrequency: Double { positive / (total + evidentialHorizon) }
    var upperFrequency: Double { (positive + evidentialHorizon) / (total + evidentialHorizon) }
    var truthValue: TruthValue { TruthValue(frequency, confidence) }
}

extension TruthValue {
    public init(_ frequency: Double, _ confidence: Double, _ rule: Rules! = nil) {
        self.frequency = rounded(frequency)
        self.confidence = rounded(confidence)
        self.rule = rule
    }
    init(_ ev: Evidence, _ rule: Rules! = nil) {
        self.frequency = rounded(ev.frequency)
        self.confidence = rounded(ev.confidence)
        self.rule = rule
    }
}

extension FrequencyInterval {
    init(_ lower: Double, _ upper: Double) {
        self.lower = lower
        self.upper = upper
    }
    var ignorance: Double { upper - lower }
    var positiveEvidence: Double { evidentialHorizon * lower / ignorance }
    var totalEvidence: Double { evidentialHorizon * (1 - ignorance) / ignorance }
    var frequency: Double { lower / (1 - ignorance) }
    var confidence: Double { 1 - ignorance }
}
