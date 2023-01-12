
public struct Narsese {
    public let parser: Parser
    public init() throws {
        let grammar = try Grammar(ebnf: grammar, start: "exp")
        parser = EarleyParser(grammar: grammar)
    }
    
    public func parse(_ s: String) throws -> ParseTree {
        try parser.syntaxTree(for: s)
    }
    
    public let grammar = """
        exp              = '<', (statement | term), '>';

        statement        = term, space, copula, space, term;
        
        copula           = '->' | '<->' | '=>' | '<=>'
                           | '•->' | '->•' | '•->•'
                           | '/=>' | '\\\\=>' | '|=>' | '/<=>' | '|<=>'
        ;

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

        term             = word | variable | exp | statement | compound;

        variable         = indep-var | dep-var | query-var;
        
        indep-var        = '#', word;
        dep-var          = '#', [word, '(', [{indep-var|','|space}], ')'];
        query-var        = '?', [word];
        
        word             = {letter|digit|_};

        space            = [{' '}];
        digit            = '0' ... '9';
        letter           = 'A' ... 'Z' | 'a' ... 'z';
        """
}
@_exported import NAL

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
	/// 	<pattern1> ::= <alternative1> | <alternative2>
	///		<pattern2> ::= 'con' 'catenation'
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
	/// 	<pattern1> ::= <alternative1> | <alternative2>
	///		<pattern2> ::= 'con' 'catenation'
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
	///		A -> a | A B
	///		B -> a | B b
	///
	/// and a string "ab"
	///
	/// The tokenizer generates the tokenization
	///
	///		[[a], [b]]
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
	///		A -> a | A B
	///		B -> a | B b
	///
	/// and a string "ab"
	///
	/// The tokenizer generates the tokenization
	///
	///		[[a], [b]]
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
///		"A" ~~> n("B") <|> t("x")
/// 				 	^ generates a production result
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
	/// 	A -> B C
	/// 	D -> x
	/// 	Start -> empty
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
	/// 	pattern1 = alternative1 | alternative2;
	///		pattern2 = 'con', 'catenation';
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
///		<expr> ::= <expr> '+' <expr> | 'a'
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
	///		<non-terminal-pattern> ::= <produced-non-terminal-pattern> | 'terminal' <concatenated-non-terminal>
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
	///		non-terminal pattern = produced non-terminal | 'terminal', concatenated non-terminal;
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
