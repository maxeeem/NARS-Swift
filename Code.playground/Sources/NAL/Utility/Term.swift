
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
                return .instance(.symbol(value.word))
            }

            if value.first == "[" {
                return .property(.symbol(value.word))
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
