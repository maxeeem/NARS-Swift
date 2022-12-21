public struct Narsese {
    public let parser: Parser
    public let dialect: Dialect
    
    public init(dialect: Dialect = .swift) throws {
        self.dialect = dialect

        let allValid: Bool = {
            // TODO: check copulas for possible conflicts
            let connectors = Connector.allCases.flatMap { $0.variants[dialect]! }
            let connectorsValid = (connectors.count == Set(connectors).count)
            if !connectorsValid {
                print("ERROR: Duplicate connectors for Narsese dialect `\(dialect)`")
                print(connectors)
            }
            return connectorsValid
        }()

        guard allValid else {
            enum DialectParsingError: Error {
                case containsDuplicates
            }
            throw DialectParsingError.containsDuplicates
        }
        
        let ebnf = Narsese.grammar(dialect)
        let grammar = try Grammar(ebnf: ebnf, start: "exp")
        
        self.parser = EarleyParser(grammar: grammar)
    }
    
    public func parse(_ s: String) throws -> ParseTree {
        try parser.syntaxTree(for: s)
    }
    
    public static func grammar(_ dialect: Dialect) -> String {
        """
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
        compound-neg     = '(', (\(ç.n.all(dialect))), (seq|space), term, ')';
        
        compound-image   = '(',
                               ('/' | '\\\\'), seq, term,
                                 ((terms, 'º', terms)
                                 | (terms, 'º', [terms])
                                 | ([terms], 'º', terms))
                           ,')'
        ;
        
        connector        = \([ç.Ω, .U, .x, .c, .d, .s, .p].map({$0.all(dialect)}).joined(separator: "|"));
        
        connector-diff   = \([ç.l, .ø].map({$0.all(dialect)}).joined(separator: "|"));
        
        terms            = term, [{seq-comma, term}|{seq-space, term}];
        
        seq              = seq-comma | seq-space;
        seq-comma        = space, ',', space;
        seq-space        = space, ' ', space;
        
        term             = word | variable | exp | statement | compound;
        
        variable         = indep-var | dep-var | query-var;
        
        indep-var        = '#', word;
        dep-var          = '#', [word, '(', [{indep-var|','|space}], ')'];
        query-var        = '?', [word];
        
        word             = {letter|digit|'_'};
        
        space            = [{' '}];
        digit            = '0' ... '9';
        letter           = 'A' ... 'Z' | 'a' ... 'z';
        """
    }
}
