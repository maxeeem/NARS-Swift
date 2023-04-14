import NAL

public struct Narsese {
    public let parser: Parser
    public let dialect: Dialect
    
    public init(dialect: Dialect) throws {
        self.dialect = dialect

        try Copula.validate(dialect)
        try Connector.validate(dialect)
        
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
        
        copula           = \(Copula.allCases.all(dialect));
        
        operation        = '(', '^', word, [seq, terms], ')';

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
        compound-neg     = '(', \(ç.neg(dialect)), (seq|space), term, ')';
        
        compound-image   = '(',
                               ('/' | '\\\\'), seq, term, seq,
                                 ((placeholder, seq, term) | (term, seq, placeholder))
                           ,')'
        ;
        
        placeholder      = 'º' | '_';
        
        connector        = \(ç.primary(dialect));
        
        connector-diff   = \(ç.diff(dialect));
        
        terms            = term, [{seq-comma, term}|{seq-space, term}];
        
        seq              = seq-comma | seq-space;
        seq-comma        = space, ',', space;
        seq-space        = space, ' ', space;

        term             = word | variable | exp | statement | compound | operation;

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
