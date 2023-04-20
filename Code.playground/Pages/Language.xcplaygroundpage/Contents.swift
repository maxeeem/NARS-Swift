
extension Term {
    func is_a(_ t: Term) -> Term { .is_a(t)(self) }
    func is_an(_ t: Term) -> Term { self.is_a(t) }
    func is_a_type_of(_ t: Term) -> Term { self.is_a(t) }
    
    func `is`(_ t: Term) -> Term { .is(t)(self) }
    func has(_ t: Term) -> Term { .has(t)(self) }
    
    func and(_ t: Term) -> Term { self && t }
    func and(_ it: (Term) -> Term) -> Term {
        if case .statement(let s, _, _) = self {
            return self && it(s)
        }
        if case .compound(let c, let terms) = self {
            if c == .c, terms.count == 2 {
                if case .statement(let s, _, _) = terms.first {
//                    return .compound(c, terms + [it(s)])
                    return &&(terms + [it(s)])
                }
            }
        }
        return .NULL
    }
    
    static func is_a(_ t: Term) -> (Term) -> Term {
        { $0 --> t }
    }
    
    static func `is`(_ t: Term) -> (Term) -> Term {
        { $0 ->• t }
    }

    static func has(_ t: Term) -> (Term) -> Term {
        { $0 ->• t }
    }
}

typealias it = Term

postfix operator •
postfix func •(_ str: String) -> Term {
    Term(stringLiteral: str)
}


print(
    "bird"•.is_an("animal").and("bird"•.has("wings")), "\n",
    "bird"•.is_an("animal").and(it.has("wings")), "\n",
    
    "raven"•.is_a_type_of("bird"), "\n",
    
    "{Bart_Simpson}"•.is_a("carton_character"), "\n",
    
    "cat"•.is("furry").and(it.is_a("mammal")).and(it.has("four_legs"))
)

let dog = Term.symbol("dog")
let mammal = Term.symbol("mammal")

print(dog.is_a_type_of(mammal))

print(it.has("wings"•.and("feathers"))("bird"))
print("\n")

import NaturalLanguage

let tagger = NLTagger(tagSchemes: [.nameTypeOrLexicalClass, .lemma])
tagger.string = "dog is a fun animal that likes to run."
tagger.enumerateTags(in: tagger.string!.range, unit: .word, scheme: .nameTypeOrLexicalClass) { tag, range in
    let stemForm = tag?.rawValue ?? String(tagger.string![range])
    print("\n", "<\(tagger.string![range])>")
    let lemma = tagger.tag(at: tagger.string![range].startIndex, unit: .word, scheme: .lemma)
    print(stemForm, lemma.0?.rawValue ?? "", terminator: "\n")
    return true
}
print("\n")

tagger.enumerateTags(in: tagger.string!.range, unit: .word, scheme: .lemma) { tag, range in
    let stemForm = tag?.rawValue ?? String(tagger.string![range])
    print(tagger.string![range])
    print(stemForm, terminator: "\n")
    return true
}


extension String {
    var range: Range<String.Index> {
        startIndex..<endIndex
    }
}
/*

<(*,cat,animal) --> is>.
//<(/,is,_,animal) --> animal>.
<cat --> animal>.
<(*,dog,animal) --> is>.
<dog --> animal>?
//<(/,is,_,animal) --> animal>?

extension String {
    var range: Range<String.Index> {
        startIndex ..< endIndex
    }
}



DOG
.  cat 0.9
.  (dog ⨯ animal) 0.9
.  (/ is º animal) 0.9

---
.  <cat -> dog>. %1.00;0.45%.abd
.  <dog <–> cat>. %1.00;0.45%.com
.  <dog -> (/ is º animal)>. %1.00;0.90%.
.  <(dog ⨯ animal) -> is>. %1.00;0.81%.ana
.  <cat <–> dog>. %1.00;0.45%.com
.  <dog -> cat>. %1.00;0.45%.abd



DOG
.  (dog ⨯ animal) 0.9
.  (/ is º animal) 0.9
.  (dog ⨯ animal) -> #x0 0.9
.  (cat ⨯ animal) -> #x0 0.9
.  (cat ⨯ animal) 0.9

---
.  <((dog ⨯ animal) -> #x0) <=> ((cat ⨯ animal) -> #x0)>. %1.00;0.45%.com
.  <(dog ⨯ animal) <–> (cat ⨯ animal)>. %1.00;0.45%.com
.  <((cat ⨯ animal) -> #x0) <=> ((dog ⨯ animal) -> #x0)>. %1.00;0.45%.com
.  <(dog ⨯ animal) -> (cat ⨯ animal)>. %1.00;0.45%.abd
.  <((cat ⨯ animal) -> #x0) => ((dog ⨯ animal) -> #x0)>. %1.00;0.45%.ind
.  <(dog ⨯ animal) -> is>. %1.00;0.90%.
.  <(cat ⨯ animal) -> (dog ⨯ animal)>. %1.00;0.45%.abd
.  <((dog ⨯ animal) -> #x0) => ((cat ⨯ animal) -> #x0)>. %1.00;0.45%.ind
.  <dog -> (/ is º animal)>. %1.00;0.81%.ana
.  <(cat ⨯ animal) <–> (dog ⨯ animal)>. %1.00;0.45%.com


*/
