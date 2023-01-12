import XCTest
@testable import NARS

// Experimental

// MARK: Tests

final class NARS_Tests: XCTestCase {

//    func testBag() throws {
//        let bag = Bag<Belief>()
//        var log: [String] = []
//        DispatchQueue.global().async {
//            for _ in 0...1000 {
//                let term = Term.word("\(Int.random(in: 0...10000))")
//                bag.put(term-* + 0.9)
//                log.append("-")
//            }
//        }
//        
//        for _ in 0...1000 {
//            let term = Term.word("\(Int.random(in: 0...10000))")
//            bag.put(term-* + 0.9)
//            log.append("+")
//            usleep(100)
//        }
//        print(log)
//    }
    
    func testWrappedBag() throws {
        let bag = Bag<TermLink>()
        let bird = Term.word("bird")
        let robin = Term.word("robin")
        bag.put(TermLink(bird, 0.9))
        bag.put(TermLink(robin, 0.9))
        print(">>\n", bag, "<<\n")
        
        let wrapped = WrappedBag(bag)
        print(">>-\n", wrapped.wrapped, wrapped.bag, "-<<\n")

        let item = wrapped.get()
        print("==", item)
        print(">>\n", bag, "<<\n")

        print(">>-\n", wrapped.wrapped, wrapped.bag, "-<<\n")
        wrapped.put(TermLink(.word("dog"), 0.9))
        print(">>-\n", wrapped.wrapped, wrapped.bag, "-<<\n")

        let wrappedItem = wrapped.get("robin")
        print("==", wrappedItem)
        print(">>-\n", wrapped.wrapped, wrapped.bag, "-<<\n")
    }

    
    func testExample() throws {
        print(
            "//----------- Test Storage\n"
        )
        let bag = Bag<TermLink>()
        let bird = Term.word("bird")
        let robin = Term.word("robin")
        bag.put(TermLink(bird, 0.9))
        bag.put(TermLink(robin, 0.9))
        print(bag)
        var item = bag.get()
        print(item!)
        print(bag)
        bag.put(item!)
        print(bag)
        item = bag.get("bird")
        print(item!)
        print(bag)
        print(
            "//----------- Test Logic\n"
        )
        print(not(0.3)) // 0.7
        print(rounded(and(0.2, 0.4))) // 0.08
        print(or(0.2, 0.4)) // 0.52
        let j1: Judgement = ("robin" --> "bird")-*(1.0, 0.8)
        let j2: Judgement = ("penguin" --> "bird") -* (0.9, 0.9)
        print(
            "------choice\n",
            choice(j1: j1, j2: j2)
        ) // robin

        print(
            "//----------- Inference Tests\n"
        )
        print("deduction", Rules.deduction.firstOrder, Rules.deduction.apply((
            ("A" --> "B")-*,
            ("C" --> "A")-*
        )).contains(
            ("C" --> "B")-*(1, 0.81)) ? "pass" : XCTFail("fail"))

        print(")))", Rules.induction.apply((
            ("A" --> "B")-*,
            ("A" --> "Z")-*
        )))
        print("induction", Rules.induction.firstOrder, Rules.induction.apply((
            ("A" --> "B")-*,
            ("A" --> "Z")-*
        )).contains(
            ("Z" --> "B")-*(1, 0.4475)) ? "pass" : XCTFail("fail"))
        
        print("abduction", Rules.abduction.firstOrder, Rules.abduction.apply((
            ("A" --> "B")-*,
            ("C" --> "B")-*
        )).contains(
            ("C" --> "A")-*(1, 0.4475)) ? "pass" : XCTFail("fail"))

        let applied = Rules.exemplification.apply((("A" --> "B")-*, ("B" --> "C")-*))
        print("exemplification", Rules.exemplification.firstOrder, (applied.first == ("C" --> "A")-*(1, 0.4475) ? "pass" : XCTFail("fail")))

        print("comparison", Rules.comparison.firstOrder, Rules.comparison.apply((
            ("A" --> "B")-*,
            ("A" --> "C")-*
        )).contains(
            ("C" <-> "B")-*(1, 0.4475)) ? "pass" : XCTFail("fail"))

//        print(Rules.comparison.apply((
//            ("A" --> "B")-*,
//            ("A" --> "C")-*
//        )))

        print("\n",
            "//----------- Experimental\n"
        )
/*
        let car = Term.word("car")
        let vehicle = Term.word("vehicle")
        let inheritance = Term.word("->")
        let carIsAVehicle = Term.compound(inheritance, [car, vehicle])
        let i = Term.word("SELF")
        let know = Term.word("know")
        let iKnowCarIsAVehicle = Term.compound(know, [i, carIsAVehicle])
        print(
            "-----",
            iKnowCarIsAVehicle
        )



        let x = Term.word("VAR#x")
        let y = Term.word("VAR#y")
        let z = Term.word("VAR#z")
        let cond = Term.word("if")
//        let then = Term.word("then")
        let premise = Term.compound(
            cond,
            [.compound(inheritance, [x, y]),
             .compound(inheritance, [z, x])])
        let conclusion = Term.compound(inheritance, [z, y])
        let eval = Term.word("eval")
        (
            /// deduction
        )
        let rule = Term.compound(eval, [premise, conclusion])
        print(rule)
        let apply = Term.compound(rule, [carIsAVehicle, iKnowCarIsAVehicle])
        print(apply)

*/
        
        let result = Rules.deduction.apply(((.statement(.variable(.independent("x")), .inheritance, .symbol("M")) => .statement(.variable(.independent("x")), .inheritance, .symbol("P")))-*, ("S" --> "M")-*))
        
        print("K", result)
        
    }
    
    func testCompounds() {
        let t1 = %["{Mars}", "{Pluto}", "{Venus}"]
        
        let t2 = %["{Pluto}", "{Saturn}"]
        
        let c1 = (t1 | t2) // {Mars, Pluto, Saturn, Venus}
        let c2 = (t1 & t2) // {Pluto}
        let c3 = (t1 - t2) // {Mars, Venus}
        let c4 = (t2 ~ t1) // {Saturn}
        // TODO: finish test.
        // it relates to .terms extension where
        // for single component intSet and extSet
        // we return the term itself not its terms
        XCTAssert(c2.description == "(⋂ {Pluto})")
    }
}
