import XCTest
@testable import NARS

// Experimental
import Foundation

// MARK: Tests

final class NARS_Tests: XCTestCase {
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
        print("deduction", Rules.deduction.rule, Rules.deduction.apply((
            ("A" --> "B")-*,
            ("C" --> "A")-*
        )) ==
            ("C" --> "B")-*(1, 0.81) ? "pass" : XCTFail("fail"))

        print("induction", Rules.induction.rule, Rules.induction.apply((
            ("A" --> "B")-*,
            ("A" --> "Z")-*
        )) ==
            ("Z" --> "B")-*(1, 0.4475) ? "pass" : XCTFail("fail"))
        
        print("abduction", Rules.abduction.rule, Rules.abduction.apply((
            ("A" --> "B")-*,
            ("C" --> "B")-*
        )) ==
            ("C" --> "A")-*(1, 0.4475) ? "pass" : XCTFail("fail"))

        let exemplificationRule = Rules.exemplification.apply
        let applied = exemplificationRule((("A" --> "B")-*, ("B" --> "C")-*))
        print("exemplification", Rules.exemplification.rule, (applied == ("C" --> "A")-*(1, 0.4475) ? "pass" : XCTFail("fail")))

        print("comparison", Rules.comparison.rule, Rules.comparison.apply((
            ("A" --> "B")-*,
            ("A" --> "C")-*
        )) ==
            ("C" <-> "B")-*(1, 0.4475) ? "pass" : XCTFail("fail"))

        //print(Rules.comparison.apply((
        //    ("A" --> "B")-*,
        //    ("A" --> "C")-*
        //)))

        print("\n",
            "//----------- Experimental\n"
        )

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

    }
}
