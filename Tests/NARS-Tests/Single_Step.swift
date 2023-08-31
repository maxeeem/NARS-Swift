//
//  Single_Step.swift
//  
//
//  Created by Maxim VT on 3/22/22.
//

import XCTest
import Darwin
@testable import NARS

var verbose = true

class Single_Step: XCTestCase {

    var output: [String] = []
    
    var nars: NARS!
    
    var __ : NARS { nars } // alias

    override func setUpWithError() throws {
//        Sentence.defaultPause = 1000 // in milliseconds
//        let timeProviderMs: () -> UInt32 = { DispatchWallTime.now().rawValue }
        var time: UInt32 = 0
        let timeProviderMs: () -> UInt32 = { time += 1 ; return time }

        nars = NARS(timeProviderMs: timeProviderMs) { self.output.append($0) ; if verbose { print($0) } }
    }

    override func tearDownWithError() throws {
        nars.reset()
        output.removeAll()
//        usleep(1000000)
    }
    
    private func outputMustContain(_ expectation: String, timeout: TimeInterval = 1) {
        func condition() -> Bool { output.contains(where: { $0.contains(expectation) }) }
        let start = Date().timeIntervalSince1970
        while ((Date().timeIntervalSince1970 - start) < timeout) && !condition() {
                usleep(1000) // wait a bit
        }
        XCTAssertTrue(condition())
    }
    
    
    func testNal7_00() { // nal7.0.nal
        let x = Term.variable(.independent("x"))
        let y = Term.variable(.independent("y"))
        nars.perform(
            (((x * "room_101") --> "enter") <<|=> ((x * "door_101") --> "open"))-*(0.9),
            (((x * "door_101") --> "open") <<|=> ((x * "key_101") --> "hold"))-*(0.8),
            .cycle(200)
        )
        /*
         '********** temporal deduction/explification

         'Someone enter the room_101 after he open the door_101
         <<(*, $x, room_101) --> enter> =\> <(*, $x, door_101) --> open>>. %0.9%

         'Someone open the door_101 after he hold the key_101
         <<(*, $y, door_101) --> open> =\> <(*, $y, key_101) --> hold>>. %0.8%

         100

         'If someone enter room_101, he should hold key_101 before
         ''outputMustContain('<<(*,$1,room_101) --> enter> =\> <(*,$1,key_101) --> hold>>. %0.72;0.58%')
         'If someone hold key_101, he will enter room_101
         ''outputMustContain('<<(*,$1,key_101) --> hold> =/> <(*,$1,room_101) --> enter>>. %1.00;0.37%')
         */
        outputMustContain("⏱ <((#x ⨯ room_101) -> enter) \\=> ((#x ⨯ key_101) -> hold)>. %0.72;0.58%")
        outputMustContain("⏱ <((#x ⨯ key_101) -> hold) /=> ((#x ⨯ room_101) -> enter)>. %1.00;0.37%")
    }
    
    func testNal7_01() {
        let x = Term.variable(.independent("x"))
        nars.perform(
            (((x * "door_101") --> "open") >>|=> ((x * "room_101") --> "enter"))-*(0.9),
            (((x * "door_101") --> "open") <<|=> ((x * "key_101") --> "hold"))-*(0.8),
            .cycle(200)
        )

        /*
         '********** temporal induction/comparison

         'Someone open door_101 before he enter room_101
         <<(*, $x, door_101) --> open> =/> <(*, $x, room_101) --> enter>>. %0.9%

         'Someone open door_101 after he hold key_101
         <<(*, $y, door_101) --> open> =\> <(*, $y, key_101) --> hold>>. %0.8%

         100

         'If someone hold key_101, he will enter room_101
         ''outputMustContain('<<(*,$1,key_101) --> hold> =/> <(*,$1,room_101) --> enter>>. %0.90;0.39%')
         'If someone enter room_101, he should hold key_101 before
         ''outputMustContain('<<(*,$1,room_101) --> enter> =\> <(*,$1,key_101) --> hold>>. %0.80;0.42%')
         'If someone hold key_101, it means he will enter room_101
         ''outputMustContain('<<(*,$1,key_101) --> hold> </> <(*,$1,room_101) --> enter>>. %0.73;0.44%')

         */
        outputMustContain("<((#x ⨯ key_101) -> hold) /=> ((#x ⨯ room_101) -> enter)>.")// %0.90;0.39%")
        outputMustContain("<((#x ⨯ room_101) -> enter) \\=> ((#x ⨯ key_101) -> hold)>.")// %0.80;0.42%")
        outputMustContain("<((#x ⨯ key_101) -> hold) /<=> ((#x ⨯ room_101) -> enter)>.")// %0.73;0.44%")
    }
    
    func testNal7_02() {
        nars.perform(
            ((("John" * "key_101") --> "hold") >>|=> (("John" * "room_101") --> "enter"))-*,
            ||(("John" * "key_101") --> "hold")-*
//            .cycle(10)
        )
        /*
         '********** inference on tense

         'John hold key_101 before he enter room_101
         <<(*,John,key_101) --> hold> =/> <(*,John,room_101) --> enter>>.

         'John is holding key_101 now
         <(*,John,key_101) --> hold>. :|:

         20

         'John will enter the room_101
         ''outputMustContain('<(*,John,room_101) --> enter>. :!5: %1.00;0.81%')
         
         */
        outputMustContain("⏱ anticipate <(John ⨯ room_101) -> enter>.")// %1.00;0.81%")
    }
   
    func testNal7_X() {
        nars.perform(
            ((("John" * "key_101") --> "hold") >>|=> (("John" * "door_101") --> "open"))-*,
            ((("John" * "key_101") --> "hold") >>|=> (("John" * "room_101") --> "enter"))-*,
            .cycle(40),
            ||(("John" * "door_101") --> "open")-*,
            .cycle(10)
        )
        /*
         
         M =/> P
         M =/> S
         
         P =/> S ?
         S =/> P ?
         
         <<(*,John,key_101) --> hold> =/> <(*,John,door_101) --> open>>.
         <<(*,John,key_101) --> hold> =/> <(*,John,room_101) --> enter>>.
         10
         <(*,John,door_101) --> open>. :|:
         
         <M =/> P>. :|:
         <M =/> S>. :|:
         
         */
        outputMustContain("⏱ anticipate <(John ⨯ room_101) -> enter>.")// %1.00;0.81%")
    }
    
    
//    func testLoop() {
//        // match> (#x1 <–> #x0) <=> (#x0 -> #x1)  __  ({#x0} <–> {#x1}) <=> ({#x0} -> {#x1})
//
//        let t1 = ("#x1" <-> "#x0") <=> ("#x0" --> "#x1")
//        let t2 = ("{#x0}" <-> "{#x1}") <=> ("{#x0}" --> "{#x1}")
//        let res = Term.logic_match(t1: t1, t2: t2)
//        print(res)
//    }
    
    
    func testMultipleQuestions() {
        nars.perform(
            ("bird" --> "animal")-*,
            ("dog" --> "person")-?,
//            .cycle(10),
            ("o" --> "x")-?,
//            .cycle(10),
            ("bird" --> "[flying]")-*,
            ("bird" --> "animal")-?,
//            .cycle(10),
            ("dog" --> "person")-*,
            .cycle(100)
        )
        outputMustContain("💡 <bird -> animal>.")
        outputMustContain("💡 <dog -> person>. %1.00;0.90%.")
    }
    
    func testMultiStep() {
        nars.perform(
            ("a" --> "b")-*,
            ("b" --> "c")-*,
            ("u" --> "v")-*,
            ("y" --> "z")-*,
            ("c" --> "d")-*,
            .cycle(200)
//            ("a" --> "d")-?
        )
        outputMustContain("<a -> d>.")// %1.00;0.73%")
    }
    /*
    func testBackward() {
        let x = Term.$x
        nars.perform(
            (__.robin --> __.bird)-*,
            (__.robin --> __.animal)-?,
            .cycle(10),
            ("bird" --> "animal")-*,
//            ("robin" --> "animal")-?, // TODO: this test should work with this line disabled
            .cycle(10)
        )
        outputMustContain("💡 <robin -> animal>.")// %1.00;0.81%.ded")
    }
    */
//    func testCycle() throws {
//        nars.cycle = true
//        nars.perform(
//            ("swan" --> "bird")-*,
//            ("bird" --> "swan")-*(0.1)
////           ("bird" <-> "swan")-?
//            //.pause(5000)
//        )
//        while true {
//        }
//    }
    
    func testNal1_00() throws {
        /// revision
        nars.perform(
            ("bird" --> "swimmer")-*,
            ("bird" --> "swimmer")-*(0.1, 0.6),
            .cycle(5)
        )
        outputMustContain("<bird -> swimmer>. %0.87;0.91%")
    }
    
    func testNal1_01() throws {
        /// deduction
        nars.perform(
            ("bird" --> "animal")-*,
            ("robin" --> "bird")-*,
            .cycle(40)
        )
        outputMustContain("<robin -> animal>. %1.00;0.81%")
    }
    
    func testNal1_02() throws {
        /// abduction
        nars.perform(
            ("sport" --> "competition")-*,
            ("chess" --> "competition")-*(0.9, 0.9),
            .cycle(40)
        )
        outputMustContain("<sport -> chess>.")// %1.00;0.42%")
        outputMustContain("<chess -> sport>.")// %0.90;0.45%")
    }
    
    func testNal1_03() throws {
        /// induction
        nars.perform(
            ("swan" --> "swimmer")-*(0.9),
            ("swan" --> "bird")-*,
            .cycle(40)
        )
        outputMustContain("<bird -> swimmer>.")// %0.90;0.45%")
        outputMustContain("<swimmer -> bird>.")// %1.00;0.42%")
    }
    
    func testNal1_04() throws {
        /// exemplification
        nars.perform(
            ("robin" --> "bird")-*,
            ("bird" --> "animal")-*,
            .cycle(40)
        )
        outputMustContain("<animal -> robin>. %1.00;0.45%")
    }
    
    func testNal1_05() throws {
        /// conversion
        nars.perform(
            ("bird" --> "swimmer")-*,
            ("swimmer" --> "bird")-?
        )
        outputMustContain("<swimmer -> bird>. %1.00;0.47%")
    }

    func testNal1_06() throws {
        /// y/n question
        nars.perform(
            ("bird" --> "swimmer")-*,
            ("bird" --> "swimmer")-?
        )
        outputMustContain("💡 <bird -> swimmer>. %1.00;0.90%")
    }
    
    func testNal1_07() throws {
        /// what? question
        nars.perform(
            ("bird" --> "swimmer")-*(1, 0.8),
            ("?" --> "swimmer")-?
        )
        outputMustContain("💡 <bird -> swimmer>. %1.00;0.80%")
    }
    
//    func testNal1_8() throws {
//        /// backward inference
//        nars.perform(
//            ("bird" --> "swimmer")-*(1, 0.8),
//            ("?1" --> "swimmer")-?,
//            .cycle
//        )
//        outputMustContain("<?1 -> bird>.")
//        outputMustContain("<bird -> ?1>.")
//    }
    
    func testNal2_00() throws {
        /// revision
        nars.perform(
            ("robin" <-> "swan")-*,
            ("robin" <-> "swan")-*(0.1, 0.6)
        )
        outputMustContain("<robin <–> swan>. %0.87;0.91%")
    }

    func testNal2_01() throws {
        /// comparison
        nars.perform(
            ("swan" --> "swimmer")-*(0.9),
            ("swan" --> "bird")-*,
            .cycle(40)
        )
        outputMustContain("<bird <–> swimmer>. %0.90;0.45%")
    }
 
    func testNal2_02() throws {
        /// backward inference
        nars.perform(
            ("bird" --> "swimmer")-*,
            ("{?1}" --> "swimmer")-?,
            .cycle(40)
        )
        outputMustContain("<{?1} -> bird>?")
    }
    
    func testNal2_03() throws {
       /// comparison
       nars.perform(
           ("sport" --> "competition")-*,
           ("chess" --> "competition")-*(0.9),
           .cycle(40)
       )
       outputMustContain("<chess <–> sport>. %0.90;0.45%")
    }

    func testNal2_04() throws {
       /// analogy
       nars.perform(
           ("swan" --> "swimmer")-*,
           ("gull" <-> "swan")-*,
           .cycle(40)
       )
       outputMustContain("<gull -> swimmer>. %1.00;0.81%")
    }

    func testNal2_05() throws {
       /// analogy
       nars.perform(
           ("gull" --> "swimmer")-*,
           ("gull" <-> "swan")-*,
           .cycle(40)
       )
       outputMustContain("<swan -> swimmer>. %1.00;0.81%")
    }

    func testNal2_06() throws {
       /// resemblance
       nars.perform(
           ("robin" <-> "swan")-*,
           ("gull" <-> "swan")-*,
           .cycle(40)
       )
       outputMustContain("<gull <–> robin>. %1.00;0.81%")
    }
    
    func testNal2_07() throws {
       /// conversions between inheritance and similarity
       nars.perform(
           ("swan" --> "bird")-*,
           ("bird" --> "swan")-*(0.1),
           .cycle(10)
       )
       outputMustContain("<bird <–> swan>. %0.10;0.81%")
    }
    
    func testNal2_08() throws {
       /// structure transformation
       nars.perform(
           ("bright" <-> "smart")-*(0.9),
           .cycle(20),
           ("[smart]" --> "[bright]")-?
       )
       outputMustContain("💡 <[smart] -> [bright]>. %0.90;0.66%")
    }
    
    func testNal2_09() throws {
       /// conversions between inheritance and similarity
       nars.perform(
           ("swan" --> "bird")-*,
           ("bird" <-> "swan")-*(0.1),
           .cycle(20)
       )
       outputMustContain("<bird -> swan>. %0.10;0.81%") // %0.10;0.73%"
    }

    func testNal2_10() throws {
       /// structure transformation
       nars.perform(
           ("Birdie" <-> "Tweety")-*(0.9),
           ("{Birdie}" <-> "{Tweety}")-?,
           .cycle(40)
       )
       outputMustContain("💡 <{Birdie} <–> {Tweety}>. %0.90;0.66%") // %0.90;0.81%
    }
    
    func testNal2_11() throws {
       /// conversions between inheritance and similarity
        nars.perform(
            ("swan" --> "bird")-*(0.9),
            .cycle(10),
            ("bird" <-> "swan")-?,
            .cycle(100)
        )
       outputMustContain("<bird <–> swan>.")// %0.90;0.47%")
    }
    

    func testNal2_12() throws {
        /// conversions between inheritance and similarity
        nars.perform(
            ("bird" <-> "swan")-*(0.9),
            ("swan" --> "bird")-?
        )
        outputMustContain("💡 <swan -> bird>. %0.90;0.81%")
    }
    
    func testNal2_13() throws {
       /// translating instance into inheritance
       nars.perform(
           ("Tweety" •-> "bird")-*
       )
       outputMustContain("<{Tweety} -> bird>. %1.00;0.90%")
    }
    
    func testNal2_14() throws {
       /// translating property into inheritance
       nars.perform(
           ("raven" ->• "black")-*
       )
       outputMustContain("<raven -> [black]>. %1.00;0.90%")
    }
    
    func testNal2_15() throws {
       /// translating instance-property into inheritance
       nars.perform(
           ("Tweety" •->• "yellow")-*
       )
       outputMustContain("<{Tweety} -> [yellow]>. %1.00;0.90%")
    }
    
    func testNal2_16() throws {
       /// set definition
       nars.perform(
           ("{Tweety}" --> "{Birdie}")-*,
           .cycle(20),
           ("{Birdie}" <-> "{Tweety}")-?
       )
       outputMustContain("<{Birdie} <–> {Tweety}>.")// %1.00;0.90%")
    }

    func testNal2_17() throws {
       /// set definition
       nars.perform(
           ("[smart]" --> "[bright]")-*,
           .cycle(20),
           ("[bright]" <-> "[smart]")-?
       )
       outputMustContain("<[bright] <–> [smart]>.")// %1.00;0.90%")
    }
    
    func testNal2_18() throws {
       /// set definition
        nars.perform(
           ("{Birdie}" <-> "{Tweety}")-*,
           ("{Tweety}" --> "{Birdie}")-?,
           .cycle(40)
        )
        outputMustContain("<Birdie <–> Tweety>.")// %1.00;0.90%")
        outputMustContain("<{Tweety} -> {Birdie}>.")// %1.00;0.90%")
    }
    
//    func testNal2_19() throws {
//       /// set definition
//       nars.perform(
//           ("[bright]" <-> "[smart]")-*,
//           .pause
//       )
//      print(output)
//       outputMustContain("<bright <-> smart>. %1.00;0.90%")
//       outputMustContain("<[bright] --> [smart]>. %1.00;0.90%")
//    }

    func testNal3_00() throws {
        /// compound composition, two premises
        nars.perform(
            ("swan" --> "swimmer")-*(0.9),
            ("swan" --> "bird")-*(0.8),
            .cycle(1000)
        )
        outputMustContain("<swan -> (bird ⋃ swimmer)>. %0.98;0.81%")
        outputMustContain("<swan -> (bird ⋂ swimmer)>. %0.72;0.81%")
    }

    func testNal3_01() throws {
        /// compound composition, two premises
        nars.perform(
            ("sport" --> "competition")-*(0.9),
            ("chess" --> "competition")-*(0.8),
            .cycle(1000)
        )
        outputMustContain("<(chess ⋃ sport) -> competition>. %0.72;0.81%")
        outputMustContain("<(sport ⋂ chess) -> competition>. %0.98;0.81%")
    }

//    func testNal3_2() throws {
//        /// compound decomposition, two premises
//        nars.perform(
//            ("robin" --> "⋂ bird swimmer")-*,
//            ("robin" --> "swimmer")-*(0.00),
//            .pause
//        )
//        output.forEach { print($0) }
//        outputMustContain("<robin -> bird>. %1.00;0.81%") // c should be 0.81
//    }
//
    func testNal3_3() throws {
        /// compound decomposition, two premises
        nars.perform(
            (-("robin" --> "swimmer"))-*,
            (-("robin" --> ("mammal" - "swimmer")))-*,
            .cycle(40)
        )
        outputMustContain("<¬(robin -> mammal)>. %1.00;0.81%")
//        print(nars.memory)
    }
    
//    func test_() {
//        /// a:  (#x1() <–> (swimmer – #x1()) ∧ (swimmer – #x1()) -> #x1())
//        /// b: ((swimmer – #x0()) <–> #x0() ∧ (swimmer – #x0()) -> #x0())
//        let a: Term = .compound(.c, [
//            .statement(
//                .variable(.dependent("x1", [])),
//                .similarity,
//                .compound(.l, ["swimmer", .variable(.dependent("x1", []))])
//            ),
//            .statement(
//                .compound(.l, ["swimmer", .variable(.dependent("x1", []))]),
//                .inheritance,
//                .variable(.dependent("x1", []))
//            ),
//        ])
//        let b: Term = .compound(.c, [
//            .statement(
//                .compound(.l, ["swimmer", .variable(.dependent("x0", []))]),
//                .similarity,
//                .variable(.dependent("x0", []))
//            ),
//            .statement(
//                .compound(.l, ["swimmer", .variable(.dependent("x0", []))]),
//                .inheritance,
//                .variable(.dependent("x0", []))
//            ),
//        ])
//        
//        _ = Term.logic_match(t1: a, t2: b) // causes a crash
//    }
    
    func testNal3_04() throws {
        /// set operations
        let t1 = Term.compound(.U, ["{Mars}", "{Pluto}", "{Venus}"])
        let t2 = Term.compound(.U, ["{Pluto}", "{Saturn}"])
        nars.perform(
            ("planetX" --> t1)-*(0.9),
            ("planetX" --> t2)-*(0.7),
            .cycle(200)
        )
        outputMustContain("<planetX -> (⋃ {Mars} {Pluto} {Saturn} {Venus})>. %0.97;0.81%")
        outputMustContain("<planetX -> (⋂ {Pluto})>. %0.63;0.81%")
    }
    
    func testNal3_05() throws {
        /// set operations
        let t1 = Term.compound(.U, ["{Mars}", "{Pluto}", "{Venus}"])
        let t2 = Term.compound(.U, ["{Pluto}", "{Saturn}"])
        nars.perform(
            ("planetX" --> t1)-*(0.9),
            ("planetX" --> t2)-*(0.1),
            .cycle(200)
        )
        outputMustContain("<planetX -> (⋃ {Mars} {Pluto} {Saturn} {Venus})>. %0.91;0.81%")
        outputMustContain("<planetX -> ({Mars} ⋃ {Venus})>. %0.81;0.81%")
    }
//
//    func testNal3_6() throws {
//        /// composition on both sides of a statement
//        let t1 = Term.compound(.U, ["bird", "swimmer"])
//        let t2 = Term.compound(.U, ["animal", "swimmer"])
//        nars.perform(
//            ("bird" --> "animal")-*(0.9),
//            (t1 --> t2)-?,
//            .pause
//        )
//        output.forEach { print($0) }
//        outputMustContain("<(bird ⋃ swimmer) -> (animal ⋃ swimmer)>. %0.90;0.73%")
//    }
    
//    func testNal3_7() throws {
//        /// composition on both sides of a statement
//        let t1 = Term.compound(.l, ["swimmer", "animal"])
//        let t2 = Term.compound(.l, ["swimmer", "bird"])
//        nars.perform(
//            ("bird" --> "animal")-*(0.9),
//            (t1 --> t2)-?,
//            .pause
//        )
//        output.forEach { print($0) }
//        outputMustContain("<(swimmer – animal) -> (swimmer – bird)>. %0.90;0.73%")
//    }
    
    func testNal3_08() throws {
        /// compound composition, one premise
        let t1 = Term.compound(.Ω, ["bird", "swimmer"])
        nars.perform(
            ("swan" --> "bird")-*(0.9),
            ("swan" --> t1)-?
        )
        outputMustContain("💡 <swan -> (bird ⋂ swimmer)>.")// %0.90;0.73%"
    }

    func testNal3_09() throws {
        /// compound composition, one premise
        let t1 = Term.compound(.U, ["swan", "swimmer"])
        nars.perform(
            ("swan" --> "bird")-*(0.9),
            (t1 --> "bird")-?
        )
        outputMustContain("💡 <(swan ⋃ swimmer) -> bird>.")// %0.90;0.73%"
    }

    func testNal3_10() throws {
        /// compound composition, one premise
        let t1 = Term.compound(.l, ["swimmer", "bird"])
        nars.perform(
            ("swan" --> "bird")-*(0.9),
            ("swan" --> t1)-?,
            .cycle(100)
        )
        outputMustContain("💡 <swan -> (swimmer – bird)>.")// %0.10;0.73%")
    }

    func testNal3_11() throws {
        /// compound composition, one premise
        let t1 = Term.compound(.ø, ["swimmer", "swan"])
        nars.perform(
            ("swan" --> "bird")-*(0.9),
            (t1 --> "bird")-?,
            .cycle(100)
        )
        outputMustContain("💡 <(swimmer ø swan) -> bird>.")// %0.10;0.73%")
    }
    
    func testVari() {
        nars.perform(
            ((.variable(.independent("x")) --> "_M") => (.variable(.independent("x")) --> "_P"))-*,
            ((.variable(.independent("x")) --> "_S") => (.variable(.independent("x")) --> "_M"))-*,
            .cycle(200)
        ) /// <(#x -> S) => (#x -> P)>. %1.00;0.81%.ded
        outputMustContain("⏱ <(#x -> _S) => (#x -> _P)>. %1.00;0.81%")
    }
    
//    func testVari3() {
//        nars.perform(
//            ("_M" --> "_T1")-*,
//            (&&[.variable(.dependent("x", [])) --> "_T1", (.variable(.dependent("x", [])) --> "_T2")])-*,
//            .cycle(40)
//        ) /// <_M -> _T2. %1.00;0.42%.ana
//        outputMustContain("⏱ <_M -> _T2>.")
//    }
    
    func testVari2() {
        nars.perform(
            ("{Tweety}" --> "_P")-*,
            ((.instance(.variable(.independent("x"))) --> "_P") => (.instance(.variable(.independent("x"))) --> "_Q"))-*,
            .cycle(100)
        ) /// <{Tweety} -> Q>. %1.00;0.81%.ded
        outputMustContain("⏱ <{Tweety} -> _Q>. %1.00;0.81%")
    }
    /*
    func testVari3() {
        let j1: Judgement = ("{Tweety}" --> "_P")-*
        let j2: Judgement = (.instance(.variable(.independent("x"))) --> "_P")-* //=> (.instance(.variable(.independent("x"))) --> "_Q"))-*
        
        let k: Judgement = (.instance(.variable(.independent("x"))) --> "_Q")-*
        
        for sol in solve(j1.statement.logic() === j2.statement.logic() || j1.statement.logic() === k.statement.logic()) {
            print("\n---SOL---\n", sol, "\n")
//            let ts = (rule.0.terms + rule.1.terms + rule.2.terms).flatMap { $0.terms.map({ $0.logic() }) }
//            let valid = sol.allSatisfy { (v, _) in
//                ts.contains { $0.equals(v) }
//            }
//
//            if valid {
//                for item in sol {
//                    // TODO: filter out invalid substitutions
//
//                    result = result.replace(termName: item.LogicVariable.name, term: .from(logic: item.LogicTerm))
//                    //                print("result\n", result)
//                }
//            }
        }
        
//        print("final\n", result)
    }
    */
//    func testNal3_111() throws {
//        nars.perform(
//            ("a" --> "b")-*,
//            ("b" --> "c")-*,
//            ("c" --> "d")-*,
//            ("a" --> "d")-?,
//            .pause(100)
//        )
//        output.forEach { print($0) }
//        outputMustContain("<a -> d>.") // should be %0.10;0.73%
//    }

    func testNal3_12() throws {
        /// compound decomposition, one premise
        let t1 = Term.compound(.Ω, ["bird", "swimmer"])
        nars.perform(
            ("robin" --> t1)-*(0.9),
            .cycle(100)
        )
        outputMustContain("<robin -> bird>.")// %0.90;0.73%"
    }
    
    func testNal4_0() throws {
        /// structural transformations
        let t1 = Term.compound(.x, ["acid", "base"])
        nars.perform(
            (t1 --> "reaction")-*,
            .cycle(100)
        )
        outputMustContain("<acid -> (/ reaction º base)>.")
        outputMustContain("<base -> (/ reaction acid º)>.")
    }
    
    func testNal5_01() throws {
        nars.perform(
            (("robin" --> "bird") => ("robin" --> "animal"))-*,
            (("robin" --> "[flying]") => ("robin" --> "bird"))-*,
            .cycle(100)
        )
        outputMustContain("<(robin -> [flying]) => (robin -> animal)>. %1.00;0.81%")
    }
}
