//
//  Single_Step.swift
//  
//
//  Created by Maxim VT on 3/22/22.
//

import XCTest

@testable import NARS

class Single_Step: XCTestCase {

    var output: [String] = []
    
    var nars: NARS!
        
    override func setUpWithError() throws {
//        Sentence.defaultPause = 1000 // in milliseconds
//        let timeProviderMs: () -> UInt32 = { DispatchWallTime.now().rawValue }
        var time: UInt32 = 0
        var timeProviderMs: () -> UInt32 = { time += 1 ; return time }

        nars = NARS(timeProviderMs: timeProviderMs) { self.output.append($0); print($0) }
    }

    override func tearDownWithError() throws {
        nars.reset()
        output.removeAll()
    }
    
    private func outputMustContain(_ expectation: String, timeout: TimeInterval = 1) {
        func condition() -> Bool { output.contains(where: { $0.contains(expectation) }) }
        let start = Date().timeIntervalSince1970
        while ((Date().timeIntervalSince1970 - start) < timeout) && !condition() {
                usleep(1000) // wait a bit
        }
        XCTAssertTrue(condition())
    }
    
    func testSample() {
        nars.perform(
            ("{tom}" --> "cat")-*,
            (Term.compound(.x, ["{tom}", "{sky}"]) --> "likes")-*,
//            ("{tom}" --> Term.compound(.e, ["likes", .º, "{sky}"]))-*,
//            ("{sky}" --> Term.compound(.e, ["likes", "{tom}", .º]))-*,
            ("{sky}" --> "[blue]")-*,
            (Term.compound(.x, ["cat", "[blue]"]) --> "likes")-?,
            .cycle(100)
//            ("cat" --> Term.compound(.e, ["likes", .º, "[blue]"]))-?,
        )
//        outputMustContain("💡 <(cat ⨯ [blue]) -> likes>.") // c should be 0.37%
        outputMustContain("<(cat ⨯ [blue]) -> likes>.") // c should be 0.37%
    }
    /*
    func testik() {
        let relation = ç.x_("water", "salt") --> "dissolve"
        let knowledge = "rain" --> "water"
        
//        nars.perform(
//            relation-*,
//            knowledge-*,
//            .cycle
//        )
        
        let image = "water" --> ç.e_("dissolve", "º", "salt")
        nars.perform(
            image-*,
            knowledge-*,
            .cycle
        )
    }
    */
    // KK <P |=> S>. %1.00;0.45%.ind ["S+(1.0, 0.9, 16781238065391810616)", "P+(1.0, 0.9, 16781238065392859616)"]
    // KK <P |=> S>. %1.00;0.45%.ind ["S+(1.0, 0.9, 16781238065391810616)", "P+(1.0, 0.9, 16781238065378744616)"]

    // KK <S |=> P>. %1.00;0.45%.ind ["P+(1.0, 0.9, 16781238065392859616)", "S+(1.0, 0.9, 16781238065391810616)"]
    // KK <S |=> P>. %1.00;0.45%.ind ["P+(1.0, 0.9, 16781238065378744616)", "S+(1.0, 0.9, 16781238065391810616)"]
/*
    func test4() {
        nars.cycle = true
        nars.perform(
            ||("_P_"•)-*,
              ("_P_"•)-*(1.0,0.9,0),
//            .pause,
            ||("_S_"•)-*
//            .cycle
        )
//        print(nars.memory)
        nars.perform(
            ||("_P_"•)-*,
//            .pause,
            ||("_S_"•)-*
//            .cycle
        )
        nars.perform(
            ||("_P_"•)-*,//(1.0,0.9,0),
//            .pause,
            ||("_S_"•)-*
              , .pause
//            .cycle
        )
        nars.cycle = false
        print(nars.memory)
    }
    */
    
    func testNal7_00() { // nal7.0.nal
        let x = Term.variable(.independent("x"))
        let y = Term.variable(.independent("y"))
        nars.perform(
            (((x * "room_101") --> "enter") <<|=> ((x * "door_101") --> "open"))-*(0.9),
            (((x * "door_101") --> "open") <<|=> ((x * "key_101") --> "hold"))-*(0.8)
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
            (((x * "door_101") --> "open") <<|=> ((x * "key_101") --> "hold"))-*(0.8)
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
        outputMustContain("⏱ <((#x ⨯ key_101) -> hold) /=> ((#x ⨯ room_101) -> enter)>. %0.90;0.39%")
        outputMustContain("⏱ <((#x ⨯ room_101) -> enter) \\=> ((#x ⨯ key_101) -> hold)>. %0.80;0.42%")
        outputMustContain("⏱ <((#x ⨯ key_101) -> hold) /<=> ((#x ⨯ room_101) -> enter)>. %0.73;0.44%")
    }
    
    func testNal7_02() {
        let x = Term.variable(.independent("x"))
        nars.perform(
            ((("John" * "key_101") --> "hold") >>|=> (("John" * "room_101") --> "enter"))-*,
            ||(("John" * "key_101") --> "hold")-*
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
        outputMustContain("⏱ <(John ⨯ room_101) -> enter>. %1.00;0.81%")
    }
   
    func testNal7_X() {
        nars.perform(
            ((("John" * "key_101") --> "hold") >>|=> (("John" * "door_101") --> "open"))-*,
            ((("John" * "key_101") --> "hold") >>|=> (("John" * "room_101") --> "enter"))-*,
            ((("John" * "room_101") --> "enter") >>|=> (("John" * "door_101") --> "open"))-*(0, 0.9)
//            ((("John" * "room_101") --> "enter") <<|=> (("John" * "door_101") --> "open"))-*,
            )
        nars.perform(
            ||(("John" * "door_101") --> "open")-*
//            ||(("John" * "room_101") --> "enter")-*,
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
        outputMustContain("⏱ <(John ⨯ room_101) -> enter>.")// %1.00;0.81%")
    }
    
    func testMultiStep() {
        nars.perform(
            ("a" --> "b")-*,
            ("b" --> "c")-*,
//            ("a" --> "c")-?,
//            .cycle(10),
            ("u" --> "v")-*,
            ("+y" --> "+z")-*,
            ("c" --> "d")-*,
//            .cycle(5),
            ("a" --> "d")-?
//            .cycle
//            .pause
        )
        
//        print(nars.foobar)
//        print(nars.memory)
//        print("RECENT")
//        print(nars.recent)
        outputMustContain("💡 <a -> d>. %1.00;0.73%")
    }
    
    func testBackward() {
        let x = Term.$x
        nars.perform(
            (__.robin --> __.bird)-*,
            (__.robin --> __.animal)-?,
            .cycle(10),
            ("bird" --> "animal")-*,
            ("robin" --> "animal")-?, // TODO: this test should work with this line disabled
            .cycle(10)
        )
        outputMustContain("💡 <robin -> animal>. %1.00;0.81%.ded")
    }
    
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
            ("bird" --> "swimmer")-*(0.1, 0.6)
        )
        outputMustContain("<bird -> swimmer>. %0.87;0.91%")
    }
    
    func testNal1_01() throws {
        /// deduction
        nars.perform(
            ("bird" --> "animal")-*,
            ("robin" --> "bird")-*
        )
        outputMustContain("<robin -> animal>. %1.00;0.81%")
    }
    
    func testNal1_02() throws {
        /// abduction
        nars.perform(
            ("sport" --> "competition")-*,
            ("chess" --> "competition")-*(0.9, 0.9)
        )
        outputMustContain("<sport -> chess>. %1.00;0.42%")
        outputMustContain("<chess -> sport>. %0.90;0.45%")
    }
    
    func testNal1_03() throws {
        /// induction
        nars.perform(
            ("swan" --> "swimmer")-*(0.9),
            ("swan" --> "bird")-*
        )
        outputMustContain("<bird -> swimmer>. %0.90;0.45%")
        outputMustContain("<swimmer -> bird>. %1.00;0.42%")
    }
    
    func testNal1_04() throws {
        /// exemplification
        nars.perform(
            ("robin" --> "bird")-*,
            ("bird" --> "animal")-*
        )
        outputMustContain("<animal -> robin>. %1.00;0.45%")
    }
    
    func testNal1_05() throws {
        /// conversion
        nars.perform(
            ("bird" --> "swimmer")-*,
            ("swimmer" --> "bird")-?
        )
        outputMustContain("💡 <swimmer -> bird>. %1.00;0.47%")
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
            ("swan" --> "bird")-*
        )
        outputMustContain("<bird <–> swimmer>. %0.90;0.45%")
    }
 
    func testNal2_02() throws {
        /// backward inference
        nars.perform(
            ("bird" --> "swimmer")-*,
            ("{?1}" --> "swimmer")-?,
            .cycle(100)
        )
        outputMustContain("<{?1} -> bird>?")
    }
    
    func testNal2_03() throws {
       /// comparison
       nars.perform(
           ("sport" --> "competition")-*,
           ("chess" --> "competition")-*(0.9)
       )
       outputMustContain("<chess <–> sport>. %0.90;0.45%")
    }

    func testNal2_04() throws {
       /// analogy
       nars.perform(
           ("swan" --> "swimmer")-*,
           ("gull" <-> "swan")-*
       )
       outputMustContain("<gull -> swimmer>. %1.00;0.81%")
    }

    func testNal2_05() throws {
       /// analogy
       nars.perform(
           ("gull" --> "swimmer")-*,
           ("gull" <-> "swan")-*
       )
       outputMustContain("<swan -> swimmer>. %1.00;0.81%")
    }

    func testNal2_06() throws {
       /// resemblance
       nars.perform(
           ("robin" <-> "swan")-*,
           ("gull" <-> "swan")-*,
           .cycle(10)
       )
        print(nars.recent)
        print(nars.recent.items.contains(where: { $0.value.description.contains("<gull <–> robin>. %1.00;0.81%") }))
       outputMustContain("<gull <–> robin>. %1.00;0.81%")
    }
    
    func testNal2_07() throws {
       /// conversions between inheritance and similarity
       nars.perform(
           ("swan" --> "bird")-*,
           ("bird" --> "swan")-*(0.1),
           .cycle(10)
       )
//        outputMustContain("⏱ <bird <–> swan>.")// %0.10;0.81%")

        nars.perform(
           ("bird" <-> "swan")-?,
           .cycle(20)
       )
        outputMustContain("💡 <bird <–> swan>.")// %0.10;0.81%")
    }
    
//    func testTerms() {
//        let S = Term.var("S")
//        let P = Term.var("P")
//
//        let something: Term = (S <-> P) <=> (.property(S) <-> .property(P))
//
//        print(Term.getTerms(something))
//        
//    }
    
    func testNal2_08() throws {
       /// structure transformation
       nars.perform(
           ("bright" <-> "smart")-*(0.9),
           .cycle,
           ("[smart]" --> "[bright]")-?
//           .pause
       )
       outputMustContain("💡 <[smart] -> [bright]>.")// %0.90;0.66%")
    }
    
//    func testNal2_09() throws {
//       /// conversions between inheritance and similarity
//       nars.perform(
//           ("swan" --> "bird")-*,
//           ("bird" <-> "swan")-*(0.1),
//           .cycle
//       )
//       outputMustContain("<bird -> swan>.")// %0.10;0.73%")
//    }

    func testNal2_10() throws {
       /// structure transformation
       nars.perform(
           ("Birdie" <-> "Tweety")-*(0.9),
           ("{Birdie}" <-> "{Tweety}")-?
       )
       outputMustContain("💡 <{Birdie} <–> {Tweety}>. %0.90;0.81%")
    }
    
    func testNal2_11() throws {
       /// conversions between inheritance and similarity
        nars.perform(
            ("swan" --> "bird")-*(0.9),
            .cycle(100),
            ("bird" <-> "swan")-?,
            .cycle(1)
        )
       outputMustContain("💡 <bird <–> swan>.")// %0.90;0.47%")
    }
    

    func testNal2_12() throws {
        /// conversions between inheritance and similarity
        nars.perform(
            ("bird" <-> "swan")-*(0.9),
            ("swan" --> "bird")-?
        )
        outputMustContain("💡 <swan -> bird>.")// %0.90;0.81%")
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
           .cycle(10)
       )
       outputMustContain("<{Birdie} <–> {Tweety}>.")// %1.00;0.90%")
    }

    func testNal2_17() throws {
       /// set definition
       nars.perform(
           ("[smart]" --> "[bright]")-*,
           .cycle(10)
       )
       outputMustContain("<[bright] <–> [smart]>.")// %1.00;0.90%")
    }
    
    func testNal2_18() throws {
       /// set definition
        nars.perform(
           ("{Birdie}" <-> "{Tweety}")-*,
           .cycle
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
            ("swan" --> "bird")-*(0.8)
        )
        outputMustContain("<swan -> (bird ⋃ swimmer)>. %0.98;0.81%")
        outputMustContain("<swan -> (bird ⋂ swimmer)>. %0.72;0.81%")
    }

    func testNal3_01() throws {
        /// compound composition, two premises
        nars.perform(
            ("sport" --> "competition")-*(0.9),
            ("chess" --> "competition")-*(0.8)
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
//    func testNal3_3() throws {
//        /// compound decomposition, two premises
//        nars.perform(
//            ("robin" --> "swimmer")-*(0.00),
//            ("robin" --> "– mammal swimmer")-*(0.00),
//            .pause
//        )
//        output.forEach { print($0) }
//        outputMustContain("<robin -> mammal>. %0.00;0.81%") // c should be 0.81
//    }
    
    func testNal3_04() throws {
        /// set operations
        let t1 = Term.compound(.U, ["{Mars}", "{Pluto}", "{Venus}"])
        let t2 = Term.compound(.U, ["{Pluto}", "{Saturn}"])
        nars.perform(
            ("planetX" --> t1)-*(0.9),
            ("planetX" --> t2)-*(0.7)
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
            ("planetX" --> t2)-*(0.1)
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
            ("swan" --> t1)-?,
            .cycle(200)
        )
        outputMustContain("💡 <swan -> (bird ⋂ swimmer)>.") // should be %0.90;0.73%
    }

    func testNal3_09() throws {
        /// compound composition, one premise
        let t1 = Term.compound(.U, ["swan", "swimmer"])
        nars.perform(
            ("swan" --> "bird")-*(0.9),
            (t1 --> "bird")-?,
            .cycle(200)
        )
        outputMustContain("💡 <(swan ⋃ swimmer) -> bird>.") // should be %0.90;0.73%
    }

    func testNal3_10() throws {
        /// compound composition, one premise
        let t1 = Term.compound(.l, ["swimmer", "bird"])
        nars.perform(
            ("swan" --> "bird")-*(0.9),
            ("swan" --> t1)-?,
            .cycle(200)
        )
        outputMustContain("💡 <swan -> (swimmer – bird)>.") // should be %0.10;0.73%
    }

    func testNal3_11() throws {
        /// compound composition, one premise
        let t1 = Term.compound(.ø, ["swimmer", "swan"])
        nars.perform(
            ("swan" --> "bird")-*(0.9),
            (t1 --> "bird")-?,
            .cycle(200)
        )
        outputMustContain("💡 <(swimmer ø swan) -> bird>.") // should be %0.10;0.73%
    }
    
    func testVari() {
        nars.perform(
            ((.variable(.independent("x")) --> "_M") => (.variable(.independent("x")) --> "_P"))-*,
            ((.variable(.independent("x")) --> "_S") => (.variable(.independent("x")) --> "_M"))-*
        ) /// <(#x -> S) => (#x -> P)>. %1.00;0.81%.ded
        outputMustContain("⏱ <(#x -> _S) => (#x -> _P)>. %1.00;0.81%")
    }
    
    func testVari3() {
        nars.perform(
            ("_M" --> "_T1")-*,
            (&&[.variable(.dependent("x", [])) --> "_T1", (.variable(.dependent("x", [])) --> "_T2")])-*
        ) /// <_M -> _T2. %1.00;0.42%.ana
        outputMustContain("⏱ <_M -> _T2>.")
    }
    
    func testVari2() {
        nars.perform(
            ("{Tweety}" --> "_P")-*,
            ((.instance(.variable(.independent("x"))) --> "_P") => (.instance(.variable(.independent("x"))) --> "_Q"))-*
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

//    func testNal3_12() throws {
//        /// compound decomposition, one premise
//        let t1 = Term.compound(.U, ["bird", "swimmer"])
//        nars.perform(
//            ("robin" --> t1)-*(0.9),
//            .pause(100)
//        )
//        output.forEach { print($0) }
//        outputMustContain("💡 <robin -> bird>.") // should be %0.90;0.73%
//    }
    
//    func testNal4_0() throws {
//        /// structural transformations
//        let t1 = Term.compound(.x, ["acid", "base"])
//        let i1 = Term.compound(.i, <#T##[Term]#>)
//        nars.perform(
//            (t1 --> "reaction")-*
//        )
//        output.forEach { print($0) }
//    }
}
