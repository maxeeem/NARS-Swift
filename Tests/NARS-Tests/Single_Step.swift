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
    
    lazy var nars = NARS { self.output.append($0) }
    
    override func setUpWithError() throws {
        // Put setup code here. This method is called before the invocation of each test method in the class.
        Sentence.defaultPause = 10 // in milliseconds
    }

    override func tearDownWithError() throws {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
        nars.reset()
        output.removeAll()
    }

    func testNal1_0() throws {
        /// revision
        nars.perform(
            ("bird" --> "swimmer")-*,
            ("bird" --> "swimmer")-*(0.1, 0.6),
            .pause
        )

        XCTAssert(output.contains(where: { $0.contains("<bird -> swimmer>. %0.87;0.91%") }))
    }
    
    func testNal1_1() throws {
        /// deduction
        nars.perform(
            ("bird" --> "animal")-*,
            ("robin" --> "bird")-*,
            .pause
        )

        XCTAssert(output.contains(where: { $0.contains("<robin -> animal>. %1.00;0.81%") }))
    }
    
    func testNal1_2() throws {
        /// abduction
        nars.perform(
            ("sport" --> "competition")-*,
            ("chess" --> "competition")-*(0.9, 0.9),
            .pause
        )

        XCTAssert(output.contains(where: { $0.contains("<sport -> chess>. %1.00;0.42%") }))
        XCTAssert(output.contains(where: { $0.contains("<chess -> sport>. %0.90;0.45%") }))
    }
    
    func testNal1_3() throws {
        /// induction
        nars.perform(
            ("swan" --> "swimmer")-*(0.9),
            ("swan" --> "bird")-*,
            .pause
        )

        XCTAssert(output.contains(where: { $0.contains("<bird -> swimmer>. %0.90;0.45%") }))
        XCTAssert(output.contains(where: { $0.contains("<swimmer -> bird>. %1.00;0.42%") }))
    }
    
    func testNal1_4() throws {
        /// exemplification
        nars.perform(
            ("robin" --> "bird")-*,
            ("bird" --> "animal")-*,
            .pause
        )

        XCTAssert(output.contains(where: { $0.contains("<animal -> robin>. %1.00;0.45%") }))
    }
    
    func testNal1_5() throws {
        /// conversion
        nars.perform(
            ("bird" --> "swimmer")-*,
            ("swimmer" --> "bird")-?,
            .pause
        )

        XCTAssert(output.contains(where: { $0.contains("<swimmer -> bird>. %1.00;0.47%") }))
    }

}
