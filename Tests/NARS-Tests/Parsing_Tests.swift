import XCTest
@testable import Narsese

final class Parsing_Tests: XCTestCase {

    var narsese: Narsese!
    
    override func setUpWithError() throws {
        // Put setup code here. This method is called before the invocation of each test method in the class.
        narsese = try Narsese(dialect: .swift)
    }

    func testExample() throws {
        // This is an example of a functional test case.
        // Use XCTAssert and related functions to verify your tests produce the correct results.
        // Any test you write for XCTest can be annotated as throws and async.
        // Mark your test throws to produce an unexpected failure when your test encounters an uncaught error.
        // Mark your test async to allow awaiting for asynchronous code to complete. Check the results with assertions afterwards.
        let term = try Term.init("<a -> b>", parser: narsese)
        XCTAssertEqual(term, ("a" --> "b"))
    }

    func testOperation() {
        var op = "(*,{SELF}) --> ^left"
        
        let comp = op.components(separatedBy: " --> ")
        
        if comp.count == 2 { // reformat operation syntax
            if comp[0].hasPrefix("(*") && comp[1].hasPrefix("^") {
                op = comp[0].replacingOccurrences(of: "*", with: comp[1])
            }
        }
        
        XCTAssertEqual(op, "(^left,{SELF})")
    }
//    func testPerformanceExample() throws {
//        // This is an example of a performance test case.
//        self.measure {
//            // Put the code you want to measure the time of here.
//        }
//    }

}
