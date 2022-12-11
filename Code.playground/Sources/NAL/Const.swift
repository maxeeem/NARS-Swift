//  “The systems show different “personalities” when predicting the future,
//   and larger k corresponds to more conservative and risk-averse behavior.”

public let evidentialHorizon: Double = 1 // "personality parameter"

public let occamsRazor: Int = 1
public let reliance: Double = 0.9


public var k: Double { evidentialHorizon }
public var r: Int { occamsRazor }


public let ETERNAL: UInt64 = 18446744073709551615 // DispatchTime.distantFuture

