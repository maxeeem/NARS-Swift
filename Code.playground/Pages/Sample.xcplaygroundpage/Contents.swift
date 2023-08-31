import PlaygroundSupport

PlaygroundPage.current.needsIndefiniteExecution = true

var verbose = true
var history = [String]()

var time: UInt32 = 0
let timeProviderMs: () -> UInt32 = { time += 1 ; return time }

let nars = NARS(timeProviderMs: timeProviderMs) { s in
    if !verbose && (s.hasPrefix(".") || s.contains("💤")) { return }
    history.append(s); print(s)
}

var __ : NARS { nars }

__.perform(
    ("dog" --> "animal")-*,
    ("Sandy" --> "dog")-*,
    .cycle(100)
)

func rep(_ t: Term) -> Term {
    ç.e_("represent", .º, t)
}

__.register("ask") { ts in
    if let question = ts.first {
        nars.perform(question-?)
    }
    return .operation("ask", ts)
}
        /*
__.perform((*["$x", "dormas"] --> rep("$x" --> "[dormas]")))
        
__.perform(*["Sandy", "dormas"])

__.perform((("kiu" --> "$x") --> rep(.operation("ask", ["?kiu" --> "$x"]))))
                
__.perform(((*["kiu", "dormas"]) --> rep("?"))-?)
__.perform(.cycle(20))
*/

let robin = Term.word("robin")
let bird = Term.word("bird")
let animal = Term.word("animal")
let mammal = Term.word("mammal")

let defaultScript = [
    ("bird" --> "animal")-*, // (1, 0.9)
    ( robin -->  bird   )-*,
    ( bird  -->  animal )-?,
    ( bird  --> "mammal")-?,
    .cycle(100),
    ( bird  -->  mammal )-*(0, 0.9),
    ( bird  -->  mammal )-?,
//    .cycle(10),
    ("bird" --> "?")-?,
    ("?"    -->  mammal)-?
//    ("?"    --> "?")-?,
]
    
//    var timestamp = Date().timeIntervalSinceReferenceDate
    /*
    let frame = 1...3
    let pixels: [Term] = frame.map { .symbol("pixel\($0)") }
    
    let figures = [0.0, 1.0, 0.0]
    let intensities: [Term] = zip(pixels, figures).map { .symbol("pixel\($0)") }
    */
//    let one: Term = .compound(.x, pixels)
    
//    nars.perform(
//        ()
//    )
    
//    output.reset()


//    nars.perform(defaultScript)
    

    //debugPrint(nars.memory)
        
//    output.reset()
        /*
    nars.perform(
        (robin-->bird)-*,
        (robin-->animal)-?,
        .pause//(15000)
    )
 */
//    timestamp = Date().timeIntervalSinceReferenceDate - timestamp
//    print(timestamp)
//    sleep(5)
    //debugPrint(nars.memory)
    //print(nars.pendingTasks)

/*
    output.reset()
 
    nars.perform(
        (bird-->animal)-*,
        ("{Tweety}"-->bird)-*,
        ("{Tweety}"-->animal)-?,
        .pause
    )
    
//    debugPrint(nars.memory)
        
    output.reset()
        
    nars.perform(
        (robin<->"swallow")-*,
        ("lark"<->robin)-*,
        .pause
    )
    
    output.text += "\ndone\n..."
 
    let t1 = %["{Mars}", "{Pluto}", "{Venus}"]
    
    let t2 = %["{Pluto}", "{Saturn}"]
    
        print("\n", t1, "\n", t2, "\n")
    
    let c1 = (t1 | t2) // {Mars, Pluto, Saturn, Venus}
    let c2 = (t1 & t2) // {Pluto}
    let c3 = (t1 - t2) // {Mars, Venus}
    let c4 = (t2 ~ t1) // {Saturn}
    
    print(c1, c1.complexity, "\n", 
          c2, c2.complexity, "\n", 
          c3, c3.complexity, "\n", 
          c4, c4.complexity, "\n")
    
    let c5 = ("{Earth}" | t2)
    print(c5, c5.complexity)
    
    let x = ("[yellow]" | "bird")
    print(x, x.complexity)
    let y = ("dog" & "cat")
    print(y, y.complexity)
    let z = ("[yellow]" - "bird")
    print(z, z.complexity)
    print(z.simplicity)
    
    let l = Term(stringLiteral: "blue")
    print(l)
    
    output.reset()
    
    let relation = ("water" * "salt") --> "dissolve"
    let knowledge = "rain" --> "water"
    
    nars.perform(
        relation-*,
        knowledge-*,
        .pause
    )
    
    output.reset()
*/
/*
    let image = "water" --> ç.e_("dissolve", "º", "salt")
    nars.perform(
        image-*,
        knowledge-*,
        .pause
    )
    
    output.reset()
        /*
    nars.perform(
        ("bird"• ->• "[has_wings]"•)-*,
        ("bird"• ->• "[can_fly]"•)-*,
        ("bird"• --> "animal"•)-*,
        ("dog"• --> "animal"•)-*,
        ("dog"• ->• "[can_fly]"•)-?,
        .pause
    )
    
    output.reset()
    
    nars.perform(
        ("sport"• --> "competition"•)-*,
        ("chess"• --> "competition"•)-*,
        .pause,
//        ("chess"• --> "sport"•)-*(1.0, 0.4475),
        .pause
    )
    
    output.reset()
    
    nars.perform(
        ("robin"• --> "animal"•)-*,
        ("robin"• --> "bird"•)-*,
        .pause,
//        ("chess"• --> "sport"•)-*(1.0, 0.4475),
        .pause
    )
    
    output.reset()
    
    nars.perform(
        ("tiger"• --> "animal"•)-*,
        ("tiger"• --> "bird"•)-*(0, 0.9),
        .pause,
//        ("chess"• --> "sport"•)-*(1.0, 0.4475),
        .pause
    )
 
    output.reset()
    
    let u = Teoremas.allCases.flatMap {
        $0.rules.compactMap { $0("robin" <-> "swan") }
    }
    print("\n\n\n\n", u)
    
    let k = Rules.deduction.apply((u[0]-*(1,1), ("robin" <-> "swan")-*))
    print("\n\n", k)
     */
    /// structure transformation
    nars.perform(
        ("Birdie" <-> "Tweety")-*(0.9),
        ("{Birdie}" <-> "{Tweety}")-?,
        .cycle
    )

//    nars.perform(
//        ("swan" --> "bird")-*(0.9),
//        ("bird" <-> "swan")-?,
//        .pause
//    )
    
    
    /*
     
     output.reset()
    print("\n\n\n\n\n")
//    nars.cycle = true
    
    nars.perform(
        ("swan" --> "bird")-*,
        ("bird" --> "swan")-*(0.1),
        .cycle,
        .pause
//           ("bird" <-> "swan")-?
        //.pause(5000)
    )
    print(nars.memory)
//    nars.cycle = false
    output.reset()
    /// set definition
    nars.perform(
        ("{Tweety}" --> "{Birdie}")-*,
        .cycle(5)
    )
    output.reset()
    print("\n\n\n\n\n")
    
    /// set definition
    nars.perform(
        ("[smart]" --> "[bright]")-*,
        .cycle(5)
    )
    
    sleep(2)
    
    output.reset()
    print("\n\n\n\n\n")
    
    /// set definition
    nars.perform(
        ("{Birdie}" <-> "{Tweety}")-*,
        .cycle(5)
    )
//    nars.perform(
//        (.compound(.x, ["A1", "B1"]) --> "opposite")-*,
//        (.compound(.x, ["B1", "A1"]) --> "opposite")-*,
//        ((.compound(.x, [.variable(.independent("1")), .variable(.independent("2"))]) --> "opposite") <=> (.compound(.x, [.variable(.independent("2")), .variable(.independent("1"))]) --> "opposite"))-?
//    )
    sleep(2)
 
/*
    output.reset()
    print("\n\n\n\n\n")
    
    nars.perform(
        ("a" --> "b")-*,
        ("b" --> "c")-*,
        ("c" --> "d")-*,
        .cycle(1),
        ("a" --> "d")-?,
        .pause
    )
     */
    let s1: Judgement = (
        .statement(.variable(.independent("x")),
            .inheritance,
            .symbol("M"))
        =>
            .statement(.variable(.independent("x")),
                .inheritance,
                .symbol("P"))
    )-*
    
    let s2: Judgement = ("S" --> "M")-*
    
    let res1 = Rules.deduction.apply((s1, s2))
    print("O1", s1)
    print("K1", res1)
    
     */
    /*
    let s3: Judgement = ("M" --> "T1")-*
    
    let s4: Judgement = (
        .compound(.c, [
            .statement(
                .variable(.dependent("x", [])),
                .inheritance,
                .symbol("T1")),
            .statement(
                .variable(.dependent("x", [])),
                .inheritance,
                .symbol("T2"))
        ])
    )-*
    
    let res2 = Rules.comparison.apply((s3, s4))
    
    print("O2", s4)
    print("K2", res2)

    
    output.reset()
    print("\n\n\n\n\n")

    nars.perform(
        Sentence(s3),
        Sentence(s4),
        .cycle(1)
    )
    
    print(nars.memory)
    
     */
    output.reset()
    print("\n\n\n\n\n")
    
    
    nars.perform(
        ((.variable(.independent("x")) --> "M") => (.variable(.independent("x")) --> "P"))-*,
        ((.variable(.independent("x")) --> "S") => (.variable(.independent("x")) --> "M"))-*,
        .cycle
    ) /// <(#x -> S) => (#x -> P)>. %1.00;0.81%.ded
    
    output.reset()
    print("\n\n\n\n\n")
    
    nars.perform(
        ("P" --> "Q")-*,
        ("{Tweety}" --> "P")-*,
        ((.instance(.variable(.independent("x"))) --> "P") => (.instance(.variable(.independent("x"))) --> "Q"))-*
    ) /// <{Tweety} -> Q>. %1.00;0.81%.ded
 
    output.reset()
    print("\n\n\n\n\n")

    nars.perform(
        ("M" --> "T1")-*,
        ("M" --> "T2")-*
//        .cycle
    )
    
    let test: Statement = .compound(.n, [.compound(.n, [.symbol("T")])])
    print("1.", test)
    
    let result = Theorems.apply(test-*)
    print("2.", result)
 */

// MARK: Tests
// print(history)
//assert(history == ["• bird -> animal<1.0, 0.9>", "• robin -> bird<1.0, 0.9>", ".  ⏱ animal -> robin<1.0, 0.4475>", "• bird -> animal?", ".  💡 bird -> animal<1.0, 0.9>", "• bird -> mammal?", ".  ⏱ robin -> mammal<1.0, 0.405>", ".  ⏱ bird -> mammal<1.0, 0.2671>", ".  ⏱ bird -> mammal?", ".  💡 bird -> mammal<1.0, 0.2671>", "• bird -> mammal<0.0, 0.9>", ".  ⏱ robin -> mammal<0.0389, 0.0316>", ".  ⏱ robin -> bird<1.0, 0.014>", "• bird -> mammal?", ".  💡 bird -> mammal<0.0389, 0.9035>", "• bird -> ?", ".  💡 bird -> animal<1.0, 0.9>", "• ? -> mammal", ".  💡 robin -> mammal<0.956, 0.4163>", "• ? -> ?", "\tI don\'t know 🤷‍♂️"])
