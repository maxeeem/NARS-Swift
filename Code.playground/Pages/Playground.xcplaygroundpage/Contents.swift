
import Foundation 
import PlaygroundSupport

var verbose = true

let output = Output()
output.isVerbose = verbose
output.onVerbose = { verbose = $0 }

// set the view and indefinite execution
PlaygroundPage.current.needsIndefiniteExecution = true
//PlaygroundPage.current.liveView = output

let robin = Term.word("robin")
let bird = Term.word("bird")
let animal = Term.word("animal")
let mammal = Term.word("mammal")

var history = [String]()

let nars = NARS { s in
    if !verbose && (s.contains("⏱") || s.contains("💤")) { return }
    history.append(s); print(s)
    //usleep(100000) // 1/100th second
    output.text = "...\(output.text.suffix(600))\n" + s
}

output.callback = { s in
    nars.perform(s, .pause)
}

output.reset = {
    nars.reset()
    output.text += "\n/// memory reset"
}

Sentence.defaultPause = 1000 // set default pause duration (ms)

let defaultScript = [
    ("bird" --> "animal")-*, // (1, 0.9)
    ( robin -->  bird   )-*,
    .pause,
    ( bird  -->  animal )-?,
    ( bird  --> "mammal")-?,
    .pause(5000),
    ( bird  -->  mammal )-*(0, 0.9),
    ( bird  -->  mammal )-?,
    .pause,
    ("bird" --> "?")-?,
    ("?"    -->  mammal)-?,
//    ("?"    --> "?")-?,
    .pause
]

// dispatch execution to background thread
// to update the user interface
DispatchQueue.global().async { 
    
//    var timestamp = Date().timeIntervalSinceReferenceDate
        
    nars.perform(defaultScript)
    
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
 
    let t1 = Term.compound(.U, ["{Mars}", "{Pluto}", "{Venus}"])
    
    let t2 = Term.compound(.U, ["{Pluto}", "{Saturn}"])
    
        print("\n", t1, "\n", t2, "\n")
    
    let c1 = ç.U_(t1, t2) // {Mars, Pluto, Saturn, Venus}
    let c2 = ç.Ω_(t1, t2) // {Pluto}
    let c3 = ç.l_(t1, t2) // {Mars, Venus}
    let c4 = ç.ø_(t2, t1) // {Saturn}
    
    print(c1, c1.complexity, "\n", 
          c2, c2.complexity, "\n", 
          c3, c3.complexity, "\n", 
          c4, c4.complexity, "\n")
    
    let c5 = ç.U_("{Earth}", t2)
    print(c5, c5.complexity)
    
    let x = ç.U_("[yellow]", "bird")
    print(x, x.complexity)
    let y = ç.Ω_("dog", "cat")
    print(y, y.complexity)
    let z = ç.l_("[yellow]", "bird")
    print(z, z.complexity)
    print(z.simplicity)
    
    let l = Term(stringLiteral: "blue")
    print(l)
    
    output.reset()
    
    let relation = ç.x_("water", "salt") --> "dissolve"
    let knowledge = "rain" --> "water"
    
    nars.perform(
        relation-*,
        knowledge-*,
        .pause
    )
    
    output.reset()
    
    let image = "water" --> ç.e_("dissolve", "º", "salt")
    nars.perform(
        image-*,
        knowledge-*,
        .pause
    )
    /*
    output.reset()
    
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
    
    /// structure transformation
    nars.perform(
        ("Birdie" <-> "Tweety")-*(0.9),
        ("{Birdie}" <-> "{Tweety}")-?,
        .pause
    )
*/
}


// MARK: Tests
// print(history)
//assert(history == ["• bird -> animal<1.0, 0.9>", "• robin -> bird<1.0, 0.9>", ".  ⏱ animal -> robin<1.0, 0.4475>", "• bird -> animal?", ".  💡 bird -> animal<1.0, 0.9>", "• bird -> mammal?", ".  ⏱ robin -> mammal<1.0, 0.405>", ".  ⏱ bird -> mammal<1.0, 0.2671>", ".  ⏱ bird -> mammal?", ".  💡 bird -> mammal<1.0, 0.2671>", "• bird -> mammal<0.0, 0.9>", ".  ⏱ robin -> mammal<0.0389, 0.0316>", ".  ⏱ robin -> bird<1.0, 0.014>", "• bird -> mammal?", ".  💡 bird -> mammal<0.0389, 0.9035>", "• bird -> ?", ".  💡 bird -> animal<1.0, 0.9>", "• ? -> mammal", ".  💡 robin -> mammal<0.956, 0.4163>", "• ? -> ?", "\tI don\'t know 🤷‍♂️"])

