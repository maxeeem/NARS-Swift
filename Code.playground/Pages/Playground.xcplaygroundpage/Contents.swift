
/*
 * each rock has a functionality
 * @author Nicknames
 */

import Foundation 
import PlaygroundSupport

var verbose = false

let output = Output()
output.isVerbose = verbose
output.onVerbose = { verbose = $0 }

// set the view and indefinite execution
PlaygroundPage.current.needsIndefiniteExecution = true
PlaygroundPage.current.liveView = output

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

let defaultScript = [
    ("bird" --> "animal")-*, // (1, 0.9)
    ( robin -->  bird   )-*,
    .pause, // 1/4 seconds 
    ( bird  -->  animal )-?,
    ( bird  --> "mammal")-?,
    .pause,
    ( bird  -->  mammal )-*(0, 0.9),
    ( bird  -->  mammal )-?,
    .pause,
    ("bird" --> "?")-?,
    ("?"    -->  mammal)-?,
    ("?"    --> "?")-?,
    .pause
]

// dispatch execution to background thread
// to update the user interface
DispatchQueue.global().async { 
    
    var timestamp = Date().timeIntervalSinceReferenceDate
        
    nars.perform(defaultScript)
    
    debugPrint(nars.memory)
        
    output.reset()
 
    nars.perform(
        (robin-->bird)-*,
        (robin-->animal)-?,
        .pause(50)
    )
    
    timestamp = Date().timeIntervalSinceReferenceDate - timestamp
    print(timestamp)
//    sleep(5)
    debugPrint(nars.memory)
    //print(nars.pendingTasks)

        
    output.reset()
    
    nars.perform(
        (bird-->animal)-*,
        ("{Tweety}"-->bird)-*,
        ("{Tweety}"-->animal)-?,
        .pause
    )
    
    debugPrint(nars.memory)
 
    output.text += "\ndone\n..."
    
}


// MARK: Tests
// print(history)
//assert(history == ["• bird -> animal<1.0, 0.9>", "• robin -> bird<1.0, 0.9>", ".  ⏱ animal -> robin<1.0, 0.4475>", "• bird -> animal?", ".  💡 bird -> animal<1.0, 0.9>", "• bird -> mammal?", ".  ⏱ robin -> mammal<1.0, 0.405>", ".  ⏱ bird -> mammal<1.0, 0.2671>", ".  ⏱ bird -> mammal?", ".  💡 bird -> mammal<1.0, 0.2671>", "• bird -> mammal<0.0, 0.9>", ".  ⏱ robin -> mammal<0.0389, 0.0316>", ".  ⏱ robin -> bird<1.0, 0.014>", "• bird -> mammal?", ".  💡 bird -> mammal<0.0389, 0.9035>", "• bird -> ?", ".  💡 bird -> animal<1.0, 0.9>", "• ? -> mammal", ".  💡 robin -> mammal<0.956, 0.4163>", "• ? -> ?", "\tI don\'t know 🤷‍♂️"])

