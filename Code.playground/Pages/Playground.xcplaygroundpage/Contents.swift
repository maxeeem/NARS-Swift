
/*
 * each rock has a functionality
 * @author Nicknames
 */

import Foundation 
import PlaygroundSupport

let output = Output()

// set the view and indefinite execution
PlaygroundPage.current.needsIndefiniteExecution = true
PlaygroundPage.current.liveView = output

let robin = Term.word("robin")
let bird = Term.word("bird")
let animal = Term.word("animal")
let mammal = Term.word("mammal")

var history = [String]()
var verbose = true

let nars = NARS { s in
    history.append(s); print(s)
    if !verbose && s.contains("⏱") { return }
    usleep(100000) // 1/100th second
    output.text = "...\(output.text.suffix(200))\n" + s
}

output.callback = { s in
    DispatchQueue.global(qos: .userInitiated).asyncAfter(deadline: .now()+1) { 
        nars.perform(s)
    }
}

output.reset = {
    nars.reset()
    output.text = "..."
}

nars.perform(
    ("bird" --> "animal")-*, // (1, 0.9)
    ( robin -->  bird   )-*,
    ( bird  -->  animal )-?,
    ( bird  --> "mammal")-?,
    ( bird  -->  mammal )-*(0, 0.9),
    ( bird  -->  mammal )-?
)

nars.perform(
    ("bird" --> "?")-?,
    ("?" --> mammal)-?,
    ("?" --> "?")-?
)
/*
nars.perform(
    ("robin" --> "animal")-*,
    ("robin" --> "bird")-*
)
sleep(1)
nars.reset()
nars.perform(
    ("tiger" --> "animal")-*,
    ("tiger" --> "bird")-*(0, 0.9)
)
*/
sleep(5)
debugPrint(nars.memory)

// MARK: Tests
// print(history)
//assert(history == ["• bird -> animal<1.0, 0.9>", "• robin -> bird<1.0, 0.9>", ".  ⏱ animal -> robin<1.0, 0.4475>", "• bird -> animal?", ".  💡 bird -> animal<1.0, 0.9>", "• bird -> mammal?", ".  ⏱ robin -> mammal<1.0, 0.405>", ".  ⏱ bird -> mammal<1.0, 0.2671>", ".  ⏱ bird -> mammal?", ".  💡 bird -> mammal<1.0, 0.2671>", "• bird -> mammal<0.0, 0.9>", ".  ⏱ robin -> mammal<0.0389, 0.0316>", ".  ⏱ robin -> bird<1.0, 0.014>", "• bird -> mammal?", ".  💡 bird -> mammal<0.0389, 0.9035>", "• bird -> ?", ".  💡 bird -> animal<1.0, 0.9>", "• ? -> mammal", ".  💡 robin -> mammal<0.956, 0.4163>", "• ? -> ?", "\tI don\'t know 🤷‍♂️"])


