
/*
 * each rock has a functionality
 * @author Nicknames
 */

import Foundation 
import PlaygroundSupport

// set the view and indefinite execution
let vc = MyViewController()

PlaygroundPage.current.needsIndefiniteExecution = true
PlaygroundPage.current.liveView = vc

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
    vc.text = "...\(vc.text.suffix(300))\n\n" + s
}

vc.callback = { s in
    DispatchQueue.global(qos: .userInitiated).asyncAfter(deadline: .now() + 1) { 
        nars.perform(s)
    }
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

sleep(1)
debugPrint(nars.memory)

// MARK: Tests
// print(history)
//assert(history == ["• bird -> animal<1.0, 0.9>", "• robin -> bird<1.0, 0.9>", ".  ⏱ animal -> robin<1.0, 0.4475>", "• bird -> animal?", ".  💡 bird -> animal<1.0, 0.9>", "• bird -> mammal?", ".  ⏱ robin -> mammal<1.0, 0.405>", ".  ⏱ bird -> mammal<1.0, 0.2671>", ".  ⏱ bird -> mammal?", ".  💡 bird -> mammal<1.0, 0.2671>", "• bird -> mammal<0.0, 0.9>", ".  ⏱ robin -> mammal<0.0389, 0.0316>", ".  ⏱ robin -> bird<1.0, 0.014>", "• bird -> mammal?", ".  💡 bird -> mammal<0.0389, 0.9035>", "• bird -> ?", ".  💡 bird -> animal<1.0, 0.9>", "• ? -> mammal", ".  💡 robin -> mammal<0.956, 0.4163>", "• ? -> ?", "\tI don\'t know 🤷‍♂️"])


