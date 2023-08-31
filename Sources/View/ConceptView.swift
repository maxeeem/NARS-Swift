
import NAL
import SwiftUI


@main
@available(iOS 14.0, *)
@available(macOS 11.0, *)
struct NARSViewApp: App {
    typealias __ = Term
    
    @State var memory: [Term] = []
    @State var env = ENV()
    
    @State var lastAction: Term?
    
    @State var cycle: Int = 0
    
    let cycleQueue = DispatchQueue(label: "Working Cycle")

    @State var pause = false
    
    var body: some Scene {
        WindowGroup {
            Group {
                ForEach(0..<6) { line in
                    HStack {
                        Term("💱")
                        ForEach(0..<3) { term in
                            env.screen[line][term]
                        }
                        Term("💱")
                    }
                    .font(.system(size: 50))
                }
                
                Text(verbatim: "CYCLES: \(cycle)")
                
                HStack {
                    Button {
                        lastAction = " \\ "
                    } label: {
                        __.left
                    }
                    .keyboardShortcut(KeyEquivalent.leftArrow)
                    
                    Button {
                        lastAction = " / "
                    } label: {
                        __.right
                    }
                    .keyboardShortcut(KeyEquivalent.rightArrow)
                }
            }
            .environmentObject(env)
            .onAppear {
                cycleQueue.async {
                    while true {
                        cycle += 1
                        
                        step()
                        
                        usleep(200000)
                    }
                }
                DispatchQueue.main.asyncAfter(deadline: .now() + 5) {
                    print(NSApplication.shared.windows)
                    
                    NSApplication.shared.activate(ignoringOtherApps: true)
                    NSApplication.shared.windows.first?.makeKey()
                    NSApplication.shared.windows.first?.orderFrontRegardless()
                }
            }
        }
    }
    
    func step() {
        let action: Term
        if lastAction != nil {
            action = lastAction!
            lastAction = nil
        } else {
            action = .SELF//env.actions.randomElement()!
            lastAction = action
            if action == .SELF {
//                                narsy.perform((snapshot >>|=> .operation("move", [.SELF, "[forward]"])))
//                                _ = narsy.operations["move"]?([.SELF, "[forward]"])
//                    narsy.perform((.operation("move", [.SELF])))
            }
            if action == " \\ " {
//                                narsy.perform((snapshot >>|=> .operation("move", [.SELF, "[left]"])))
//                                _ = narsy.operations["move"]?([.SELF, "[left]"])
//                    narsy.perform((.operation("move", [.SELF, "[left]"])))
            }
            if action == " / " {
//                                narsy.perform((snapshot >>|=> .operation("move", [.SELF, "[right]"])))
//                                _ = narsy.operations["move"]?([.SELF, "[right]"])
//                    narsy.perform((.operation("move", [.SELF, "[right]"])))
            }
            
//                away += 1 // temp
            
//                usleep(150000)
//                            narsy.perform(.cycle(10))
        }
        
        if env.screen[5][0] != .NULL && env.screen[5][0]  != " • " {
            env.screen[5][0] = action
        }
        
        if env.screen[5][1] != .NULL && env.screen[5][1]  != " • " {
            env.screen[5][1] = action
        }
        
        if env.screen[5][2] != .NULL && env.screen[5][2]  != " • " {
            env.screen[5][2] = action
        }
        
//                        cycle += 1
//                        usleep(200000)

//            print("\(env.screen[0].line)\n\(env.screen[1].line)\n\(env.screen[2].line)\n\(env.screen[3].line)\n\(env.screen[4].line)\n\(env.screen[5].line) SCORE: \(away)  \(ratio)")

        
        if env.screen[5][0] == " / " {
            if env.screen[4][1] != .NULL {
                env.screen[4][1] = " % "
            } else {
                env.screen[4][1] = .SELF
            }
        }
        
        
        if env.screen[5][1] == " \\ " {
            if env.screen[4][0] != .NULL {
                env.screen[4][0] = " % "
            } else {
                env.screen[4][0] = .SELF
            }
        }
        
        if env.screen[5][1] == " / " {
            if env.screen[4][2] != .NULL {
                env.screen[4][2] = " % "
            } else {
                env.screen[4][2] = .SELF
            }
        }
        
        
        if env.screen[5][2] == " \\ " {
            if env.screen[4][1] != .NULL {
                env.screen[4][1] = " % "
            } else {
                env.screen[4][1] = .SELF
            }
        }
        
        
        
        if env.screen[5][0] == .SELF || env.screen[5][0] == " \\ " || env.screen[5][0] == " % " {
            if env.screen[4][0] != .NULL {
                env.screen[4][0] = " % "
            } else {
                env.screen[4][0] = .SELF
            }
        }
        
        if env.screen[5][1] == .SELF || env.screen[5][1] == " % " {
            if env.screen[4][1] != .NULL {
                env.screen[4][1] = " % "
            } else {
                env.screen[4][1] = .SELF
            }
        }
        if env.screen[5][2] == .SELF || env.screen[5][2] == " / " || env.screen[5][2] == " % " {
            if env.screen[4][2] != .NULL {
                env.screen[4][2] = " % "
            } else {
                env.screen[4][2] = .SELF
            }
        }
        
        
        
        _ = Term.compound(.x, env.screen.removeLast())
        
        if env.screen[0] != empty {
            env.screen.insert(empty, at: 0)
        } else {
            env.screen.insert(lines.randomElement()!, at: 0)
        }
    }
}


@available(iOS 13.0, *)
@available(macOS 10.15, *)
class ENV: ObservableObject {
    var screen: [Line] = [
        [" • ", .NULL, .NULL],
        [.NULL, " • ", .NULL],
        [.NULL, .NULL, " • "],
        [.NULL, .NULL, .NULL],
        [.NULL, .NULL, .NULL],
        [.NULL, .SELF, .NULL]
    ]
    var actions: [Term] = [" \\ ", .SELF, " / "]
}


@available(iOS 13.0.0, *)
@available(macOS 10.15.0, *)
extension Term: View {
    public var body: some View {
        if self == "NULL" {
            return Text("➰")
        }
        if self == "•" {
            return Text("🌨️")
        }
        if ["SELF", "\\", "/"].contains(self) {
            return Text("⛄️")
        }
        if self == "%" {
            return Text("☃️")
        }
        return Text(verbatim: description)
    }
}


extension Term: Identifiable {
    public var id: String { description }
}


typealias Line = [Term]

let empty: Line = [.NULL, .NULL, .NULL]

let lines: [Line] = [
    [.NULL, .NULL, .NULL],
    [.NULL, .NULL, .NULL],
    [.NULL, .NULL, .NULL],
    [" • ", .NULL, .NULL],
    [.NULL, " • ", .NULL],
    [.NULL, .NULL, " • "],
]

