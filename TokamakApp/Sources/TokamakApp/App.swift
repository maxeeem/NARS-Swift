import TokamakDOM
import JavaScriptKit

import NARS
import Narsese

class NARS_Singleton: ObservableObject {
    @Published var verbose = false
    @Published var history = [String]()
    
    var narsese: Narsese!
    
    lazy var instance = NARS(timeProviderMs: timeProviderMs, output)

    var count = 0
    var time: UInt32 = 0

    func timeProviderMs() -> UInt32 {
        time += 1
        return time
    }
    
    func output(_ s: String) {
        print(s)
        count += 1
        if verbose == false && s.contains("⏱") { return }
        history.append("\(count) " + s);
    }
    
    func reset() {
        instance.reset()
        output("• 🧨 Reset completed!")
    }

    func defaultDialect(_ dialect: Dialect = .swift) {
        do {
            narsese = try Narsese.init(dialect: dialect)
        } catch {
            history.append("\(error)")
        }
    }
}

@main
struct TokamakApp: App {
    @StateObject var nars = NARS_Singleton()

    var body: some Scene {
        WindowGroup("Tokamak App") {
            ContentView()
                .padding(.all)
//                .background(Color.init(hex: "1e212d"))
                .environmentObject(nars)
                .onAppear {
                    nars.instance.perform(
                        ("bird" --> "animal")-*,
                        ("robin" --> "bird")-*,
                        ("robin" --> "animal")-?
                    )

                    nars.reset()

                    nars.instance.perform(
                        ("{sky}" --> "[blue]")-*,
                        ("{tom}" --> "cat")-*,
                        ("{tom}" --> ç.e_("likes", .º, "{sky}"))-*
                    )
                }
        }
    }
}


struct ContentView: View {
    @EnvironmentObject var nars: NARS_Singleton

    @State var input = "<[blue] -> (/ likes cat º)>?"
    
    let dialects = Array(Dialect.allCases.reversed())
        .filter({$0 != .ona}) // TODO: need to add mappings
    
    @State var dialect = 0 // swift

    var w: Double {
        JSObject.global.window.object!.innerWidth.number ?? 640
    }
    var h: Double {
        JSObject.global.window.object!.innerHeight.number ?? 480
    }
    
    @State var containerView: JSObject? = nil
    @State var scrollView: JSObject? = nil
    @State var inputView: JSObject? = nil
    @State var logoView: JSObject? = nil
    
    var body: some View {
        VStack {
            HStack {
                Image("icon_sm.png")
                    ._domRef($logoView)
                    .frame(width: 40)
                    .padding(.horizontal)
                    .onAppear {
                        // set initial input field size
                        _ = inputView?.size = .number(w/10)
                        
                        logoView?.onclick = .object(JSClosure({ _ in
                            _ = JSObject.global.window.location.replace("https://www.intelligentmachines.io/")
                            return .undefined
                        }))
                        
                        _ = containerView?.jsValue.addEventListener("click", JSClosure({ a in
                            // update input field size
                            _ = inputView?.size = .number(w/10)
                            dialect = dialect // cause view to re-render and adjust size
                            return .undefined
                        }))
                    }

                Spacer()
                
                Picker("Dialect ", selection: $dialect) {
                    ForEach(0..<dialects.count) {
                        Text(dialects[$0].name + ($0 == 0 ? " (default)" : ""))
                    }
                    Text("...")
                }
                
                Spacer()
                                    
                Button("🧨") {
                    nars.reset()
                }
                .buttonStyle(BorderlessButtonStyle())
            }
            .padding(.bottom, 20)

            ScrollView(showsIndicators: false) {
                ForEach(Array(zip(nars.history.indices, nars.history)), id: \.0) { (line, text) in
                    Text(text)
                        .font(.caption)
                        .frame(width: w - 20, alignment: .leading)
                        .padding(.bottom, 5)
                        .onAppear {
                            if line == nars.count-2 {
                                _ = scrollView?.jsValue.scrollIntoView(false)
                            }
                        }
                }
                ._domRef($scrollView)
                .padding(.leading)
            }
            .frame(width: w, height: h - 160, alignment: .leading)
            ._domRef($containerView)
            .padding(.bottom, 20)
            
            Group {
                Button(action: {
                    nars.verbose.toggle()
                    nars.output("• Verbose: " + (nars.verbose ? "ON" : "OFF"))
                }, label: {
                    Text(nars.verbose ? "🔈" : "🔇")
                        .padding(.horizontal)
                })
                .buttonStyle(BorderlessButtonStyle())

                TextField("Input", text: $input, onCommit: {
                    process()
                })
                .textFieldStyle(RoundedBorderTextFieldStyle())
                ._domRef($inputView)
                
                Button(action:  {
                    process()
                }, label: {
                    Text("🆗")
                        .font(.title2)
                        .padding(.horizontal)
                })
                .buttonStyle(BorderlessButtonStyle())
            }
            .frame(alignment: .center)
            .padding(.bottom, 20)
        }
    }
    
    func process() {
        if nars.narsese?.dialect != dialects[dialect] {
            nars.defaultDialect(dialects[dialect])
        }
        let s = input.trimmingCharacters(in: .whitespaces)
        if let x = Sentence(s, parser: nars.narsese) {
            nars.instance.perform(x)
        }
    }
}


// MARK: - Extensions

func contents(_ s: String) -> String {
    // TODO: parse :|: and %0.9% etc
    let start = s.index(s.startIndex, offsetBy: 0)
    let end = s.index(s.endIndex, offsetBy: -1)
    let contents = String(s[start..<end])
    return contents
}


extension Sentence {
    init?(_ s: String, parser: Narsese) {
        if let duration = Int(s) {
            self = .cycle(duration)
            return
        }
        
        var s = s
        
        var f: Double!
        var c: Double!
        
        if let start = s.firstIndex(of: "%") {
            let vals = s.suffix(from: start)
                .replacingOccurrences(of: "%", with: "")
                .components(separatedBy: ";")
            if vals.count == 1 {
                f = Double(vals[0])
            } else if vals.count == 2 {
                f = Double(vals[0])
                c = Double(vals[1])
            }
            
            s = String(s.prefix(upTo: start)).trimmingCharacters(in: .whitespaces)
        }
        
        let contents = contents(s)

        do {
            let term = try Term(contents, parser: parser)

            f = f ?? 1
            c = c ?? 0.9
            
            if s.hasSuffix(">.") {
                self = .judgement(term-*(f, c))
                return
            }
            
            if s.hasSuffix(">?") {
                self = .question(term-?)
                return
            }
            
            if s.hasSuffix(">!") {
                self = .goal(term-!)
                return
            }
            return nil
        } catch {
            print(error)
        }
        
        return nil
    }
}


extension Dialect {
    var name: String {
        switch self {
        case .canonical:
            return "Canonical"
        case .ona:
            return "ONA"
        case .opennars:
            return "OpenNARS"
        case .swift:
            return "Swift"
        }
    }
}
