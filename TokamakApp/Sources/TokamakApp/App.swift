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

    @State var w: Double = JSObject.global.window.object!.innerWidth.number ?? 640
    @State var h: Double = JSObject.global.window.object!.innerHeight.number ?? 480
    
    @State var scrollView: JSObject? = nil
    @State var inputView: JSObject? = nil
    @State var logoView: JSObject? = nil
    
    var body: some View {
        VStack {
            HStack {
                Image("icon_sm.png")
                    ._domRef($logoView)
                    .frame(width: 40)
                    .padding([.leading, .top])
                    .onAppear {
                        // set initial input field size
                        _ = inputView?.size = .number(w/15)
                        
                        logoView?.onclick = .object(JSClosure({ _ in
                            _ = JSObject.global.window.location.replace("https://www.intelligentmachines.io/")
                            return .undefined
                        }))
                        
                        _ = JSObject.global.window.matchMedia("(orientation: portrait)").addListener(JSClosure({ _ in
                            w = JSObject.global.window.object!.innerWidth.number ?? 640
                            h = JSObject.global.window.object!.innerHeight.number ?? 480
                            // update input field size
                            _ = inputView?.size = .number(w/15)
                            dialect = dialect // cause view to re-render and adjust size
                            return .undefined
                        }))
                    }

                Spacer()
                
                Picker("", selection: $dialect) {
                    ForEach(0..<dialects.count) {
                        Text(dialects[$0].name + ($0 == 0 ? " (default)" : ""))
                    }
                    Text("...")
                }
                .foregroundColor(.accentColor)
                
                Spacer()
                                    
                Button("🧨") {
                    nars.reset()
                }
                .buttonStyle(BorderlessButtonStyle())
            }
            .padding(.bottom, 20)

            ScrollView(showsIndicators: false) {
                LazyVGrid(columns: [GridItem()]) {
                    ForEach(nars.history, id: \.self) { text in
                        Text(text)
                            .frame(maxWidth: .infinity, alignment: .leading)
                            .padding(.bottom, 5)
                            .onAppear {
                                _ = scrollView?.jsValue.scrollIntoView(false)
                            }
                    }
                    ._domRef($scrollView)
                    .padding(.leading)
                }
            }
            .frame(width: w, height: h - 158, alignment: .leading)
            .padding(.bottom, 20)
            
            Group {
                Button(action: {
                    nars.verbose.toggle()
                    nars.output("• Verbose: " + (nars.verbose ? "ON" : "OFF"))
                }, label: {
                    Text(nars.verbose ? "🔈" : "🔇")
                        .padding(.trailing)
                })
                .buttonStyle(BorderlessButtonStyle())

                TextField("Input", text: $input, onCommit: {
                    process()
                })
                ._domRef($inputView)
                
                Button(action:  {
                    process()
                }, label: {
                    Text("▶︎")
                        .font(.title2)
                })
                .foregroundColor(.accentColor)
                .padding(.leading, 5)
                .buttonStyle(BorderedProminentButtonStyle())
            }
            .frame(alignment: .center)
            .padding(.bottom, 20)
        }
    }
    
    func process() {
        guard !input.isEmpty else { return }
        
        if nars.narsese?.dialect != dialects[dialect] {
            nars.defaultDialect(dialects[dialect])
        }
        let s = input.trimmingCharacters(in: .whitespaces)
        if let x = Sentence(s, parser: nars.narsese) {
            nars.instance.perform(x)
            input = ""
        } else {
            nars.output("Parsing error")
        }
        
        if nars.history.count > 50 { // trim for efficiency
            nars.history = ["..."] + nars.history.suffix(50)
        }
        
        _ = JSObject.global.document.activeElement.blur()
        _ = JSObject.global.window.scrollTo(["top": 0])
        _ = scrollView?.jsValue.scrollIntoView(false)
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
        
        var tense: Tense? = nil
        
        if s.hasSuffix(":\\:") {
            tense = .past
            s = s.replacingOccurrences(of: ":\\:", with: "").trimmingCharacters(in: .whitespaces)
        }
        if s.hasSuffix(":|:") {
            tense = .present
            s = s.replacingOccurrences(of: ":|:", with: "").trimmingCharacters(in: .whitespaces)
        }
        if s.hasSuffix(":/:") {
            tense = .future
            s = s.replacingOccurrences(of: ":/:", with: "").trimmingCharacters(in: .whitespaces)
        }
        
        let contents = contents(s)

        do {
            let term = try Term(contents, parser: parser)

            f = f ?? 1
            c = c ?? 0.9
            
            if s.hasSuffix(">.") {
                let j = Judgement(term, TruthValue(f, c), tense: tense)
                self = .judgement(j)
                return
            }
            
            if s.hasSuffix(">?") {
                let q = Question(term, .truth, tense)
                self = .question(q)
                return
            }
            
            if s.hasSuffix(">!") {
                let g = Goal(term, DesireValue(f, c))
                self = .goal(g)
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
