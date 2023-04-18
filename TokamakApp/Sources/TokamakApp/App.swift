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
                .background(Color.init(hex: "1e212d"))
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

    var body: some View {
        Group {
            HStack {
                Image("icon_sm.png")
                    .frame(width: 40)
                    .padding(.horizontal)

                Spacer()
                
                Picker("Dialect ", selection: $dialect) {
                    Text("")
                    ForEach(0..<dialects.count) {
                        Text(dialects[$0].name + ($0 == 0 ? " (default)" : ""))
                    }
                }
                
                Spacer()
                                    
                Button("🧨") {
                    nars.reset()
                }
                .buttonStyle(BorderlessButtonStyle())
            }
            .padding(.bottom, 20)

            GeometryReader { _ in
                ScrollView(showsIndicators: false) {
                    ForEach(nars.history, id: \.self) { line in
                        Text(line)
                            .font(.subheadline)
                            .frame(maxWidth: .infinity, alignment: .leading)
                    }
                    .padding(.leading)
                }
                .frame(width: w, height: h - 130, alignment: .leading)
                .padding(.bottom, 20)
            }
            
            HStack {
                Button(nars.verbose ? "🔈" : "🔇") {
                    nars.verbose.toggle()
                    nars.output("• Verbose: " + (nars.verbose ? "ON" : "OFF"))
                }
                .buttonStyle(LinkButtonStyle())
                Spacer()
                Group {
                    TextField("Input", text: $input, onCommit: {
                        process()
                    })
                    
                    Button("↵") {
                        process()
                    }
                    .buttonStyle(BorderedButtonStyle())
                }
                Spacer()
            }
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
