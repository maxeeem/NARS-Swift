import TokamakDOM

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
    
    var body: some View {
        VStack {
            HStack {
                Text("NARSY")
                    .foregroundColor(.orange)
                    .padding(.all)

                Spacer()
                
                Picker("Dialect ", selection: $dialect) {
                    Text("")
                    ForEach(0..<dialects.count) {
                        Text(dialects[$0].name + ($0 == 0 ? " (default)" : ""))
                    }
                }
                
                Spacer()
                
                Button("🧨 Reset") {
                    nars.reset()
                }
                .frame(maxWidth: 100)
            }

            VStack { Spacer() }

            ScrollView {
                ForEach(nars.history, id: \.self) { line in
                    Text(line)
                        .font(.footnote)
                        .frame(maxWidth: .infinity, alignment: .leading)
                }
            }

            VStack { Spacer() }
            
            HStack {
                Button(nars.verbose ? "⏱ Verbose" : "⏱ Quiet") {
                    nars.verbose.toggle()
                }
                .frame(maxWidth: 100)
                
                Spacer()
                
                TextField("Input", text: $input, onCommit: {
                    process()
                })
                
                Spacer(minLength: 20)
                
                Button("Submit") {
                    process()
                }
                .foregroundColor(.blue)
                
                Spacer()
            }
            
            VStack { Spacer() }
        }
        .padding(.all)
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
        
        let contents = contents(s)

        do {
            let term = try Term(contents, parser: parser)

            if s.hasSuffix(">.") {
                self = .judgement(term-*)
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
