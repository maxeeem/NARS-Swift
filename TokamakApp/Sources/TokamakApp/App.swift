import TokamakDOM

import NARS
import Narsese

class NARS_Singleton: ObservableObject {
    var verbose = true
    @Published var history = [String]()
    
    var narsese: Narsese!
    
    init() {
        do {
            narsese = try Narsese.init(dialect: .swift)
        } catch {
            history.append("\(error)")
        }
    }

    var count = 0
    var time: UInt32 = 0
    lazy var timeProviderMs: () -> UInt32 = { self.time += 1 ; return self.time }

    lazy var instance = NARS(timeProviderMs: timeProviderMs) { [unowned self] s in
        if self.verbose == false && s.contains("⏱") { return }
//        DispatchQueue.main.async {
            self.count += 1
            self.history.append("\(self.count) " + s);
//        }
//        print(s)
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
                        .cycle(40)
                    )
                }
        }
    }
}


struct ContentView: View {
    @EnvironmentObject var nars: NARS_Singleton

    @State var input = ""
    
    var body: some View {
        VStack {
            Text("Hello, \(nars.instance.name)!")
            Spacer()
            ZStack(alignment: .topTrailing) {
                ScrollView {
//                    ScrollViewReader { value in
                        ForEach(nars.history, id: \.self) { line in
                            Text(line)
                                .font(.footnote)
                                .frame(maxWidth: .infinity, alignment: .leading)
                        }
//                        .onChange(of: nars.history.count) { newValue in
//                            value.scrollTo(nars.history[newValue - 1])
//                        }
//                    }
                }
                Button("🧨") {
                    nars.instance.reset()
                    nars.count += 1
                    nars.history.append("\(nars.count) .  🧨 Reset completed!")
                }
                .font(Font.title)
                .background(Color.white)
                .foregroundColor(.red)
                .clipShape(Circle())
                .shadow(radius: 1)
                .padding()
            }
            Spacer()
            HStack {
                TextField("Input", text: $input)
                Button("Submit") {
                    do {
                        let x = try Term(input, parser: nars.narsese)
                        nars.instance.perform(x)
                    } catch {
                        nars.history.append("\(error)")
                    }
                }
            }
        }
    }
}
