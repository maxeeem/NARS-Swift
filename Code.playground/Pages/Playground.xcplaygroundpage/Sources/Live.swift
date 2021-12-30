import UIKit

public final class Output: UIViewController {
    let bview = ButtonView()
    let controller = DetailViewController()
    public var text = "" {
        didSet {
            DispatchQueue.main.async {
                self.bview.button.setTitle(self.text, for: .normal)
            }
        }
    }
    public var callback: (Sentence) -> () = {_ in}
    public var reset: () -> () = {}
    public var isVerbose = false
    public var onVerbose: (Bool) -> () = {_ in}
    
    override public func loadView() {
        bview.button.addTarget(self, action: #selector(buttonDidTap), for: .touchDown)
        self.view = bview
    }
    
    @objc
    private func buttonDidTap() {
        controller.dview.verbose.isOn = isVerbose
        controller.callback = { s in
            self.callback(s)
        }
        controller.reset = {
            self.reset()
        }
        controller.verbose = { v in
            self.onVerbose(v)
        }
        present(controller, animated: true, completion: nil)
    }
    
}

class ButtonView: UIView {
    let button = UIButton(type: .custom)
    override init(frame: CGRect) {
        super.init(frame: frame)
        createSubviews()
    }
    
    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)
        createSubviews()
    }
    
    func createSubviews() {
        self.backgroundColor = UIColor.systemBlue
        
        button.frame = CGRect(x: 0, y: 0, width: 200, height: 20)
        button.setTitle("Tap to go to Detail View", for: .normal)
        button.titleLabel?.lineBreakMode = .byWordWrapping
        button.setTitleColor(.white, for: .normal)
        
        button.contentEdgeInsets = UIEdgeInsets(top: 10,left: 10,bottom: 5,right: 5)
        button.isUserInteractionEnabled = true
        self.addSubview(button)
        
        button.translatesAutoresizingMaskIntoConstraints = false
        button.centerYAnchor.constraint(equalTo: self.centerYAnchor).isActive = true
        button.centerXAnchor.constraint(equalTo: self.centerXAnchor).isActive = true
    }
    
    
}

final class DetailViewController: UIViewController {
    var callback: (Sentence) -> () = {_ in}
    var reset: () -> () = {}
    var verbose: (Bool) -> () = {_ in}
    
    let dview = DetailUIView()
    
    override func loadView() {
        dview.button.addTarget(self, action: #selector(buttonDidTap), for: .touchDown)
        dview.reset.addTarget(self, action: #selector(resetButtonDidTap), for: .touchDown)
        dview.go.addTarget(self, action: #selector(goButtonDidTap), for: .touchDown)
        self.view = dview
        dview.subject.becomeFirstResponder()
    }
    
    @objc
    private func buttonDidTap() {
        dismiss(animated: true, completion: nil)
    }
    @objc
    private func resetButtonDidTap() {
        reset()
//        dismiss(animated: true, completion: nil)
    }
    @objc
    private func goButtonDidTap() {
        verbose(dview.verbose.isOn)
        var truth = dview.toggle.isOn ? (1.0, 0.9) : (0, 0.9)
        let s = dview.subject.text!.trimmingCharacters(in: [" "])
        let p = dview.predicate.text!.trimmingCharacters(in: [" "])
        if !s.isEmpty, !p.isEmpty {
            if s == p { // tautology 
                truth = (1.0, 1.0)
            }
            let sentence: Sentence = (s-->p)-*truth
            callback(sentence)
        }
        //dismiss(animated: true, completion: nil)
    }
}

class DetailUIView: UIView {
    let button = UIButton(type: .custom)
    let reset = UIButton(type: .custom)
    let go = UIButton(type: .custom)
    let subject = UITextField()
    let predicate = UITextField()
    let toggle = UISwitch()
    let verbose = UISwitch()
    override init(frame: CGRect) {
        super.init(frame: frame)
        createSubviews()
    }
    
    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)
        createSubviews()
    }
    
    func createSubviews() {
        self.backgroundColor = .clear
        let v = UIView()
        v.backgroundColor = .white
        v.frame = CGRect(x: 0, y: 0, width: 420, height: 300)
        addSubview(v)
        button.frame = CGRect(x: 0, y: 0, width: 200, height: 20)
        button.setTitle("Tap to dismiss view", for: .normal)
        button.setTitleColor(.systemBlue, for: .normal)
        button.isUserInteractionEnabled = true
        button.contentEdgeInsets = UIEdgeInsets(top: 10,left: 10,bottom: 5,right: 5)
        
        self.addSubview(button)
        
        reset.frame = CGRect(x: 250, y: 10, width: 200, height: 20)
        reset.setTitle("Memory reset", for: .normal)
        reset.setTitleColor(.systemRed, for: .normal)
        reset.isUserInteractionEnabled = true
        
        self.addSubview(reset)
        
        button.translatesAutoresizingMaskIntoConstraints = false
        button.centerYAnchor.constraint(equalTo: self.centerYAnchor).isActive = true
        button.centerXAnchor.constraint(equalTo: self.centerXAnchor).isActive = true
        
        self.addSubview(button)
        
        subject.placeholder = "  bear"
        subject.layer.sublayerTransform = CATransform3DMakeTranslation(10, 0, 0);
        subject.autocapitalizationType = .none
        subject.frame = CGRect(x: 40, y: 150, width: 100, height: 40)
        subject.backgroundColor = .systemBlue
        subject.textColor = .white
        subject.isUserInteractionEnabled = true
        self.addSubview(subject)
        
        let copula = UILabel()
        copula.text = " -> "
        copula.frame = CGRect(x: 140, y: 150, width: 40, height: 40)
        copula.textColor = .systemBlue
        copula.backgroundColor = .white
        self.addSubview(copula)
        
        predicate.placeholder = "  animal"
        predicate.layer.sublayerTransform = CATransform3DMakeTranslation(10, 0, 0);
        predicate.autocapitalizationType = .none
        predicate.frame = CGRect(x: 169, y: 150, width: 100, height: 40)
        predicate.backgroundColor = .systemBlue
        predicate.textColor = .white
        self.addSubview(predicate)
        
        go.frame = CGRect(x: 300, y: 150, width: 100, height: 40)
        go.backgroundColor = .systemGreen
        go.setTitle("Go", for: .normal)
        go.setTitleColor(.white, for: .normal)
        go.isUserInteractionEnabled = true
        go.contentEdgeInsets = UIEdgeInsets(top: 10,left: 10,bottom: 5,right: 5)
        
        self.addSubview(go)
        
        let label = UILabel()
        label.frame = CGRect(x: 60, y: 200, width: 250, height: 40)
        label.text = "positive or negative evidence?"
        label.textColor = .gray
        label.backgroundColor = .white
        self.addSubview(label)
        
        toggle.isOn = true
        toggle.frame = CGRect(x: 320, y: 200, width: 100, height: 40)
        
        self.addSubview(toggle)
        
        let l = UILabel()
        l.frame = CGRect(x: 169, y: 60, width: 250, height: 40)
        l.text = "verbose output?"
        l.textColor = .gray
        l.backgroundColor = .white
        self.addSubview(l)
        
        verbose.isOn = false
        verbose.frame = CGRect(x: 320, y: 60, width: 100, height: 40)
        
        self.addSubview(verbose)
    }
}
