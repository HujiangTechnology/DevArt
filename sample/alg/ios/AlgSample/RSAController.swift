//
//  RSAController.swift
//  AlgSample
//
//  Created by rarnu on 7/27/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

import UIKit

class RSAController: UIViewController {

    private var btnKeypair: UIButton?
    private var tvEncTitle: UILabel?
    private var tvDecTitle: UILabel?
    private var etEncSrc: UITextField?
    private var etDecSrc: UITextField?
    private var tvEncDest: UILabel?
    private var tvDecDest: UILabel?
    private var btnEncGo: UIButton?
    private var btnDecGo: UIButton?
    private var tvStatus: UILabel?
    
    private var PUBKEY_PATH = ""
    private var PUBKEY_PASS = "hujiang"
    private var PRIVKEY_PATH = ""
    private var PRIVKEY_PASS = "hujiang"
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        let DOC = getDocumentPath()
        PUBKEY_PATH = "\(DOC)/rsa.pub"
        PRIVKEY_PATH = "\(DOC)/rsa.priv"
        
        let w = UIScreen.main().bounds.size.width
        let h = UIScreen.main().bounds.size.height
        
        btnKeypair = UIButton(type: UIButtonType.system)
        btnKeypair?.frame = CGRect(x: 8, y: 64 + 8, width: w - 16, height: 32)
        btnKeypair?.backgroundColor = self.view.tintColor
        btnKeypair?.setTitleColor(UIColor.white(), for: [])
        btnKeypair?.setTitle(STR_BTN_KEYPAIR, for: [])
        tvEncTitle = UILabel(frame: CGRect(x: 8, y: 64 + 48, width: w - 16, height: 32))
        tvEncTitle?.text = STR_ENC
        etEncSrc = UITextField(frame: CGRect(x: 8, y: 64 + 88, width: w - 16, height: 32))
        etEncSrc?.borderStyle = UITextBorderStyle.roundedRect
        etEncSrc?.placeholder = STR_ENC_HINT
        tvEncDest = UILabel(frame: CGRect(x: 8, y: 64 + 128, width: w - 16, height: 32))
        btnEncGo = UIButton(type: UIButtonType.system)
        btnEncGo?.frame = CGRect(x: 8, y: 64 + 168, width: w - 16, height: 32)
        btnEncGo?.backgroundColor = self.view.tintColor
        btnEncGo?.setTitleColor(UIColor.white(), for: [])
        btnEncGo?.setTitle(STR_BTN_GO, for: [])
        
        tvDecTitle = UILabel(frame: CGRect(x: 8, y: 64 + 208, width: w - 16, height: 32))
        tvDecTitle?.text = STR_DEC
        etDecSrc = UITextField(frame: CGRect(x: 8, y: 64 + 248, width: w - 16, height: 32))
        etDecSrc?.borderStyle = UITextBorderStyle.roundedRect
        etDecSrc?.placeholder = STR_DEC_HINT
        tvDecDest = UILabel(frame: CGRect(x: 8, y: 64 + 288, width: w - 16, height: 32))
        btnDecGo = UIButton(type: UIButtonType.system)
        btnDecGo?.frame = CGRect(x: 8, y: 64 + 328, width: w - 16, height: 32)
        btnDecGo?.backgroundColor = self.view.tintColor
        btnDecGo?.setTitleColor(UIColor.white(), for: [])
        btnDecGo?.setTitle(STR_BTN_GO, for: [])
        
        tvStatus = UILabel(frame: CGRect(x: 8, y: h - 40, width: w - 16, height: 32))
        
        btnKeypair?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        btnEncGo?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        btnDecGo?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        
        self.view.addSubview(btnKeypair!)
        self.view.addSubview(tvEncTitle!)
        self.view.addSubview(etEncSrc!)
        self.view.addSubview(tvEncDest!)
        self.view.addSubview(btnEncGo!)
        self.view.addSubview(tvDecTitle!)
        self.view.addSubview(etDecSrc!)
        self.view.addSubview(tvDecDest!)
        self.view.addSubview(btnDecGo!)
        self.view.addSubview(tvStatus!)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }
    
    func handleMessage(msg: String?) {
        tvStatus?.text = ""
        if (msg == "TRUE" || msg == "FALSE") {
            btnKeypair?.isEnabled = true
            showAlert(msg: msg)
        } else {
            tvStatus?.text = msg
        }
    }
    
    func handleEncData(data: String?) {
        btnEncGo?.isEnabled = true
        tvEncDest?.text = data
        etDecSrc?.text = data
    }
    
    func handleDecData(data: String?) {
        btnDecGo?.isEnabled = true
        tvDecDest?.text = data
    }
    
    func showAlert(msg: String?) {
        let alert = UIAlertController(title: "Hint", message: msg, preferredStyle: UIAlertControllerStyle.alert)
        alert.addAction(UIAlertAction(title: "OK", style: UIAlertActionStyle.default, handler: nil))
        self.present(alert, animated: true, completion: nil)
    }
    
    func threadGenerateKeyPair() {
        self.performSelector(onMainThread: #selector(handleMessage(msg:)), with: "Generating Key Pair ...", waitUntilDone: true)
        let ret = rsaGenerateKeys(0, NSString(string: PUBKEY_PASS).utf8String, NSString(string:PRIVKEY_PASS).utf8String, NSString(string: PUBKEY_PATH).utf8String, NSString(string: PRIVKEY_PATH).utf8String)
        self.performSelector(onMainThread: #selector(handleMessage(msg:)), with: (ret == 0 ? "TRUE" : "FALSE"), waitUntilDone: true)
    }
    
    func threadEncrypt() {
        self.performSelector(onMainThread: #selector(handleMessage(msg:)), with: "Encrypting ...", waitUntilDone: true)
        let ori = etEncSrc?.text
        let enc = rsaEncryptString(0, NSString(string: PUBKEY_PASS).utf8String, NSString(string: PUBKEY_PATH).utf8String, NSString(string: ori!).utf8String)
        let encStr = NSString(utf8String: enc!)
        self.performSelector(onMainThread: #selector(handleEncData(data:)), with: encStr as? String, waitUntilDone: true)
        self.performSelector(onMainThread: #selector(handleMessage(msg:)), with: "", waitUntilDone: true)
    }
    
    func threadDecrypt() {
        self.performSelector(onMainThread: #selector(handleMessage(msg:)), with: "Decrypting ...", waitUntilDone: true)
        let ori = etDecSrc?.text
        let dec = rsaDecryptString(0, NSString(string:PRIVKEY_PASS).utf8String, NSString(string: PRIVKEY_PATH).utf8String, NSString(string: ori!).utf8String)
        let decStr = NSString(utf8String: dec!)
        self.performSelector(onMainThread: #selector(handleDecData(data:)), with: decStr as? String, waitUntilDone: true)
        self.performSelector(onMainThread: #selector(handleMessage(msg:)), with: "", waitUntilDone: true)
    }
    
    func btnClicked(sender: AnyObject?) {
        let btn = sender as? UIButton
        if (btn == btnKeypair) {
            btnKeypair?.isEnabled = false
            Thread.detachNewThreadSelector(#selector(threadGenerateKeyPair), toTarget: self, with: nil)
        } else if (btn == btnEncGo) {
            btnEncGo?.isEnabled = false
            Thread.detachNewThreadSelector(#selector(threadEncrypt), toTarget: self, with: nil)
        } else if (btn == btnDecGo) {
            btnDecGo?.isEnabled = false
            Thread.detachNewThreadSelector(#selector(threadDecrypt), toTarget: self, with: nil)
        }
    }
    
    private func getDocumentPath() -> String {
        var paths = NSSearchPathForDirectoriesInDomains(FileManager.SearchPathDirectory.documentDirectory, FileManager.SearchPathDomainMask.allDomainsMask, true)
        return paths[0]
    }

    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        self.view.endEditing(true)
    }
    

}
