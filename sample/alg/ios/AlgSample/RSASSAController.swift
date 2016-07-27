//
//  RSASSAController.swift
//  AlgSample
//
//  Created by rarnu on 7/27/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

import UIKit

class RSASSAController: UIViewController {

    private var btnKeypair: UIButton?
    private var tvEncTitle: UILabel?
    private var tvVerifyTitle: UILabel?
    private var etEncSrc: UITextField?
    private var etVerifySrc: UITextField?
    private var etVerifyOri: UITextField?
    private var tvEncDest: UILabel?
    private var tvVerifyDest: UILabel?
    private var btnEncGo: UIButton?
    private var btnVerifyGo: UIButton?
    private var tvStatus: UILabel?
    
    private var PUBKEY_PATH = ""
    private var PUBKEY_PASS = "hujiang"
    private var PRIVKEY_PATH = ""
    private var PRIVKEY_PASS = "hujiang"
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
        let DOC = getDocumentPath()
        PUBKEY_PATH = "\(DOC)/rsassa"
        PRIVKEY_PATH = "\(DOC)/rsassa.priv"
        
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
        
        tvVerifyTitle = UILabel(frame: CGRect(x: 8, y: 64 + 208, width: w - 16, height: 32))
        tvVerifyTitle?.text = STR_VERIFY
        etVerifySrc = UITextField(frame: CGRect(x: 8, y: 64 + 248, width: w - 16, height: 32))
        etVerifySrc?.borderStyle = UITextBorderStyle.roundedRect
        etVerifySrc?.placeholder = STR_VERIFY_HINT
        etVerifyOri = UITextField(frame: CGRect(x: 8, y: 64 + 288, width: w - 16, height: 32))
        etVerifyOri?.borderStyle = UITextBorderStyle.roundedRect
        etVerifyOri?.placeholder = STR_VERIFY_ORI_HINT
        tvVerifyDest = UILabel(frame: CGRect(x: 8, y: 64 + 328, width: w - 16, height: 32))
        btnVerifyGo = UIButton(type: UIButtonType.system)
        btnVerifyGo?.frame = CGRect(x: 8, y: 64 + 368, width: w - 16, height: 32)
        btnVerifyGo?.backgroundColor = self.view.tintColor
        btnVerifyGo?.setTitleColor(UIColor.white(), for: [])
        btnVerifyGo?.setTitle(STR_BTN_GO, for: [])
        
        tvStatus = UILabel(frame: CGRect(x: 8, y: h - 40, width: w - 16, height: 32))
        
        btnKeypair?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        btnEncGo?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        btnVerifyGo?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        
        self.view.addSubview(btnKeypair!)
        self.view.addSubview(tvEncTitle!)
        self.view.addSubview(etEncSrc!)
        self.view.addSubview(tvEncDest!)
        self.view.addSubview(btnEncGo!)
        self.view.addSubview(tvVerifyTitle!)
        self.view.addSubview(etVerifySrc!)
        self.view.addSubview(etVerifyOri!)
        self.view.addSubview(tvVerifyDest!)
        self.view.addSubview(btnVerifyGo!)
        self.view.addSubview(tvStatus!)
        
    }
    
    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    func handleMessage(msg: String?) {
        tvStatus?.text = ""
        if (msg == "TRUE" || msg == "FALSE") {
            showAlert(msg: msg)
        } else {
            tvStatus?.text = msg
        }
    }
    
    func handleEncData(data: String?) {
        tvEncDest?.text = data
        etVerifySrc?.text = data
    }
    
    func handleDecData(data: String?) {
        tvVerifyDest?.text = data
    }
    
    func showAlert(msg: String?) {
        let alert = UIAlertController(title: "Hint", message: msg, preferredStyle: UIAlertControllerStyle.alert)
        alert.addAction(UIAlertAction(title: "OK", style: UIAlertActionStyle.default, handler: nil))
        self.present(alert, animated: true, completion: nil)
    }
    
    func threadGenerateKeyPair() {
        self.performSelector(onMainThread: #selector(handleMessage(msg:)), with: "Generating Key Pair ...", waitUntilDone: true)
        let ret = rsassaGenerateKeys(1, NSString(string: PUBKEY_PASS).utf8String, NSString(string:PRIVKEY_PASS).utf8String, NSString(string: PUBKEY_PATH).utf8String, NSString(string: PRIVKEY_PATH).utf8String)
        self.performSelector(onMainThread: #selector(handleMessage(msg:)), with: (ret == 0 ? "TRUE" : "FALSE"), waitUntilDone: true)
    }
    
    func threadEncrypt() {
        self.performSelector(onMainThread: #selector(handleMessage(msg:)), with: "Encrypting ...", waitUntilDone: true)
        let ori = etEncSrc?.text
        let enc = rsassaSignString(1, 0, NSString(string: PRIVKEY_PASS).utf8String, NSString(string: PRIVKEY_PATH).utf8String, NSString(string: ori!).utf8String)
        let encStr = NSString(utf8String: enc!)
        self.performSelector(onMainThread: #selector(handleEncData(data:)), with: encStr as? String, waitUntilDone: true)
        self.performSelector(onMainThread: #selector(handleMessage(msg:)), with: "", waitUntilDone: true)
    }
    
    func threadVerify() {
        self.performSelector(onMainThread: #selector(handleMessage(msg:)), with: "Verifying ...", waitUntilDone: true)
        let e = etVerifySrc?.text
        let ori = etVerifyOri?.text
        let r = rsassaVerifyString(1, 0, NSString(string:PUBKEY_PASS).utf8String, NSString(string: PUBKEY_PATH).utf8String, NSString(string: e!).utf8String, NSString(string: ori!).utf8String)
        let decStr = (r == 0 ? "TRUE" : "FALSE")
        self.performSelector(onMainThread: #selector(handleDecData(data:)), with: decStr, waitUntilDone: true)
        self.performSelector(onMainThread: #selector(handleMessage(msg:)), with: "", waitUntilDone: true)
    }
    
    func btnClicked(sender: AnyObject?) {
        let btn = sender as? UIButton
        if (btn == btnKeypair) {
            Thread.detachNewThreadSelector(#selector(threadGenerateKeyPair), toTarget: self, with: nil)
        } else if (btn == btnEncGo) {
            etVerifyOri?.text = etEncSrc?.text
            Thread.detachNewThreadSelector(#selector(threadEncrypt), toTarget: self, with: nil)
        } else if (btn == btnVerifyGo) {
            Thread.detachNewThreadSelector(#selector(threadVerify), toTarget: self, with: nil)
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
