//
//  RDLController.swift
//  AlgSample
//
//  Created by rarnu on 7/27/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

import UIKit

class RDLController: UIViewController {
    
    private var tvEncTitle: UILabel?
    private var tvDecTitle: UILabel?
    private var etEncSrc: UITextField?
    private var etEncKey: UITextField?
    private var etDecSrc: UITextField?
    private var etDecKey: UITextField?
    private var tvEncDest: UILabel?
    private var tvDecDest: UILabel?
    private var btnEncGo: UIButton?
    private var btnDecGo: UIButton?

    override func viewDidLoad() {
        super.viewDidLoad()

        let w = UIScreen.main.bounds.size.width
        
        tvEncTitle = UILabel(frame: CGRect(x: 8, y: 64 + 8, width: w - 16, height: 32))
        tvEncTitle?.text = STR_ENC
        etEncSrc = UITextField(frame: CGRect(x: 8, y: 64 + 48, width: w - 16, height: 32))
        etEncSrc?.borderStyle = UITextBorderStyle.roundedRect
        etEncSrc?.placeholder = STR_ENC_HINT
        etEncKey = UITextField(frame: CGRect(x: 8, y: 64 + 88, width: w - 16, height: 32))
        etEncKey?.borderStyle = UITextBorderStyle.roundedRect
        etEncKey?.placeholder = STR_ENC_KEY_HINT
        tvEncDest = UILabel(frame: CGRect(x: 8, y: 64 + 128, width: w - 16, height: 32))
        btnEncGo = UIButton(type: UIButtonType.system)
        btnEncGo?.frame = CGRect(x: 8, y: 64 + 168, width: w - 16, height: 32)
        btnEncGo?.backgroundColor = self.view.tintColor
        btnEncGo?.setTitleColor(UIColor.white, for: [])
        btnEncGo?.setTitle(STR_BTN_GO, for: [])
        
        tvDecTitle = UILabel(frame: CGRect(x: 8, y: 64 + 208, width: w - 16, height: 32))
        tvDecTitle?.text = STR_DEC
        etDecSrc = UITextField(frame: CGRect(x: 8, y: 64 + 248, width: w - 16, height: 32))
        etDecSrc?.borderStyle = UITextBorderStyle.roundedRect
        etDecSrc?.placeholder = STR_DEC_HINT
        etDecKey = UITextField(frame: CGRect(x: 8, y: 64 + 288, width: w - 16, height: 32))
        etDecKey?.borderStyle = UITextBorderStyle.roundedRect
        etDecKey?.placeholder = STR_DEC_KEY_HINT
        tvDecDest = UILabel(frame: CGRect(x: 8, y: 64 + 328, width: w - 16, height: 32))
        btnDecGo = UIButton(type: UIButtonType.system)
        btnDecGo?.frame = CGRect(x: 8, y: 64 + 368, width: w - 16, height: 32)
        btnDecGo?.backgroundColor = self.view.tintColor
        btnDecGo?.setTitleColor(UIColor.white, for: [])
        btnDecGo?.setTitle(STR_BTN_GO, for: [])
        
        btnEncGo?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        btnDecGo?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        
        self.view.addSubview(tvEncTitle!)
        self.view.addSubview(etEncSrc!)
        self.view.addSubview(etEncKey!)
        self.view.addSubview(tvEncDest!)
        self.view.addSubview(btnEncGo!)
        self.view.addSubview(tvDecTitle!)
        self.view.addSubview(etDecSrc!)
        self.view.addSubview(etDecKey!)
        self.view.addSubview(tvDecDest!)
        self.view.addSubview(btnDecGo!)
        
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    func handleEncData(data: String?) {
        btnEncGo?.isEnabled = true
        tvEncDest?.text = data
        etDecSrc?.text = data
        etDecKey?.text = etEncKey?.text
    }
    
    func threadEncrypt() {
        let ori = NSString(string: etEncSrc!.text!).utf8String
        let key = NSString(string: etEncKey!.text!).utf8String
        let enc = rdlEncryptString(0, 1, key, ori)
        let encStr = NSString(utf8String: enc!)
        self.performSelector(onMainThread: #selector(handleEncData(data:)), with: encStr as? String, waitUntilDone: true)
    }
    
    func handleDecData(data: String?) {
        btnDecGo?.isEnabled = true
        tvDecDest?.text = data
    }
    
    func threadDecrypt() {
        let ori = NSString(string: etDecSrc!.text!).utf8String
        let key = NSString(string: etDecKey!.text!).utf8String
        let dec = rdlDecryptString(0, 1, key, ori)
        let decStr = NSString(utf8String: dec!)
        self.performSelector(onMainThread: #selector(handleDecData(data:)), with: decStr as? String, waitUntilDone: true)
    }
    
    func btnClicked(sender: AnyObject?) {
        let btn = sender as? UIButton
        if (btn == btnEncGo) {
            btnEncGo?.isEnabled = false
            Thread.detachNewThreadSelector(#selector(threadEncrypt), toTarget: self, with: nil)
        } else if (btn == btnDecGo) {
            btnDecGo?.isEnabled = false
            Thread.detachNewThreadSelector(#selector(threadDecrypt), toTarget: self, with: nil)
        }
    }
    
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        self.view.endEditing(true)
    }
    

}
