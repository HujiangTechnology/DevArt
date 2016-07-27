//
//  SHA1Controller.swift
//  AlgSample
//
//  Created by rarnu on 7/26/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

import UIKit

class SHA1Controller: UIViewController {

    private var etSrc: UITextField?
    private var tvDest: UILabel?
    private var btnGo: UIButton?
    
    override func viewDidLoad() {
        super.viewDidLoad()

        let w = UIScreen.main().bounds.size.width
        
        etSrc = UITextField(frame: CGRect(x: 8, y: 64 + 8, width: w - 16, height: 32))
        etSrc?.borderStyle = UITextBorderStyle.roundedRect
        etSrc?.placeholder = STR_ENC_HINT
        
        tvDest = UILabel(frame: CGRect(x: 8, y: 64 + 48, width: w - 16, height: 32))
        btnGo = UIButton(type: UIButtonType.system)
        btnGo?.frame = CGRect(x: 8, y: 64 + 88, width: w - 16, height: 32)
        btnGo?.backgroundColor = self.view.tintColor
        btnGo?.setTitleColor(UIColor.white(), for: [])
        btnGo?.setTitle(STR_BTN_GO, for: [])
        
        btnGo?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        
        self.view.addSubview(etSrc!)
        self.view.addSubview(tvDest!)
        self.view.addSubview(btnGo!)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    func btnClicked(sender: AnyObject?) {
        let ori = NSString(string: etSrc!.text!).utf8String
        let enc = sha1EncryptString(ori)
        let envStr = NSString(utf8String: enc!)
        tvDest?.text = envStr as? String
    }
    
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        self.view.endEditing(true)
    }

}
