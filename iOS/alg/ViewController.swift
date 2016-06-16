//
//  ViewController.swift
//  alg
//
//  Created by rarnu on 5/27/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

import UIKit

class ViewController: UIViewController {

    @IBOutlet var btnRun: UIButton? = nil
    @IBOutlet var txtResult: UITextView? = nil
    
    override func viewDidLoad() {
        super.viewDidLoad()
        
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }
    
    func getDocumentPath() -> String {
        let paths = NSSearchPathForDirectoriesInDomains(NSSearchPathDirectory.DocumentDirectory, NSSearchPathDomainMask.AllDomainsMask, true)
        return paths[0]
    }
    
    func runAlgSample() {
        let str = "abcdefg"
        let strPChar = NSString(string: str).UTF8String
        let emptyPChar = NSString(string:"").UTF8String
        self.performSelectorOnMainThread(#selector(ViewController.mainLoopMessage), withObject: "Origin: \(str)\n=================================", waitUntilDone: true)

        // md5 sample
        let retMd5 = md5EncryptString(strPChar)
        var msgStr = "MD5 Encrypt: \(NSString(UTF8String: retMd5) as! String)"
        self.performSelectorOnMainThread(#selector(ViewController.mainLoopMessage), withObject: "\(msgStr)\n=================================", waitUntilDone: true)

        // sha1 sample
        let retSha1 = sha1EncryptString(strPChar)
        msgStr = "SHA1 Encrypt: \(NSString(UTF8String: retSha1) as! String)"
        self.performSelectorOnMainThread(#selector(ViewController.mainLoopMessage), withObject: "\(msgStr)\n=================================", waitUntilDone: true)

        // RSA sample
        var s = ""
        let privPath = "\(getDocumentPath())/rsa.priv"
        let pubPath = "\(getDocumentPath())/rsa.pub"
        let privPathPChar = NSString(string: privPath).UTF8String
        let pubPathPChar = NSString(string: pubPath).UTF8String
        let retGK = rsaGenerateKeys(0, emptyPChar, emptyPChar, pubPathPChar, privPathPChar)
        if (retGK == 0) {
            s += "RSA Generate key ok\n"
            let retEncrypt = rsaEncryptString(0, emptyPChar, pubPathPChar, strPChar)
            s += "RSA Encrypt: \(NSString(UTF8String: retEncrypt)!)\n"
            let retDecrypt = rsaDecryptString(0, emptyPChar, privPathPChar, retEncrypt)
            s += "RSA Decrypt: \(NSString(UTF8String: retDecrypt)!)\n"
        } else {
            s += "RSA Generate key failed\n"
        }
        self.performSelectorOnMainThread(#selector(ViewController.mainLoopMessage), withObject: "\(s)=================================", waitUntilDone: true)

        // DSA sample
        self.performSelectorOnMainThread(#selector(ViewController.mainLoopMessage), withObject: "DSA Verify may takes long time, please wait...", waitUntilDone: true)
        s = ""
        let dsaPriv = "\(getDocumentPath())/dsa.priv"
        let dsaPub = "\(getDocumentPath())/dsa.pub"
        let dsaPrivPChar = NSString(string: dsaPriv).UTF8String
        let dsaPubPChar = NSString(string: dsaPub).UTF8String
        let retDsaGK = dsaGenerateKeys(0, emptyPChar, emptyPChar, dsaPubPChar, dsaPrivPChar)
        if (retDsaGK == 0) {
            s += "DSA Generate key ok\n";
            let retSign = dsaSignString(0, emptyPChar, dsaPrivPChar, strPChar)
            s += "DSA Sign: \(NSString(UTF8String: retSign)!)\n"
            let retVerify = dsaVerifyString(0, emptyPChar, dsaPubPChar, retSign, strPChar)
            s += "DSA Verify: \(retVerify == 0 ? "TRUE": "FALSE")\n"
        } else {
            s += "DSA Generate key failed\n"
        }
        self.performSelectorOnMainThread(#selector(ViewController.mainLoopMessage), withObject: "\(s)=================================", waitUntilDone: true)
        self.performSelectorOnMainThread(#selector(ViewController.completeSample), withObject: nil, waitUntilDone: true)
    }
    
    func mainLoopMessage(msg: String) {
        txtResult?.text! += msg + "\n"
    }
    
    func completeSample() {
        btnRun?.enabled = true
    }
    
    @IBAction func btnRunClick(sender: AnyObject?) {
        btnRun?.enabled = false
        txtResult?.text = ""
        NSThread.detachNewThreadSelector(#selector(ViewController.runAlgSample), toTarget: self, withObject: nil)
    }

}

