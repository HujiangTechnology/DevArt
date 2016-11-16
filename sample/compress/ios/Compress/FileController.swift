//
//  FileController.swift
//  Compress
//
//  Created by rarnu on 7/19/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

import UIKit

class FileController: UIViewController {
    
    var btnMakeTest: UIButton?
    var btnCleanTest: UIButton?
    var btnCleanUnzip: UIButton?
    var btnShowTest: UIButton?
    var btnShowUnzip: UIButton?
    var btnShowSD: UIButton?

    private var DOC: String?
    private var SDZIPSRC: String?
    private var SDUNZIPDEST: String?
    
    override func viewDidLoad() {
        super.viewDidLoad()
        DOC = getDocumentPath()
        SDZIPSRC = "\(DOC!)/test"
        SDUNZIPDEST = "\(DOC!)/unzip/"
        initUI()
        initEvent()
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }
    
    private func initUI() {
        let size = UIScreen.main.bounds.size
        let w = size.width
        let noffset = self.navigationController!.navigationBar.frame.size.height + UIApplication.shared.statusBarFrame.size.height
        
        btnMakeTest = UIButton(type: UIButtonType.system)
        btnMakeTest?.frame = CGRect(x: 8, y: noffset + 16, width: w - 16, height: 32)
        btnMakeTest?.backgroundColor = self.view.tintColor
        btnMakeTest?.setTitleColor(UIColor.white, for: [])
        btnMakeTest?.setTitle("Make Test", for: [])
        btnCleanTest = UIButton(type: UIButtonType.system)
        btnCleanTest?.frame = CGRect(x: 8, y: noffset + 56, width: w - 16, height: 32)
        btnCleanTest?.backgroundColor = self.view.tintColor
        btnCleanTest?.setTitleColor(UIColor.white, for: [])
        btnCleanTest?.setTitle("Clean Test", for: [])
        btnCleanUnzip = UIButton(type: UIButtonType.system)
        btnCleanUnzip?.frame = CGRect(x: 8, y: noffset + 96, width: w - 16, height: 32)
        btnCleanUnzip?.backgroundColor = self.view.tintColor
        btnCleanUnzip?.setTitleColor(UIColor.white, for: [])
        btnCleanUnzip?.setTitle("Clean Unzip", for: [])
        btnShowTest = UIButton(type: UIButtonType.system)
        btnShowTest?.frame = CGRect(x: 8, y: noffset + 136, width: w - 16, height: 32)
        btnShowTest?.backgroundColor = self.view.tintColor
        btnShowTest?.setTitleColor(UIColor.white, for: [])
        btnShowTest?.setTitle("Show Test", for: [])
        btnShowUnzip = UIButton(type: UIButtonType.system)
        btnShowUnzip?.frame = CGRect(x: 8, y: noffset + 176, width: w - 16, height: 32)
        btnShowUnzip?.backgroundColor = self.view.tintColor
        btnShowUnzip?.setTitleColor(UIColor.white, for: [])
        btnShowUnzip?.setTitle("Show Unzip", for: [])
        btnShowSD = UIButton(type: UIButtonType.system)
        btnShowSD?.frame = CGRect(x: 8, y: noffset + 216, width: w - 16, height: 32)
        btnShowSD?.backgroundColor = self.view.tintColor
        btnShowSD?.setTitleColor(UIColor.white, for: [])
        btnShowSD?.setTitle("Show Root", for: [])
        
        self.view.addSubview(btnMakeTest!)
        self.view.addSubview(btnCleanTest!)
        self.view.addSubview(btnCleanUnzip!)
        self.view.addSubview(btnShowTest!)
        self.view.addSubview(btnShowUnzip!)
        self.view.addSubview(btnShowSD!)
    }
    
    private func initEvent() {
        btnMakeTest?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        btnCleanTest?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        btnCleanUnzip?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        btnShowTest?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        btnShowUnzip?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        btnShowSD?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
    }
    
    func btnClicked(sender: AnyObject?) {
        let btn = sender as? UIButton
        if (btn == btnMakeTest) {
            makeTest()
        } else if (btn == btnCleanTest) {
            cleanTest()
        } else if (btn == btnCleanUnzip) {
            cleanUnzip()
        } else if (btn == btnShowTest) {
            showTest()
        } else if (btn == btnShowUnzip) {
            showUnzip()
        } else if (btn == btnShowSD) {
            showSD()
        }
    }
    
    private func getDocumentPath() -> String {
        var paths = NSSearchPathForDirectoriesInDomains(FileManager.SearchPathDirectory.documentDirectory, FileManager.SearchPathDomainMask.allDomainsMask, true)
        return paths[0]
    }
    
    private func makeTest() {
        // TODO: make test
        let mgr = FileManager.default
        var isDir: ObjCBool = false
        if (mgr.fileExists(atPath: SDZIPSRC!, isDirectory: &isDir)) {
            if (isDir.boolValue) {
                cleanTest()
            }
        }
        do {
            try mgr.createDirectory(atPath: SDZIPSRC!, withIntermediateDirectories: true, attributes: nil)
            try mgr.createDirectory(atPath: "\(SDZIPSRC!)/sub1", withIntermediateDirectories: true, attributes: nil)
            try mgr.createDirectory(atPath: "\(SDZIPSRC!)/sub2", withIntermediateDirectories: true, attributes: nil)
            mgr.createFile(atPath: "\(SDZIPSRC!)/sub1/a", contents: nil, attributes: nil)
            mgr.createFile(atPath: "\(SDZIPSRC!)/sub1/b", contents: nil, attributes: nil)
            mgr.createFile(atPath: "\(SDZIPSRC!)/sub2/c", contents: nil, attributes: nil)
            mgr.createFile(atPath: "\(SDZIPSRC!)/sub2/d", contents: nil, attributes: nil)
            mgr.createFile(atPath: "\(SDZIPSRC!)/e", contents: nil, attributes: nil)
            mgr.createFile(atPath: "\(SDZIPSRC!)/f", contents: nil, attributes: nil)
            showAlert(msg: "make test ok")
        } catch let e {
            showAlert(msg: "make test error: \(e)")
        }
        
    }
    
    private func cleanTest() {
        // TODO: clean test
        let mgr = FileManager.default
        do {
            try mgr.removeItem(atPath: SDZIPSRC!)
            showAlert(msg: "clean test ok")
        } catch let e {
            showAlert(msg: "clean test error: \(e)")
        }
    }
    
    private func cleanUnzip() {
        // TODO: clean unzip
        let mgr = FileManager.default
        do {
            try mgr.removeItem(atPath: SDUNZIPDEST!)
            showAlert(msg: "clean unzip ok")
        } catch let e {
            showAlert(msg: "clean unzip error: \(e)")
            
        }
    }
    
    private func showTest() {
        // TODO: show test
        let mgr = FileManager.default
        var str = ""
        let ar = mgr.subpaths(atPath: SDZIPSRC!)
        if (ar != nil) {
            for f in ar! {
                str += "\(f)\n"
            }
        }
        showAlert(msg: str)
    }
    
    private func showUnzip() {
        // TODO: show unzip
        let mgr = FileManager.default
        var str = ""
        let ar = mgr.subpaths(atPath: SDUNZIPDEST!)
        if (ar != nil) {
            for f in ar! {
                str += "\(f)\n"
            }
        }
        showAlert(msg: str)
    }
    
    private func showSD() {
        let mgr = FileManager.default
        do {
            let ar = try mgr.contentsOfDirectory(atPath: DOC!)
            var str = ""
            for f in ar {
                str += "\(f)\n"
            }
            showAlert(msg: str)
        } catch let e {
            showAlert(msg: "show sd error: \(e)")
        }
    }
    
    private func showAlert(msg: String?) {
        let alert = UIAlertController(title: "Hint", message: msg, preferredStyle: UIAlertControllerStyle.alert)
        alert.addAction(UIAlertAction(title: "OK", style: UIAlertActionStyle.default, handler: nil))
        self.present(alert, animated: true, completion: nil)
    }

}
