//
//  ViewController.swift
//  Compress
//
//  Created by rarnu on 6/3/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

import UIKit

class ViewController: UIViewController {
    
    @IBOutlet var btnCompress: UIButton?
    @IBOutlet var btnUncompress: UIButton?
    @IBOutlet var btnGetHelp: UIButton?

    override func viewDidLoad() {
        super.viewDidLoad()
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }

    func getDocumentPath() -> String {
        var paths = NSSearchPathForDirectoriesInDomains(NSSearchPathDirectory.DocumentDirectory, NSSearchPathDomainMask.AllDomainsMask, true)
        return paths[0]
    }
    
    private func clean() {
        let path = getDocumentPath()
        let testPath = "\(path)/test/"
        let zipPath = "\(path)/test.zip"
        let mgr = NSFileManager.defaultManager()
        do {
            try mgr.removeItemAtPath(testPath)
        } catch {
            
        }
        do {
            try mgr.removeItemAtPath(zipPath)
        } catch {
            
        }
    }
    

    @IBAction func btnCompressClick(sender: AnyObject?) {
        clean()
        // base files
        let path = getDocumentPath()
        let testPath = "\(path)/test/"
        let zipPath = "\(path)/test.zip"
        
        let fileA = "\(testPath)a"
        let fileB = "\(testPath)b"
        let dirSub1 = "\(testPath)sub1/"
        let fileC = "\(dirSub1)c"
        let fileD = "\(dirSub1)d"
        let dirSub2 = "\(dirSub1)sub2/"
        let fileE = "\(dirSub2)e"
        let fileF = "\(dirSub2)f"
        
        let mgr = NSFileManager.defaultManager()
        do {
            try mgr.createDirectoryAtPath(testPath, withIntermediateDirectories: true, attributes: nil)
            mgr.createFileAtPath(fileA, contents: nil, attributes: nil)
            mgr.createFileAtPath(fileB, contents: nil, attributes: nil)
            try mgr.createDirectoryAtPath(dirSub1, withIntermediateDirectories: true, attributes: nil)
            mgr.createFileAtPath(fileC, contents: nil, attributes: nil)
            mgr.createFileAtPath(fileD, contents: nil, attributes: nil)
            try mgr.createDirectoryAtPath(dirSub2, withIntermediateDirectories: true, attributes: nil)
            mgr.createFileAtPath(fileE, contents: nil, attributes: nil)
            mgr.createFileAtPath(fileF, contents: nil, attributes: nil)
            let ret = compress(NSString(string: zipPath).UTF8String, NSString(string:testPath).UTF8String)
            print("compressed: \(ret)")
        } catch (let e) {
            print("error: \(e)")
        }
    }
    
    @IBAction func btnUncompressClick(sender: AnyObject?) {
        let path = getDocumentPath()
        let unzipPath = "\(path)/unzip"
        let zipPath = "\(path)/test.zip"
        let mgr = NSFileManager.defaultManager()
        
        do {
            try mgr.createDirectoryAtPath(unzipPath, withIntermediateDirectories: true, attributes: nil)
            let ret = uncompress(NSString(string: zipPath).UTF8String, NSString(string: unzipPath).UTF8String)
            print("uncompressed: \(ret)")
        } catch (let e) {
            print("error: \(e)")
        }
    }
    
    @IBAction func btnGetHelp(sender: AnyObject?) {
        let help = getHelp()
        let helpStr = NSString(UTF8String: help)
        print("helpinfo: \n\(helpStr)")
    }
}

