//
//  ViewController.swift
//  Compress
//
//  Created by rarnu on 6/3/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

import UIKit

class ViewController: UIViewController, UITableViewDataSource, UITableViewDelegate {
    
    @IBOutlet var btnCompress: UIButton?
    @IBOutlet var btnUncompress: UIButton?
    @IBOutlet var btnClean: UIButton?
    @IBOutlet var btnGetHelp: UIButton?
    @IBOutlet var table: UITableView?

    var listFiles = NSMutableArray()
    var basePath = ""
    
    override func viewDidLoad() {
        super.viewDidLoad()
        basePath = getDocumentPath()
        table?.registerClass(UITableViewCell.classForCoder(), forCellReuseIdentifier: "Cell")
        table?.tableFooterView = UIView(frame: CGRectZero)
        getFilesInFolder(basePath)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }

    func getDocumentPath() -> String {
        var paths = NSSearchPathForDirectoriesInDomains(NSSearchPathDirectory.DocumentDirectory, NSSearchPathDomainMask.AllDomainsMask, true)
        return paths[0]
    }
    
    private func clean() {
        let testPath = "\(basePath)/test/"
        let zipPath = "\(basePath)/test.zip"
        let unzipPath = "\(basePath)/unzip"
        let mgr = NSFileManager.defaultManager()
        do {
            try mgr.removeItemAtPath(testPath)
        } catch {
            
        }
        do {
            try mgr.removeItemAtPath(zipPath)
        } catch {
            
        }
        do {
            try mgr.removeItemAtPath(unzipPath)
        } catch {
            
        }
    }
    
    private func getFilesInFolder(folder: String) {
        let mgr = NSFileManager.defaultManager()
        let files = mgr.subpathsAtPath(folder)
        listFiles.removeAllObjects()
        if (files != nil) {
            for s in files! {
                listFiles.addObject(s)
            }
        }
        table?.reloadData()
    }
    
    func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return 1
    }
    
    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return listFiles.count
    }
    
    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        let path = "\(basePath)/\(listFiles.objectAtIndex(indexPath.row) as! String)"
        let info = getPathInfo(path)
        if (info != "") {
            showAlertMessage(info)
        }
    }
    
    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let cell = table!.dequeueReusableCellWithIdentifier("Cell", forIndexPath: indexPath)
        cell.textLabel?.text = listFiles.objectAtIndex(indexPath.row) as? String
        return cell
    }
    
    @IBAction func btnCleanClick(sender: AnyObject?) {
        clean()
        getFilesInFolder(basePath)
    }
    
    @IBAction func btnCompressClick(sender: AnyObject?) {
        // base files
        let testPath = "\(basePath)/test/"
        let zipPath = "\(basePath)/test.zip"
        
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
            showAlertMessage("Compressed: \(ret)")
        } catch (let e) {
            showAlertMessage("Compress Error: \(e)")
        }
        getFilesInFolder(basePath)
    }
    
    @IBAction func btnUncompressClick(sender: AnyObject?) {
        let unzipPath = "\(basePath)/unzip"
        let zipPath = "\(basePath)/test.zip"
        let mgr = NSFileManager.defaultManager()
        
        do {
            try mgr.createDirectoryAtPath(unzipPath, withIntermediateDirectories: true, attributes: nil)
            let ret = uncompress(NSString(string: zipPath).UTF8String, NSString(string: unzipPath).UTF8String)
            showAlertMessage("Uncompressed: \(ret)")
        } catch (let e) {
            showAlertMessage("Uncompress Error: \(e)")
        }
        getFilesInFolder(basePath)
    }
    
    @IBAction func btnGetHelp(sender: AnyObject?) {
        let help = getHelp()
        let helpStr = NSString(UTF8String: help)
        showAlertMessage("\(helpStr)")
    }
    
    private func showAlertMessage(msg: String) {
        let alert = UIAlertController(title: "Hint", message: msg, preferredStyle: UIAlertControllerStyle.Alert)
        alert.addAction(UIAlertAction(title: "OK", style: UIAlertActionStyle.Default, handler: nil))
        self.presentViewController(alert, animated: false, completion: nil)
    }
    
    private func getPathInfo(path: String) -> String {
        let mgr = NSFileManager.defaultManager()
        var isDir: ObjCBool = false
        let exist = mgr.fileExistsAtPath(path, isDirectory: &isDir)
        var fileSize: Int64 = 0
        var ret = ""
        if (exist) {
            do {
                var attrs: [String: AnyObject]?
                try attrs = mgr.attributesOfItemAtPath(path)
                fileSize = (attrs![NSFileSize] as! NSNumber).longLongValue
                ret = "Path: \(path)\nIsDir: \(isDir ? "TRUE" : "FALSE")\nSize: \(fileSize)"
            } catch(let e) {
                showAlertMessage("GetInfo Error: \(e)")
            }
        } else {
            ret = "Path: \(path)\nNot Exist"
        }
        return ret
        
    }
}

