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
    @IBOutlet var table: UITableView?

    var listFiles = NSMutableArray()
    var basePath = ""
    
    override func viewDidLoad() {
        super.viewDidLoad()
        basePath = getDocumentPath()
        table?.register(UITableViewCell.classForCoder(), forCellReuseIdentifier: "Cell")
        table?.tableFooterView = UIView(frame: CGRect.zero)
        getFilesInFolder(basePath)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }

    func getDocumentPath() -> String {
        var paths = NSSearchPathForDirectoriesInDomains(FileManager.SearchPathDirectory.documentDirectory, FileManager.SearchPathDomainMask.allDomainsMask, true)
        return paths[0]
    }
    
    private func clean() {
        let testPath = "\(basePath)/test/"
        let zipPath = "\(basePath)/test.zip"
        let unzipPath = "\(basePath)/unzip"
        let mgr = FileManager.default()
        do {
            try mgr.removeItem(atPath: testPath)
        } catch {
            
        }
        do {
            try mgr.removeItem(atPath: zipPath)
        } catch {
            
        }
        do {
            try mgr.removeItem(atPath: unzipPath)
        } catch {
            
        }
    }
    
    private func getFilesInFolder(_ folder: String) {
        let mgr = FileManager.default()
        let files = mgr.subpaths(atPath: folder)
        listFiles.removeAllObjects()
        if (files != nil) {
            for s in files! {
                listFiles.add(s)
            }
        }
        table?.reloadData()
    }
    
    func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
    
    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return listFiles.count
    }
    
    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        let path = "\(basePath)/\(listFiles.object(at: (indexPath as NSIndexPath).row) as! String)"
        let info = getPathInfo(path)
        if (info != "") {
            showAlertMessage(info)
        }
    }
    
    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = table!.dequeueReusableCell(withIdentifier: "Cell", for: indexPath)
        cell.textLabel?.text = listFiles.object(at: (indexPath as NSIndexPath).row) as? String
        return cell
    }
    
    @IBAction func btnCleanClick(_ sender: AnyObject?) {
        clean()
        getFilesInFolder(basePath)
    }
    
    @IBAction func btnCompressClick(_ sender: AnyObject?) {
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
        
        let mgr = FileManager.default()
        do {
            try mgr.createDirectory(atPath: testPath, withIntermediateDirectories: true, attributes: nil)
            mgr.createFile(atPath: fileA, contents: nil, attributes: nil)
            mgr.createFile(atPath: fileB, contents: nil, attributes: nil)
            try mgr.createDirectory(atPath: dirSub1, withIntermediateDirectories: true, attributes: nil)
            mgr.createFile(atPath: fileC, contents: nil, attributes: nil)
            mgr.createFile(atPath: fileD, contents: nil, attributes: nil)
            try mgr.createDirectory(atPath: dirSub2, withIntermediateDirectories: true, attributes: nil)
            mgr.createFile(atPath: fileE, contents: nil, attributes: nil)
            mgr.createFile(atPath: fileF, contents: nil, attributes: nil)
            let ret = compress(NSString(string: zipPath).utf8String, NSString(string:testPath).utf8String)
            print("Compress => \(ret)")
            let status = NativeCompress.getStatus(zipPath)!
            let msg = "compress => errMsg: \(status.errorMessage!), fileCount: \(status.fileCount), compressed: \(status.compressCount)"
            showAlertMessage(msg)
        } catch (let e) {
            showAlertMessage("Compress Error: \(e)")
        }
        getFilesInFolder(basePath)
    }
    
    @IBAction func btnUncompressClick(_ sender: AnyObject?) {
        let unzipPath = "\(basePath)/unzip"
        let zipPath = "\(basePath)/test.zip"
        let mgr = FileManager.default()
        
        do {
            try mgr.createDirectory(atPath: unzipPath, withIntermediateDirectories: true, attributes: nil)
            let ret = uncompress(NSString(string: zipPath).utf8String, NSString(string: unzipPath).utf8String)
            print("Uncompress => \(ret)")
            let status = NativeCompress.getUncompressStatus(zipPath)!
            let msg = "uncompress => errMsg: \(status.errorMessage!), fileCount: \(status.fileCount), uncompressed: \(status.uncompressCount)"
            showAlertMessage(msg)
        } catch (let e) {
            showAlertMessage("Uncompress Error: \(e)")
        }
        getFilesInFolder(basePath)
    }
    
    private func showAlertMessage(_ msg: String) {
        let alert = UIAlertController(title: "Hint", message: msg, preferredStyle: UIAlertControllerStyle.alert)
        alert.addAction(UIAlertAction(title: "OK", style: UIAlertActionStyle.default, handler: nil))
        self.present(alert, animated: false, completion: nil)
    }
    
    private func getPathInfo(_ path: String) -> String {
        let mgr = FileManager.default()
        var isDir: ObjCBool = false
        let exist = mgr.fileExists(atPath: path, isDirectory: &isDir)
        var fileSize = ""
        var ret = ""
        if (exist) {
            let pathChar = NSString(string: path).utf8String
            fileSize = String(utf8String: getFileSize(pathChar))!
            ret = "Path: \(path)\nIsDir: \(isDir ? "TRUE" : "FALSE")\nSize: \(fileSize)"
        } else {
            ret = "Path: \(path)\nNot Exist"
        }
        return ret
        
    }
}

