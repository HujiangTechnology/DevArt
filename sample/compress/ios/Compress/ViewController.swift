//
//  ViewController.swift
//  Compress
//
//  Created by rarnu on 6/3/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

import UIKit

class ViewController: UIViewController {
    
    private var tvSrc: UILabel?
    private var tvDest: UILabel?
    private var etSrc: UITextField?
    private var etDest: UITextField?
    private var tvFormat: UILabel?
    private var spFormat: UIButton?
    private var tvFormatHint: UILabel?
    private var btnZip: UIButton?
    private var btnUnzip: UIButton?
    private var btnGo: UIButton?
    private var svLog: UIScrollView?
    private var tvLog: UITextView?
    
    private var DOC: String?
    private var SDZIPSRC: String?
    private var SDZIPSRCSINGLE: String?
    private var SDZIPDEST: String?
    private var SDUNZIPDEST: String?
    private var SDUNZIPDESTSINGLE: String?
    
    private var isUnzip = false
    private var selectedFormat = 0
    
    private var ZIP_TYPE: Array<FormatInfo>?
    
    override func viewDidLoad() {
        super.viewDidLoad()
        DOC = getDocumentPath()
        SDZIPSRC = "\(DOC!)/test"
        SDZIPSRCSINGLE = "\(DOC!)/test.txt"
        SDZIPDEST = "\(DOC!)"
        SDUNZIPDEST = "\(DOC!)/unzip/"
        SDUNZIPDESTSINGLE = "\(DOC!)/test.file"
        
        ZIP_TYPE = [
            FormatInfo(".hjz", 0, SDZIPSRC!, "\(SDZIPDEST!)/test.hjz", "\(SDZIPDEST!)/test.hjz", SDUNZIPDEST!),
            FormatInfo(".hjp", 0, SDZIPSRC!, "\(SDZIPDEST!)/test.hjp", "\(SDZIPDEST!)/test.hjp", SDUNZIPDEST!),
            FormatInfo(".zip", 0, SDZIPSRC!, "\(SDZIPDEST!)/test.zip", "\(SDZIPDEST!)/test.zip", SDUNZIPDEST!),
            FormatInfo(".jar", 0, SDZIPSRC!, "\(SDZIPDEST!)/test.jar", "\(SDZIPDEST!)/test.jar", SDUNZIPDEST!),
            FormatInfo(".tar", 0, SDZIPSRC!, "\(SDZIPDEST!)/test.tar", "\(SDZIPDEST!)/test.tar", SDUNZIPDEST!),
            FormatInfo(".gz", 1, SDZIPSRCSINGLE!, "\(SDZIPDEST!)/test.gz", "\(SDZIPDEST!)/test.gz", SDUNZIPDESTSINGLE!),
            FormatInfo(".gzip", 1, SDZIPSRCSINGLE!, "\(SDZIPDEST!)/test.gzip", "\(SDZIPDEST!)/test.gzip", SDUNZIPDESTSINGLE!),
            FormatInfo(".tgz", 0, SDZIPSRC!, "\(SDZIPDEST!)/test.tgz", "\(SDZIPDEST!)/test.tgz", SDUNZIPDEST!),
            FormatInfo(".tar.gz", 0, SDZIPSRC!, "$\(SDZIPDEST!)/test.tar.gz", "\(SDZIPDEST!)/test.tar.gz", SDUNZIPDEST!)
        ]
        
        initUI()
        initEvent()
        initData()
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }

    private func getDocumentPath() -> String {
        var paths = NSSearchPathForDirectoriesInDomains(FileManager.SearchPathDirectory.documentDirectory, FileManager.SearchPathDomainMask.allDomainsMask, true)
        return paths[0]
    }
    
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
        self.view.endEditing(true)
    }
    
    private func initUI() {
        let size = UIScreen.main.bounds.size
        let w = size.width
        let h = size.height
        let noffset = self.navigationController!.navigationBar.frame.size.height + UIApplication.shared.statusBarFrame.size.height
        tvSrc = UILabel(frame: CGRect(x: 8, y: noffset + 16, width: w / 4, height: 32))
        tvSrc?.text = "Source"
        tvDest = UILabel(frame: CGRect(x: 8, y: noffset + 56, width: w / 4, height: 32))
        tvDest?.text = "Destination"
        tvFormat = UILabel(frame: CGRect(x: 8, y: noffset + 96, width: w / 4, height: 32))
        tvFormat?.text = "Format"
        etSrc = UITextField(frame: CGRect(x: w / 4 + 8, y: noffset + 16, width: w * 3 / 4 - 24, height: 32))
        etSrc?.borderStyle = UITextBorderStyle.roundedRect
        etDest = UITextField(frame: CGRect(x: w / 4 + 8, y: noffset + 56, width: w * 3 / 4 - 24, height: 32))
        etDest?.borderStyle = UITextBorderStyle.roundedRect
        spFormat = UIButton(type: UIButtonType.system)
        spFormat?.frame = CGRect(x: w / 4 + 8, y: noffset + 96, width: w * 2 / 4 - 24, height: 32)
        spFormat?.backgroundColor = self.view.tintColor
        spFormat?.setTitleColor(UIColor.white, for: [])
        tvFormatHint = UILabel(frame: CGRect(x: w * 3 / 4, y: noffset + 96, width: w / 4, height: 32))
        btnZip = UIButton(type: UIButtonType.system)
        btnZip?.frame = CGRect(x: 8, y: noffset + 136, width: w / 2 - 8, height: 32)
        btnZip?.backgroundColor = self.view.tintColor
        btnZip?.setTitleColor(UIColor.white, for: [])
        btnZip?.setTitle("Zip", for: [])
        btnUnzip = UIButton(type: UIButtonType.system)
        btnUnzip?.frame = CGRect(x: w / 2 + 8, y: noffset + 136, width: w / 2 - 16, height: 32)
        btnUnzip?.backgroundColor = self.view.tintColor
        btnUnzip?.setTitleColor(UIColor.white, for: [])
        btnUnzip?.setTitle("Unzip", for: [])
        btnGo = UIButton(type: UIButtonType.system)
        btnGo?.frame = CGRect(x: 8, y: noffset + 176, width: w - 16, height: 32)
        btnGo?.backgroundColor = self.view.tintColor
        btnGo?.setTitleColor(UIColor.white, for: [])
        btnGo?.setTitle("GO! GO! GO", for: [])
        svLog = UIScrollView(frame: CGRect(x: 8, y: noffset + 216, width: w - 16, height: h - noffset - 216 - 8))
        svLog?.contentSize = svLog!.frame.size
        // svLog?.isUserInteractionEnabled = false
        svLog?.isMultipleTouchEnabled = false
        tvLog = UITextView(frame: CGRect(x: 0, y: 0, width: svLog!.frame.size.width, height: svLog!.frame.size.height))
        tvLog?.isEditable = false
        tvLog?.isSelectable = false
        // tvLog?.isUserInteractionEnabled = false
        tvLog?.isMultipleTouchEnabled = false
        self.view.addSubview(tvSrc!)
        self.view.addSubview(tvDest!)
        self.view.addSubview(tvFormat!)
        self.view.addSubview(etSrc!)
        self.view.addSubview(etDest!)
        self.view.addSubview(spFormat!)
        self.view.addSubview(tvFormatHint!)
        self.view.addSubview(btnZip!)
        self.view.addSubview(btnUnzip!)
        self.view.addSubview(btnGo!)
        self.view.addSubview(svLog!)
        svLog?.addSubview(tvLog!)
        
    }
    
    private func initEvent() {
        btnZip?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        btnUnzip?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        btnGo?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
        spFormat?.addTarget(self, action: #selector(btnClicked(sender:)), for: UIControlEvents.touchDown)
    }
    
    private func initData() {
        isUnzip = false
        selectedFormat = 0
        refreshData()
    }
    
    private func refreshData() {
        self.navigationItem.title = "ZipSample (\(isUnzip ? "Unzip" : "Zip") Mode)"
        let item = ZIP_TYPE![selectedFormat]
        if (isUnzip) {
            etSrc?.text = item.unzipSrc!
            etDest?.text = item.unzipDest!
        } else {
            etSrc?.text = item.zipSrc!
            etDest?.text = item.zipDest!
        }
        spFormat?.setTitle(item.format!, for: [])
        tvFormatHint?.text = item.fileType == 0 ? "Multi Files" : "Single File"
    }
    
    func btnClicked(sender: AnyObject?) {
        let btn = sender as? UIButton
        if (btn == btnZip) {
            isUnzip = false
            refreshData()
        } else if (btn == btnUnzip) {
            isUnzip = true
            refreshData()
        } else if (btn == btnGo) {
            btnGo?.isEnabled = false
            if (isUnzip) {
                Thread.detachNewThreadSelector(#selector(threadUnzip), toTarget: self, with: nil)
            } else {
                Thread.detachNewThreadSelector(#selector(threadZip), toTarget: self, with: nil)
            }
        } else if (btn == spFormat) {
            chooseFormat()
        }
    }
    
    private func chooseFormat() {
        let alert = UIAlertController(title: "Compress Fomrat", message: "", preferredStyle: UIAlertControllerStyle.alert)
        for item in ZIP_TYPE! {
            let action = UIAlertAction(title: item.format, style: UIAlertActionStyle.default, handler: alertFormatClick)
            alert.addAction(action)
        }
        self.present(alert, animated: true, completion: nil)
    }
    
    private func alertFormatClick(action: UIAlertAction) {
        processSelectPosition(title: action.title)
    }
    
    private func processSelectPosition(title: String?) {
        for i in 0..<ZIP_TYPE!.count {
            if (ZIP_TYPE![i].format == title) {
                selectedFormat = i
                break
            }
        }
        refreshData()
    }
    
    func threadZip() {
        let src = etSrc?.text
        let dest = etDest?.text
        let start = Date(timeIntervalSinceNow: 0).timeIntervalSince1970 * 1000
        sendMessage(h: 0, msg: "[zip-start] \(start)")
        let errorCode = compress(NSString(string: dest!).utf8String, NSString(string: src!).utf8String)
        let end = Date(timeIntervalSinceNow: 0).timeIntervalSince1970 * 1000
        sendMessage(h: 0, msg: "[zip-end] \(end)")
        let status = NativeCompress.getStatus(dest!)!
        sendMessage(h: 0, msg: "[zip-status] err: \(errorCode)")
        sendMessage(h: 0, msg: "[zip-status] msg: \(status.errorMessage!)")
        sendMessage(h: 0, msg: "[zip-status] filePath: \(status.filePath!)")
        sendMessage(h: 0, msg: "[zip-status] fileCount: \(status.fileCount)")
        sendMessage(h: 0, msg: "[zip-status] compressedCount: \(status.compressCount)")
        sendMessage(h: 1, msg: nil)
    }
    
    func threadUnzip() {
        let src = etSrc?.text
        let dest = etDest?.text
        let start = Date(timeIntervalSinceNow: 0).timeIntervalSince1970 * 1000
        sendMessage(h: 0, msg: "[unzip-start] \(start)")
        let errorCode = uncompress(NSString(string: src!).utf8String, NSString(string: dest!).utf8String)
        let end = Date(timeIntervalSinceNow: 0).timeIntervalSince1970 * 1000
        sendMessage(h: 0, msg: "[unzip-end] \(end)")
        let status = NativeCompress.getUncompressStatus(src!)!
        sendMessage(h: 0, msg: "[unzip-status] err: \(errorCode)")
        sendMessage(h: 0, msg: "[unzip-status] msg: \(status.errorMessage!)")
        sendMessage(h: 0, msg: "[unzip-status] filePath: \(status.filePath!)")
        sendMessage(h: 0, msg: "[unzip-status] fileCount: \(status.fileCount)")
        sendMessage(h: 0, msg: "[unzip-status] uncompressedCount: \(status.uncompressCount)")
        sendMessage(h: 1, msg: nil)
    }
    
    func hZipHandler() {
        btnGo?.isEnabled = true
    }
    
    func hMsgHandler(msg: String?) {
        tvLog?.text = tvLog!.text + "\n\(msg!)"
    }
    
    private func sendMessage(h: Int, msg: String?) {
        if (h == 0) {
            self.performSelector(onMainThread: #selector(hMsgHandler(msg:)), with: msg, waitUntilDone: true)
        } else if (h == 1) {
            self.performSelector(onMainThread: #selector(hZipHandler), with: nil, waitUntilDone: true)
        }
    }
    
}

