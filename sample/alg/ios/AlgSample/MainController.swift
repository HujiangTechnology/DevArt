//
//  MainController.swift
//  AlgSample
//
//  Created by rarnu on 7/26/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

import UIKit

class MainController: UITableViewController {

    let list = ["MD5","SHA1","LMD","ELF", "BASE64", "RSA", "DSA", "RDL", "RSASSA", "AES"]
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.tableView.tableFooterView = UIView(frame: CGRect.zero)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }

    // MARK: - Table view data source

    override func numberOfSections(in tableView: UITableView) -> Int {
        return 1
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return list.count
    }
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "Cell", for: indexPath)
        cell.textLabel?.text = list[indexPath.row]
        return cell
    }

    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        
        let name = list[indexPath.row]
        let controllerName = "\(name)Controller"
        let vc = self.storyboard?.instantiateViewController(withIdentifier: controllerName)
        if (vc != nil) {
            self.navigationController?.pushViewController(vc!, animated: true)
        }
        tableView.deselectRow(at: indexPath, animated: true)
    }

}
