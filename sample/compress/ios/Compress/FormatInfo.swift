//
//  FormatInfo.swift
//  Compress
//
//  Created by rarnu on 7/19/16.
//  Copyright © 2016 rarnu. All rights reserved.
//

import UIKit

class FormatInfo: NSObject {

    var format: String?
    var fileType: Int?
    var zipSrc: String?
    var zipDest: String?
    var unzipSrc: String?
    var unzipDest: String?
    
    init(_ aformat: String?, _ afileType: Int?, _ azipSrc: String?, _ azipDest: String?, _ aunzipSrc: String?, _ aunzipDest: String?) {
        self.format = aformat
        self.fileType = afileType
        self.zipSrc = azipSrc
        self.zipDest = azipDest
        self.unzipSrc = aunzipSrc
        self.unzipDest = aunzipDest
    }
}
