package com.hujiang.devart.utils.http

import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileInputStream
import java.io.InputStream

/**
 * Created by rarnu on 3/25/16.
 */
class FilePartSource : PartSource {

    private var _file: File? = null
    private var _fileName: String? = null

    constructor(file: File?) {
        _file = file
        if (file != null) {
            if (!file.isFile || !file.canRead()) {
                throw Exception()
            }
            _fileName = file.name
        }
    }

    constructor(fileName: String?, file: File?) : this(file) {
        if (fileName != null) {
            _fileName = fileName
        }
    }

    override fun getLength(): Long = if (_file != null) {
        _file!!.length()
    } else {
        0
    }


    override fun getFileName(): String? = _fileName ?: "noname"

    override fun createInputStream(): InputStream? = if (_file != null) {
        FileInputStream(_file)
    } else {
        ByteArrayInputStream(byteArrayOf())
    }

}