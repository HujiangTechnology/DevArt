package com.hujiang.devart.server

import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream

/**
 * Created by rarnu on 4/27/16.
 */
class DefaultTempFile: TempFile {

    var _file: File? = null
    var _fstream: OutputStream? = null

    constructor(tempdir: File?) {
        _file = File.createTempFile("HTTPServer-", "", tempdir)
        _fstream = FileOutputStream(_file)
    }

    override fun delete() {
        HTTPServer.safeClose(_fstream)
        if (!_file!!.delete()) {
            throw Exception("could not delete temporary file")
        }
    }

    override fun getName(): String? = _file?.absolutePath

    override fun open(): OutputStream? = _fstream

}