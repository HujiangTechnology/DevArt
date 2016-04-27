package com.hujiang.devart.server

import android.util.Log
import java.io.File

/**
 * Created by rarnu on 4/27/16.
 */
class DefaultTempFileManager: TempFileManager {

    var _tmpdir: File? = null
    var _tempFiles: MutableList<TempFile?>? = null

    constructor() {
        _tmpdir = File(System.getProperty("java.io.tmpdir"))
        if (!_tmpdir!!.exists()) {
            _tmpdir?.mkdirs()
        }
        _tempFiles = arrayListOf()
    }

    override fun clear() {
        for (file in _tempFiles!!) {
            try {
                file?.delete()
            } catch (e: Exception) {
                Log.e("LOG", "could not delete file ${e.message}")
            }
        }
        _tempFiles?.clear()
    }

    override fun createTempFile(filenameHint: String?): TempFile? {
        val tempFile = DefaultTempFile(_tmpdir)
        _tempFiles?.add(tempFile)
        return tempFile
    }

}