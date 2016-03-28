package com.hujiang.devart.component.cache

import android.content.Context
import com.hujiang.devart.utils.FileUtils
import com.hujiang.devart.utils.MiscUtils
import java.io.File

/**
 * Created by rarnu on 3/28/16.
 */
object FileCache {

    private var _cacheDir: File? = null

    fun initFileCache(context: Context) {
        if (MiscUtils.isSDCardExists()) {
            _cacheDir = File(android.os.Environment.getExternalStorageDirectory(), ".cache")
        } else {
            _cacheDir = context.cacheDir
        }
        if (!_cacheDir!!.exists()) {
            _cacheDir!!.mkdirs()
        }
    }

    fun getFile(url: String): File? = File(_cacheDir, url.hashCode().toString())

    fun getFilePath(url: String): String? = getFile(url)?.absolutePath

    fun putFile(filePath: String) {
        val filename = filePath.substringAfterLast("/")
        val fDest = File(_cacheDir, filename)
        if (fDest.exists()) {
            fDest.delete()
        }
        FileUtils.copyFile(filePath, fDest.absolutePath, null)
    }

    fun getFileByName(fileName: String): File? {
        val f = File(_cacheDir, fileName)
        return if (f.exists()) f else null
    }

    fun clearCache() {
        val files = _cacheDir?.listFiles()
        if (files != null) {
            for (f in files) {
                f.delete()
            }
        }
    }
}