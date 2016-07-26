package com.hujiang.devart.utils

import android.util.Log

/**
 * Created by rarnu on 3/30/16.
 */
object ZipUtils {

    val ERRORCODE_NOERROR = 0
    val ERRORCODE_FORMAT_NOT_SUPPORT = -1
    val ERRORCODE_UNCOMPRESS = -2
    val ERRORCODE_COMPRESS = -3

    init {
        try {
            System.loadLibrary("hjz")
        } catch(e: Exception) {
            Log.e("ZipUtils", "System.loadLibrary(\"hjz\") => ${e.message}")
        }
    }

    external fun uncompress(filePath: String, dest: String): Int
    external fun compress(filePath: String, src: String): Int
    external fun getFileSize(path: String): String

    external fun getCompressErrorCode(filePath: String): Int
    external fun getCompressErrorMessage(filePath: String): String
    external fun getCompressFileCount(filePath: String): Int

    external fun getCompressedCount(filePath: String): Int
    external fun getUncompressErrorCode(filePath: String): Int
    external fun getUncompressErrorMessage(filePath: String): String
    external fun getUncompressFileCount(filePath: String): Int
    external fun getUncompressedCount(filePath: String): Int

    data class CompressStatus(var filePath: String?, var fileCount: Int, var compressCount: Int, var errCode: Int, var errMsg: String?)
    data class UncompressStatus(var filePath: String?, var fileCount: Int, var uncompressCount: Int, var errCode: Int, var errMsg: String?)
    fun getCompressStatus(filePath: String): CompressStatus = CompressStatus(filePath, getCompressFileCount(filePath), getCompressedCount(filePath), getCompressErrorCode(filePath), getCompressErrorMessage(filePath))
    fun getUncompressStatus(filePath: String): UncompressStatus = UncompressStatus(filePath, getUncompressFileCount(filePath), getUncompressedCount(filePath), getUncompressErrorCode(filePath), getUncompressErrorMessage(filePath))

}