package com.hujiang.devart.utils

import android.util.Log

/**
 * Created by rarnu on 3/30/16.
 */
object ZipUtils {

    init {
        try {
            System.loadLibrary("hjz")
        } catch(e: Exception) {
            Log.e("ZipUtils", "System.loadLibrary(\"hjz\") => ${e.message}")
        }
    }

    external fun uncompress(filePath: String, dest: String): Int
    external fun compress(filePath: String, src: String): Int
    external fun getLastError(): Int
    external fun getLastErrorMessage(): String
    external fun getHelp(): String
}