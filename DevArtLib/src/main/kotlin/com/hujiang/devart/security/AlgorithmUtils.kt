package com.hujiang.devart.security

import android.util.Log

/**
 * Created by rarnu on 4/1/16.
 */
object AlgorithmUtils {

    init {
        try {
            System.loadLibrary("alg")
        } catch(e: Exception) {
            Log.e("AlgorithmUtils", "System.loadLibrary(\"alg\") => ${e.message}")
        }
    }

    external fun md5EncryptString(str: String): String
    external fun md5EncryptFile(filePath: String): String
    external fun sha1EncryptString(str: String): String
    external fun sha1EncryptFile(filePath: String): String
    external fun lmdEncryptString(str: String): String
    external fun lmdEncryptFile(filePath: String): String
    external fun elfEncryptString(str: String): String

    /**
     * ```
     * val a = "abcd"
     * val key = "key"
     * val h = desBytes2Str(a)
     * val hk = desBytes2Str(key)
     * val e = desEncryptString(a, key)
     * ```
     */
    external fun desEncryptString(str: String, key: String): String

    /**
     * ```
     * val a = "0x68 0x74 0x74 0x70 0x3a 0x2f 0x2f"
     * val key = "key"
     * val hk = desBytes2Str(key)
     * val d = desDecryptString(a, hk)
     * val str = desBytes2Str(d)
     * ```
     */
    external fun desDecryptString(str: String, key: String): String

    fun desStr2Bytes(str: String): String {
        val bytes = str.toByteArray()
        var ret = ""
        for (b in bytes) {
            if (b < 0) {
                throw IllegalArgumentException("Unsupport NON-ASCII characters.")
            }
            ret += "0x${toHex(b)} "
        }
        return ret
    }

    fun desBytes2Str(str: String): String {
        val strArr = str.split(" ")
        var ret = ""
        var v: Int
        for (s in strArr) {
            if (s.trim() != "") {
                v = Integer.parseInt(s.replace("0x", ""), 16)
                ret += "${v.toChar()}"
            }
        }
        return ret
    }

    fun toHex(b: Byte): String {
        var result = Integer.toHexString(b.toInt())
        if (result.length == 1) {
            result = "0${result}"
        }
        return result
    }


}