package com.hujiang.devart.security

import android.util.Log

/**
 * All algorithm methods should be run in a thread
 *
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

    // Hash ==============================================================================
    external fun md5EncryptString(str: String): String
    external fun md5EncryptFile(filePath: String): String
    external fun sha1EncryptString(str: String): String
    external fun sha1EncryptFile(filePath: String): String
    external fun lmdEncryptString(str: String): String
    external fun lmdEncryptFile(filePath: String): String
    external fun elfEncryptString(str: String): String

    // DES ==============================================================================
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
            result = "0$result"
        }
        return result
    }

    // Base64 ==============================================================================
    external fun base64EncryptString(str: String): String
    external fun base64DecryptString(str: String): String

    // RSA ==============================================================================
    // keysize:  (aks128 = 0, aks256 = 1, aks512 = 2, aks768 = 3, aks1024 = 4)
    // return Integer: succ = 0, other = error code
    external fun rsaGenerateKeys(keySize: Int, pubPass: String, privPass: String, pubSavePath: String, privSavePath: String): Int
    external fun rsaEncryptString(keySize: Int, pubPass: String, pubPath: String, str: String): String
    external fun rsaEncryptFile(keySize: Int, pubPass: String, pubPath: String, filePath: String, outFilePath: String): Int
    external fun rsaDecryptString(keySize: Int, privPass: String, privPath: String, str: String): String
    external fun rsaDecryptFile(keySize: Int, privPass: String, privPath: String, filePath: String, outFilePath: String): Int
    external fun rsaGetPubkeyModules(keySize: Int, pubPass: String, pubPath: String): String
    external fun rsaGetPrivkeyModules(keySize: Int, privPass: String, privPath: String): String

    // DSA ==============================================================================
    // keysize:  (aks128 = 0, aks256 = 1, aks512 = 2, aks768 = 3, aks1024 = 4)
    // return Integer: succ = 0, other = error code
    external fun dsaGenerateKeys(keySize: Int, pubPass: String, privPass: String, pubSavePath: String, privSavePath: String): Int
    external fun dsaSignString(keySize: Int, privPass: String, privPath: String, str: String): String
    external fun dsaSignFile(keySize: Int, privPass: String, privPath: String, filePath: String): String
    external fun dsaVerifyString(keySize: Int, pubPass: String, pubPath: String, rs: String, str: String): Int
    external fun dsaVerifyFile(keySize: Int, pubPass: String, pubPath: String, rs: String, filePath: String): Int
    external fun dsaGetPubkeyQPGY(keySize: Int, pubPass: String, pubPath: String): String
    external fun dsaGetPrivkeyQPGX(keySize: Int, privPass: String, privPath: String): String

    // RDL ==============================================================================
    // keySize: (ks128 = 0, ks192 = 1, ks256 = 2);
    // cipherMode: (cmECB = 0, cmCBC = 1);
    // return Integer: succ = 0, other = error code
    external fun rdlEncryptString(keySize: Int, cipherMode: Int, key: String, str: String): String
    external fun rdlEncryptFile(keySize: Int, cipherMode: Int, key: String, filePath: String, outFilePath: String): Int
    external fun rdlDecryptString(keySize: Int, cipherMode: Int, key: String, str: String): String
    external fun rdlDecryptFile(keySize: Int, cipherMode: Int, key: String, filePath: String, outFilePath: String): Int

    // RSASSA ==============================================================================
    // keysize:  (aks128 = 0, aks256 = 1, aks512 = 2, aks768 = 3, aks1024 = 4)
    // hashMethod : (md5 = 0, sha1 = 1)
    // return Integer: succ = 0, other = error code
    external fun rsassaGenerateKeys(keySize: Int, pubPass: String, privPass: String, pubSavePath: String, privSavePath: String): Int
    external fun rsassaSignString(keySize: Int, hashMethod: Int, privPass: String, privPath: String, str: String): String
    external fun rsassaSignFile(keySize: Int, hashMethod: Int, privPass: String, privPath: String, filePath: String): String
    external fun rsassaVerifyString(keySize: Int, hashMethod: Int, pubPass: String, pubPath: String, sig: String, str: String): Int
    external fun rsassaVerifyFile(keySize: Int, hashMethod: Int, pubPass: String, pubPath: String, sig: String, filePath: String): Int

}