package com.hujiang.devart.security

import java.io.File
import java.security.cert.Certificate
import java.util.jar.JarEntry
import java.util.jar.JarFile

/**
 * Created by rarnu on 4/8/16.
 */
object SignatureUtils {

    fun compareSignature(apk1: String, apk2: String): Boolean = compareSignature(File(apk1), File(apk2))

    fun compareSignature(apk1: File, apk2: File): Boolean {
        val sig1 = getSignaturesFromApk(apk1)
        val sig2 = getSignaturesFromApk(apk2)
        return sig1.equals(sig2)
    }

    fun getSignaturesFromApk(filePath:String): MutableList<String> = getSignaturesFromApk(File(filePath))

    fun getSignaturesFromApk(file: File): MutableList<String> {
        val signatures = arrayListOf<String>()
        val jarFile = JarFile(file)
        try {
            val je = jarFile.getJarEntry("AndroidManifest.xml")
            val certs = je.certificates
            if (certs != null) {
                for (c in certs) {
                    val sig = toCharsString(c.encoded)
                    signatures.add(sig)
                }
            }
        } catch (e: Exception) {
        }
        return signatures
    }

    private fun toCharsString(sigBytes: ByteArray): String {
        val sig = sigBytes
        val n = sig.size
        val n2 = n * 2
        val text = CharArray(n2)
        for (j in 0 .. n-1) {
            val v = sig[j]
            var d = (v.toInt() shr 4) and 0xf
            text[j * 2] = (if (d >= 10) ('a' + d - 10) else ('0' + d)).toChar()
            d = v.toInt() and 0xf
            text[j * 2 + 1] = (if (d >= 10) ('a' + d - 10) else ('0' + d)).toChar()
        }
        return String(text)
    }

}