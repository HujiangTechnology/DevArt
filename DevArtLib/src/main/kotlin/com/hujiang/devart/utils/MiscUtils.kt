package com.hujiang.devart.utils

import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Build
import android.os.Environment
import android.telephony.TelephonyManager
import com.hujiang.devart.command.Command
import java.io.File
import java.util.*

/**
 * Created by rarnu on 3/28/16.
 */
object MiscUtils {

    fun doScanMedia(context: Context) =
            context.sendBroadcast(Intent(Intent.ACTION_MEDIA_SCANNER_SCAN_FILE, Uri.parse("file://" + Environment.getExternalStorageDirectory().absolutePath)))

    fun isSDCardExists(): Boolean = Environment.getExternalStorageState() == Environment.MEDIA_MOUNTED

    fun getSecondSdcardPath(hasSplit: Boolean): String? {
        val ret = Command.runCommand("mount", false, null)
        val lines = ret.result.split("\n")
        val systemSdcard = Environment.getExternalStorageDirectory().absolutePath
        var extSdcard: String = ""
        for (s in lines) {
            if (s.contains("secure") || s.contains("asec")) {
                continue
            }
            if (s.contains("fat") || s.contains("fuse")) {
                val columns = s.split(" ")
                if (columns.size > 3) {
                    if (columns[3].contains("rw,")) {
                        extSdcard += columns[1] + "\n"
                    }
                }
            }
        }
        extSdcard = extSdcard.replace(systemSdcard, "").trim()
        if (!extSdcard.endsWith("/") && hasSplit) {
            extSdcard += "/"
        }
        return extSdcard
    }


    fun isEmulator(context: Context): Boolean = try {
        var ret: Boolean
        val tm = context.getSystemService(Context.TELEPHONY_SERVICE) as TelephonyManager
        val imei = tm.deviceId
        if (imei != null && imei == "000000000000000") {
            ret = true
        } else {
            ret = (Build.MODEL == "sdk") || (Build.MODEL == "google_sdk")
        }
        if (!ret) {
            ret = isBlueStacks()
        }
        if (!ret) {
            ret = isGenymotion()
        }
        ret
    } catch (e: Exception) {
        false
    }


    fun randomString(length: Int): String {
        val result = StringBuilder()
        val text = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        val c = text.toCharArray()
        val random = Random()
        for (i in 0..length - 1) {
            result.append(c[random.nextInt(c.size)])
        }
        return result.toString()
    }

    private fun isBlueStacks(): Boolean = Build.MODEL.toLowerCase().contains("bluestacks")

    private fun isGenymotion(): Boolean = Build.MODEL.toLowerCase().contains("genymotion")

    fun isNoMediaContained(path: String): Boolean {
        val rootPath = Environment.getExternalStorageDirectory().absolutePath
        var basePath = path
        var hasNomedia = false
        while (basePath != rootPath) {
            if (File(basePath, ".nomedia").exists()) {
                hasNomedia = true
                break
            }
            basePath = basePath.substringBeforeLast("/")
        }
        return hasNomedia
    }

}