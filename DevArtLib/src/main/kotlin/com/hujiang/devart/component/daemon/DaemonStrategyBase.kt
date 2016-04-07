package com.hujiang.devart.component.daemon

import android.app.Activity
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.os.IBinder
import android.os.Parcel
import android.os.RemoteException
import android.text.TextUtils
import android.util.Log
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStream

/**
 * Created by rarnu on 4/7/16.
 */
abstract class DaemonStrategyBase: IDaemonStrategy {

    companion object {
        val INDICATOR_DIR_NAME = "indicators"
        val INDICATOR_PERSISTENT_FILENAME = "indicator_p"
        val INDICATOR_DAEMON_ASSISTANT_FILENAME = "indicator_d"
        val OBSERVER_PERSISTENT_FILENAME = "observer_p"
        val OBSERVER_DAEMON_ASSISTANT_FILENAME = "observer_d"
        val BINARY_DEST_DIR_NAME = "bin"
        val BINARY_FILE_NAME = "daemon"
    }

    protected var _remote: IBinder? = null
    protected var _configs: DaemonConfigurations? = null

    override fun onInitialization(context: Context?): Boolean = initIndicatorFiles(context)


    protected fun initAmsBinder() {
        try {
            val activityManagerNative = Class.forName("android.app.ActivityManagerNative")
            val amn = activityManagerNative.getMethod("getDefault").invoke(activityManagerNative)
            val remoteField = amn.javaClass.getDeclaredField("mRemote")
            remoteField.isAccessible = true
            _remote = remoteField.get(amn) as IBinder
        } catch (e: Exception) {
            Log.e("LOG", "DaemonStrategyBase:initAmsBinder => ${e.message}")
        }
    }

    protected fun initIndicatorFiles(context: Context?): Boolean {
        val dirFile = context?.getDir(INDICATOR_DIR_NAME, Context.MODE_PRIVATE)!!
        if (!dirFile.exists()) {
            dirFile.mkdirs()
        }
        try {
            createNewFile(dirFile, INDICATOR_PERSISTENT_FILENAME)
            createNewFile(dirFile, INDICATOR_DAEMON_ASSISTANT_FILENAME)
            return true
        } catch (e: IOException) {
            Log.e("LOG", "DaemonStrategyBase: initIndicatorFiles => ${e.message}")
            return false
        }
    }

    protected  fun createNewFile(dirFile: File?, fileName: String?) {
        val file = File(dirFile, fileName)
        if (!file.exists()) {
            file.createNewFile()
        }
    }

    protected fun install(context: Context?, destDirName: String?, assetsDirName: String?, filename: String?): Boolean {
        val file = File(context?.getDir(destDirName, Context.MODE_PRIVATE), filename)
        if (file.exists()) {
            return true
        }
        try {
            copyAssets(context, (if (TextUtils.isEmpty(assetsDirName)) "" else (assetsDirName + File.separator)) + filename, file, "700")
            return true
        } catch (e: Exception) {
            Log.e("LOG", "DaemonStrategyBase: install => ${e.message}")
            return false
        }
    }

    private fun copyAssets(context: Context?, assetsFilename: String?, file: File?, mode: String?) {
        val manager = context?.assets
        val ins = manager?.open(assetsFilename)
        copyFile(file, ins!!, mode)
    }

    private fun copyFile(file: File?, ins: InputStream, mode: String?) {
        if (!file!!.parentFile.exists()) {
            file.parentFile.mkdirs()
        }
        val abspath = file.absolutePath
        val out = FileOutputStream(file)
        val buf = ByteArray(1024)
        var len: Int
        while (true) {
            len = ins.read(buf)
            if (len > 0) {
                out.write(buf, 0, len)
            } else {
                break
            }
        }
        out.close()
        ins.close()
        Runtime.getRuntime().exec("chmod ${mode} ${abspath}").waitFor()
    }

}