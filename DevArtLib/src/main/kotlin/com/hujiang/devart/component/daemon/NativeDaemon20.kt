package com.hujiang.devart.component.daemon

import android.content.Context
import android.util.Log

/**
 * Created by rarnu on 4/7/16.
 */
class NativeDaemon20(context: Context?) : NativeDaemonBase(context) {

    init {
        try {
            System.loadLibrary("daemon20")
        } catch (e: Exception) {
            Log.e("LOG", "loadLibrary: daemon20 => ${e.message}")
        }
    }

    external fun doDaemon(pkgName: String?, svcName: String?, daemonPath: String?)

}