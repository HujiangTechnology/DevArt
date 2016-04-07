package com.hujiang.devart.component.daemon

import android.content.Context
import android.util.Log

/**
 * Created by rarnu on 4/7/16.
 */
class NativeDaemon21(context: Context?) : NativeDaemonBase(context) {

    init {
        try {
            System.loadLibrary("daemon21")
            Log.e("LOG", "loadLibrary: daemon21")
        } catch (e: Exception) {
            Log.e("LOG", "loadLibrary: daemon21 => ${e.message}")
        }
    }

    external fun doDaemon(indicatorSelfPath: String?, indicatorDaemonPath: String?, observerSelfPath: String?, observerDaemonPath: String?)

}