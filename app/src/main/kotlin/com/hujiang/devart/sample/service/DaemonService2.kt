package com.hujiang.devart.sample.service

import android.app.Service
import android.content.Intent
import android.os.IBinder
import android.util.Log

/**
 * Created by rarnu on 4/7/16.
 */
class DaemonService2: Service() {
    override fun onBind(intent: Intent?): IBinder? = null
    override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
        Log.e("LOG", "DaemonService2:onStartCommand")
        return START_NOT_STICKY
    }
}