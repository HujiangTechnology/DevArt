package com.hujiang.devart.sample.service

import android.app.Service
import android.content.Intent
import android.os.IBinder
import android.util.Log

/**
 * Created by rarnu on 4/7/16.
 */
class DaemonService1: Service() {

    override fun onCreate() {
        super.onCreate()
    }

    override fun onBind(intent: Intent?): IBinder? = null

    override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
        Log.e("LOG", "DaemonService1:onStartCommand")
        return super.onStartCommand(intent, flags, startId)
    }
}