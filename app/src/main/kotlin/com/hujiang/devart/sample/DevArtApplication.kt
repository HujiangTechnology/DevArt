package com.hujiang.devart.sample

import android.content.Context
import android.util.Log
import com.hujiang.devart.component.daemon.DaemonApplication
import com.hujiang.devart.component.daemon.DaemonConfigurations
import com.hujiang.devart.sample.receiver.DaemonReceiver1
import com.hujiang.devart.sample.receiver.DaemonReceiver2
import com.hujiang.devart.sample.service.DaemonService1
import com.hujiang.devart.sample.service.DaemonService2

/**
 * Created by rarnu on 4/7/16.
 */
class DevArtApplication: DaemonApplication() {

    companion object {
        val PROCESS1 = "com.hujiang.devart.sample:process1"
        val PROCESS2 = "com.hujiang.devart.sample:process2"
    }

    override fun attachBaseContextByDaemon(context: Context?) {
        super.attachBaseContextByDaemon(context)
    }

    override fun getDaemonConfigurations(): DaemonConfigurations? {
        val configuration1 = DaemonConfigurations.DaemonConfiguration(PROCESS1, DaemonService1::class.java.canonicalName, DaemonReceiver1::class.java.canonicalName)
        val configuration2 = DaemonConfigurations.DaemonConfiguration(PROCESS2, DaemonService2::class.java.canonicalName, DaemonReceiver2::class.java.canonicalName)
        val listener = MyDaemonListener()
        return DaemonConfigurations(configuration1, configuration2, listener)
    }

    class MyDaemonListener: DaemonConfigurations.DaemonListener{
        override fun onPersistentStart(context: Context?) {
            Log.e("LOG", "DevArtApplication:onPersistentStart")
        }

        override fun onDaemonAssistantStart(context: Context?) {
            Log.e("LOG", "DevArtApplication:onDaemonAssistantStart")
        }

        override fun onWatchDaemonDead() {
            Log.e("LOG", "DevArtApplication:onWatchDaemonDaed")
        }
    }
}