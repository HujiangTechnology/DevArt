package com.hujiang.devart.component.daemon

import android.app.Activity
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.os.Parcel
import android.os.RemoteException
import android.util.Log
import java.io.File
import kotlin.concurrent.thread

/**
 * Created by rarnu on 4/7/16.
 */
class DaemonStrategy23 : DaemonStrategyBase() {

    protected var _broadcastData: Parcel? = null

    override fun onPersistentCreate(context: Context?, configs: DaemonConfigurations?) {
        initAmsBinder()
        initBroadcastParcel(context, configs?.DAEMON_ASSISTANT_CONFIG?.receiverName)
        sendBroadcastByAmsBinder()
        thread {
            val indicatorDir = context?.getDir(INDICATOR_DIR_NAME, Context.MODE_PRIVATE)
            NativeDaemon21(context).doDaemon(File(indicatorDir, INDICATOR_PERSISTENT_FILENAME).absolutePath, File(indicatorDir, INDICATOR_DAEMON_ASSISTANT_FILENAME).absolutePath, File(indicatorDir, OBSERVER_PERSISTENT_FILENAME).absolutePath, File(indicatorDir, OBSERVER_DAEMON_ASSISTANT_FILENAME).absolutePath)
        }
        val componentName = ComponentName(context?.packageName, configs?.PERSISTENT_CONFIG?.serviceName)
        val intent = Intent()
        intent.component = componentName
        context?.startService(intent)
        if (configs?.LISTENER != null) {
            _configs = configs
            configs?.LISTENER?.onPersistentStart(context)
        }
    }

    override fun onDaemonAssistantCreate(context: Context?, configs: DaemonConfigurations?) {
        initAmsBinder()
        initBroadcastParcel(context, configs?.PERSISTENT_CONFIG?.receiverName)
        sendBroadcastByAmsBinder()
        thread {
            val indicatorDir = context?.getDir(INDICATOR_DIR_NAME, Context.MODE_PRIVATE)
            NativeDaemon21(context).doDaemon(File(indicatorDir, INDICATOR_DAEMON_ASSISTANT_FILENAME).absolutePath, File(indicatorDir, INDICATOR_PERSISTENT_FILENAME).absolutePath, File(indicatorDir, OBSERVER_DAEMON_ASSISTANT_FILENAME).absolutePath, File(indicatorDir, OBSERVER_PERSISTENT_FILENAME).absolutePath)
        }
        val componentName = ComponentName(context?.packageName, configs?.DAEMON_ASSISTANT_CONFIG?.serviceName)
        val intent = Intent()
        intent.component = componentName
        context?.startService(intent)
        if (configs?.LISTENER != null) {
            _configs = configs
            configs?.LISTENER?.onDaemonAssistantStart(context)
        }
    }

    override fun onDaemonDead() {
        if (sendBroadcastByAmsBinder()) {
            _configs?.LISTENER?.onWatchDaemonDaed()
            android.os.Process.killProcess(android.os.Process.myPid())
        }
    }

    protected fun initBroadcastParcel(context: Context?, broadcastName: String?) {
        val intent = Intent()
        val componentName = ComponentName(context?.packageName, broadcastName)
        intent.component = componentName
        intent.flags = Intent.FLAG_INCLUDE_STOPPED_PACKAGES
        _broadcastData = Parcel.obtain()
        _broadcastData?.writeInterfaceToken("android.app.IActivityManager")
        _broadcastData?.writeStrongBinder(null)
        intent.writeToParcel(_broadcastData, 0)
        _broadcastData?.writeString(intent.resolveTypeIfNeeded(context?.contentResolver))
        _broadcastData?.writeStrongBinder(null)
        _broadcastData?.writeInt(Activity.RESULT_OK)
        _broadcastData?.writeString(null)
        _broadcastData?.writeBundle(null)
        _broadcastData?.writeString(null)
        _broadcastData?.writeInt(-1)
        _broadcastData?.writeInt(0)
        _broadcastData?.writeInt(0)
        _broadcastData?.writeInt(0)
    }

    protected fun sendBroadcastByAmsBinder(): Boolean {
        try {
            if (_remote == null || _broadcastData == null) {
                Log.e("LOG", "DaemonStrategy23:sendBroadcastByAmsBinder => REMOTE IS NULL or PARCEL IS NULL")
                return false
            }
            _remote?.transact(14, _broadcastData, null, 0)
            return true
        } catch (e: RemoteException) {
            Log.e("LOG", "DaemonStrategy23: sendBroadcastByAmsBinder => ${e.message}")
            return false
        }
    }

}