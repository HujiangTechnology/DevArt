package com.hujiang.devart.component.daemon

import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.os.IBinder
import android.os.Parcel
import android.os.RemoteException
import android.util.Log
import java.io.File
import kotlin.concurrent.thread

/**
 * Created by rarnu on 4/7/16.
 */
class DaemonStrategy22: DaemonStrategyBase() {

    private var	_serviceData: Parcel? = null

    override fun onPersistentCreate(context: Context?, configs: DaemonConfigurations?) {
        initAmsBinder()
        initServiceParcel(context, configs?.DAEMON_ASSISTANT_CONFIG?.serviceName)
        startServiceByAmsBinder()
        thread {
            val indicatorDir = context?.getDir(INDICATOR_DIR_NAME, Context.MODE_PRIVATE)
            NativeDaemon21(context).doDaemon(File(indicatorDir, INDICATOR_PERSISTENT_FILENAME).absolutePath, File(indicatorDir, INDICATOR_DAEMON_ASSISTANT_FILENAME).absolutePath, File(indicatorDir, OBSERVER_PERSISTENT_FILENAME).absolutePath, File(indicatorDir, OBSERVER_DAEMON_ASSISTANT_FILENAME).absolutePath)
        }
        if(configs?.LISTENER != null){
            _configs = configs
            _configs?.LISTENER?.onPersistentStart(context)
        }
    }

    override fun onDaemonAssistantCreate(context: Context?, configs: DaemonConfigurations?) {
        initAmsBinder()
        initServiceParcel(context, configs?.PERSISTENT_CONFIG?.serviceName)
        startServiceByAmsBinder()
        thread {
            val indicatorDir = context?.getDir(INDICATOR_DIR_NAME, Context.MODE_PRIVATE)
            NativeDaemon21(context).doDaemon(File(indicatorDir, INDICATOR_DAEMON_ASSISTANT_FILENAME).absolutePath, File(indicatorDir, INDICATOR_PERSISTENT_FILENAME).absolutePath, File(indicatorDir, OBSERVER_DAEMON_ASSISTANT_FILENAME).absolutePath, File(indicatorDir, OBSERVER_PERSISTENT_FILENAME).absolutePath)
        }
        if(configs?.LISTENER != null){
            _configs = configs
            _configs?.LISTENER?.onDaemonAssistantStart(context)
        }
    }

    override fun onDaemonDead() {
        if(startServiceByAmsBinder()){
            _configs?.LISTENER?.onWatchDaemonDaed()
            android.os.Process.killProcess(android.os.Process.myPid())
        }
    }

    protected  fun initServiceParcel(context: Context?, serviceName: String?){
        val intent = Intent()
        val component = ComponentName(context?.packageName, serviceName)
        intent.component = component
        _serviceData = Parcel.obtain()
        _serviceData?.writeInterfaceToken("android.app.IActivityManager")
        _serviceData?.writeStrongBinder(null)
        intent.writeToParcel(_serviceData, 0)
        _serviceData?.writeString(null)
        _serviceData?.writeInt(0)
    }

    private fun startServiceByAmsBinder(): Boolean {
        try {
            if(_remote == null || _serviceData == null){
                Log.e("LOG", "DaemonStrategy22:startServiceByAmsBinder => REMOTE IS NULL or PARCEL IS NULL")
                return false
            }
            _remote?.transact(34, _serviceData, null, 0)
            return true
        } catch (e: RemoteException) {
            Log.e("LOG", "DaemonStrategy22:startServiceByAmsBinder => ${e.message}")
            return false
        }
    }
}