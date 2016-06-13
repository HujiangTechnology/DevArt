package com.hujiang.devart.component.daemon

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
class DaemonStrategyXiaomi: DaemonStrategyBase() {

    private var	_serviceData: Parcel? = null

    override fun onInitialization(context: Context?): Boolean = installBinary(context)

    override fun onPersistentCreate(context: Context?, configs: DaemonConfigurations?) {
        initAmsBinder()
        initServiceParcel(context, configs?.DAEMON_ASSISTANT_CONFIG?.serviceName)
        thread(priority = Thread.MAX_PRIORITY) {
            val binaryFile = File(context?.getDir(BINARY_DEST_DIR_NAME, Context.MODE_PRIVATE), BINARY_FILE_NAME)
            try {
                NativeDaemon20(context).doDaemon(context?.packageName, configs?.DAEMON_ASSISTANT_CONFIG?.serviceName, binaryFile.absolutePath)
            } catch(t: Throwable) {

            }
        }
        if(configs?.LISTENER != null){
            _configs = configs
            _configs?.LISTENER?.onPersistentStart(context)
        }
    }

    override fun onDaemonAssistantCreate(context: Context?, configs: DaemonConfigurations?) {
        val intent = Intent()
        val component = ComponentName(context?.packageName, configs?.PERSISTENT_CONFIG?.serviceName)
        intent.component = component
        context?.startService(intent)
        configs?.LISTENER?.onWatchDaemonDead()
        android.os.Process.killProcess(android.os.Process.myPid())
    }

    override fun onDaemonDead() {
        if(startServiceByAmsBinder()){
            _configs?.LISTENER?.onWatchDaemonDead()
            android.os.Process.killProcess(android.os.Process.myPid())
        }
    }

    private fun startServiceByAmsBinder(): Boolean {
        try {
            if(_remote == null || _serviceData == null){
                Log.e("LOG", "DaemonStrategyXiaomi:startServiceByAmsBinder => REMOTE IS NULL or PARCEL IS NULL")
                return false
            }
            _remote?.transact(34, _serviceData, null, 0)
            return true
        } catch (e: RemoteException) {
            Log.e("LOG", "DaemonStrategyXiaomi: startServiceByAmsBinder => ${e.message}")
            return false
        }
    }

    private fun initServiceParcel(context: Context?, serviceName: String?){
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

    private fun installBinary(context: Context?): Boolean = install(context, BINARY_DEST_DIR_NAME, null, BINARY_FILE_NAME)

}