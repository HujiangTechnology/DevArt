package com.hujiang.devart.component.daemon

import android.app.AlarmManager
import android.app.PendingIntent
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.os.Build
import android.os.SystemClock
import java.io.File
import kotlin.concurrent.thread

/**
 * Created by rarnu on 4/7/16.
 */
open class DaemonStrategyUnder21 : DaemonStrategyBase() {

    private var _alarmManager: AlarmManager? = null
    private var _pendingIntent: PendingIntent? = null

    override fun onInitialization(context: Context?): Boolean = installBinary(context)

    override fun onPersistentCreate(context: Context?, configs: DaemonConfigurations?) {
        initAlarm(context, configs?.DAEMON_ASSISTANT_CONFIG?.serviceName)
        thread(priority = Thread.MAX_PRIORITY) {
            val binaryFile = File(context?.getDir(BINARY_DEST_DIR_NAME, Context.MODE_PRIVATE), BINARY_FILE_NAME)
            try {
                NativeDaemon20(context).doDaemon(context?.packageName, configs?.DAEMON_ASSISTANT_CONFIG?.serviceName, binaryFile.absolutePath)
            } catch(t: Throwable) {

            }
        }
        configs?.LISTENER?.onPersistentStart(context)
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
        _alarmManager?.setRepeating(AlarmManager.ELAPSED_REALTIME, SystemClock.elapsedRealtime(), 100, _pendingIntent)
        android.os.Process.killProcess(android.os.Process.myPid())
    }

    private fun initAlarm(context: Context?, serviceName: String?) {
        if (_alarmManager == null) {
            _alarmManager = context?.getSystemService(Context.ALARM_SERVICE) as AlarmManager
        }
        if (_pendingIntent == null) {
            val intent = Intent()
            val component = ComponentName(context?.packageName, serviceName)
            intent.component = component
            intent.flags = Intent.FLAG_EXCLUDE_STOPPED_PACKAGES
            _pendingIntent = PendingIntent.getService(context, 0, intent, 0)
        }
        _alarmManager?.cancel(_pendingIntent)
    }

    private fun installBinary(context: Context?): Boolean {
        var binaryDirName: String
        val abi = Build.CPU_ABI
        if (abi.startsWith("mips")) {
            binaryDirName = "mips"
        } else if (abi.startsWith("x86")) {
            binaryDirName = "x86"
        } else {
            binaryDirName = "armeabi"
        }
        return install(context, BINARY_DEST_DIR_NAME, binaryDirName, BINARY_FILE_NAME)
    }


}