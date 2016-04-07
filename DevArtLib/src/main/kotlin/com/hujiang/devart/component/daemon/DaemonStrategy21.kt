package com.hujiang.devart.component.daemon

import android.app.AlarmManager
import android.app.PendingIntent
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.os.SystemClock
import java.io.File
import kotlin.concurrent.thread

/**
 * Created by rarnu on 4/7/16.
 */
class DaemonStrategy21: DaemonStrategyBase() {

    private var _alarmManager: AlarmManager? = null
    private var _pendingIntent: PendingIntent? = null

    override fun onPersistentCreate(context: Context?, configs: DaemonConfigurations?) {
        val intent = Intent()
        val componentName = ComponentName(context?.packageName, configs?.DAEMON_ASSISTANT_CONFIG?.serviceName)
        intent.component = componentName
        context?.startService(intent)
        initAlarm(context, configs?.PERSISTENT_CONFIG?.serviceName)
        thread(priority = Thread.MAX_PRIORITY) {
            val indicatorDir = context?.getDir(INDICATOR_DIR_NAME, Context.MODE_PRIVATE)
            NativeDaemon21(context).doDaemon(File(indicatorDir, INDICATOR_PERSISTENT_FILENAME).absolutePath, File(indicatorDir, INDICATOR_DAEMON_ASSISTANT_FILENAME).absolutePath, File(indicatorDir, OBSERVER_PERSISTENT_FILENAME).absolutePath, File(indicatorDir, OBSERVER_DAEMON_ASSISTANT_FILENAME).absolutePath)
        }
        if(configs?.LISTENER != null){
            _configs = configs
            _configs?.LISTENER?.onPersistentStart(context)
        }
    }

    override fun onDaemonAssistantCreate(context: Context?, configs: DaemonConfigurations?) {
        val intent = Intent()
        val componentName = ComponentName(context?.packageName, configs?.PERSISTENT_CONFIG?.serviceName)
        intent.component = componentName
        context?.startService(intent)
        initAlarm(context, configs?.PERSISTENT_CONFIG?.serviceName)
        thread(priority = Thread.MAX_PRIORITY) {
            val indicatorDir = context?.getDir(INDICATOR_DIR_NAME, Context.MODE_PRIVATE)
            NativeDaemon21(context).doDaemon(File(indicatorDir, INDICATOR_DAEMON_ASSISTANT_FILENAME).absolutePath, File(indicatorDir, INDICATOR_PERSISTENT_FILENAME).absolutePath, File(indicatorDir, OBSERVER_DAEMON_ASSISTANT_FILENAME).absolutePath, File(indicatorDir, OBSERVER_PERSISTENT_FILENAME).absolutePath)
        }
        if(configs?.LISTENER != null){
            _configs = configs
            configs?.LISTENER?.onDaemonAssistantStart(context)
        }
    }

    override fun onDaemonDead() {
        _alarmManager?.setRepeating(AlarmManager.ELAPSED_REALTIME, SystemClock.elapsedRealtime(), 100, _pendingIntent)
        _configs?.LISTENER?.onWatchDaemonDaed()
        android.os.Process.killProcess(android.os.Process.myPid())
    }

    private fun initAlarm(context: Context?, serviceName: String?){
        if(_alarmManager == null){
            _alarmManager = context?.getSystemService(Context.ALARM_SERVICE) as AlarmManager
        }
        if(_pendingIntent == null){
            val intent = Intent()
            val component = ComponentName(context?.packageName, serviceName)
            intent.component = component
            intent.flags = Intent.FLAG_EXCLUDE_STOPPED_PACKAGES
            _pendingIntent = PendingIntent.getService(context, 0, intent, 0)
        }
        _alarmManager?.cancel(_pendingIntent)
    }
}