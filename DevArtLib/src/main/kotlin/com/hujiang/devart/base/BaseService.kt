package com.hujiang.devart.base

import android.app.Notification
import android.app.Service
import android.content.Intent
import android.os.Handler
import android.os.IBinder
import android.os.Message
import com.hujiang.devart.base.common.Actions
import com.hujiang.devart.utils.NotificationUtils

/**
 * Created by rarnu on 3/23/16.
 */
abstract class BaseService: Service() {

    abstract fun initIntent()

    abstract fun fiIntent()

    abstract fun showNotification(): Boolean

    abstract fun getSendIntent(): Intent?

    abstract fun doOperation(command: String, n: Notification?)

    abstract fun getCommandCondition(command: String): Boolean

    abstract fun getIcon24(): Int

    private var operating = false

    override fun onBind(intent: Intent?): IBinder? = null

    private fun doSendMessage() {
        Thread({
            while (true) {
                if (!operating) {
                    break
                }
                sendBroadcast(getSendIntent())
                Thread.sleep(500)
            }
        }).start()
    }

    private fun operation(command: String, id: Int, title: Int, desc: Int, procId: Int, procTitle: Int, procDesc: Int) {
        val n = NotificationUtils.buildNotification(applicationContext, id, getIcon24(), procTitle, procDesc, Actions.ACTION_NOTIFY_NULL, null, false)
        if (showNotification()) {
            startForeground(procId, n)
        }
        val h = object : Handler() {
            override fun handleMessage(msg: Message?) {
                if (msg!!.what == 1) {
                    operating = false
                    fiIntent()
                    sendBroadcast(getSendIntent())
                    if (showNotification()) {
                        stopForeground(true)
                        doNotification(id, title, desc, true, Actions.ACTION_NOTIFY)
                    }
                }
                super.handleMessage(msg)
            }
        }
        Thread({
            doOperation(command, n)
            h.sendEmptyMessage(1)
        }).start()
    }

    private fun doNotification(id: Int, title: Int, desc: Int, canClose: Boolean, action: String) =
            NotificationUtils.showNotification(applicationContext, id, getIcon24(), title, desc, action, canClose)

    /**
     * you may pass the parameters via INTENT
     *
     * intent.getStringExtra("command")
     *
     * intent.getIntExtra("id", 0)
     *
     * intent.getIntExtra("title", 0)
     *
     * intent.getIntExtra("desc", 0)
     *
     * intent.getIntExtra("procId", 0)
     *
     * intent.getIntExtra("procTitle", 0)
     *
     * intent.getIntExtra("procDesc", 0)
     *
     */
    override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
        if (intent != null) {
            val command = intent.getStringExtra("command")
            val id = intent.getIntExtra("id", 0)
            val title = intent.getIntExtra("title", 0)
            val desc = intent.getIntExtra("desc", 0)
            val procId = intent.getIntExtra("procId", 0)
            val procTitle = intent.getIntExtra("procTitle", 0)
            val procDesc = intent.getIntExtra("procDesc", 0)
            if (command != null) {
                if (getCommandCondition(command)) {
                    operating = true
                    initIntent()
                    doSendMessage()
                    operation(command, id, title, desc, procId, procTitle, procDesc)
                }
            }
        }
        return super.onStartCommand(intent, flags, startId)
    }
}