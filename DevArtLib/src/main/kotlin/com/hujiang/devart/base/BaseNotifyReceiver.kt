package com.hujiang.devart.base

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import com.hujiang.devart.base.common.Actions
import com.hujiang.devart.utils.NotificationUtils

/**
 * Created by rarnu on 3/25/16.
 */
abstract class BaseNotifyReceiver: BroadcastReceiver() {
    override fun onReceive(context: Context?, intent: Intent?) {
        if (intent == null) {
            return
        }
        val action = intent.action
        if (action == null || action == "") {
            return
        }
        if (action == Actions.ACTION_NOTIFY) {
            val id = intent.getIntExtra("id", 0)
            onReceiveNotify(context, id)
            NotificationUtils.cancalAllNotification(context!!, intArrayOf(id))

        }
    }

    abstract fun onReceiveNotify(context: Context?, id: Int)
}