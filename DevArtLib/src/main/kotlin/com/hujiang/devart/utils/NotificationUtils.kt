package com.hujiang.devart.utils

import android.app.Notification
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import java.io.Serializable

/**
 * Created by rarnu on 3/23/16.
 */
object NotificationUtils {

    fun cancelNotication(context: Context, id: Int) {
        val mgr = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        mgr.cancel(id)
    }

    fun cancalAllNotification(context: Context, ids: IntArray?) {
        if (ids != null && ids.size > 0) {
            for (id in ids) {
                cancelNotication(context, id)
            }
        }
    }

    fun showNotification(context: Context, id: Int, icon: Int, title: Int, desc: Int, action: String, obj: Serializable?, canClose: Boolean) {
        val mgr = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        mgr.cancel(id)
        val n = buildNotification(context, id, icon, title, desc, action, obj, canClose)
        mgr.notify(id, n)
    }

    fun showNotification(context: Context, id: Int, icon: Int, title: Int, desc: Int, action: String, canClose: Boolean) =
            showNotification(context, id, icon, title, desc, action, null, canClose)

    fun buildNotification(context: Context, id: Int, icon: Int, title: Int, desc: Int, action: String, obj: Serializable?, canClose: Boolean): Notification? {
        val inMain = Intent(action)
        inMain.putExtra("id", id)
        if (obj != null) {
            inMain.putExtra("object", obj)
        }
        val pMain = PendingIntent.getBroadcast(context, 0, inMain, PendingIntent.FLAG_UPDATE_CURRENT)
        val n = Notification.Builder(context)
                .setSmallIcon(icon)
                .setTicker(context.getString(title))
                .setWhen(System.currentTimeMillis())
                .setContentTitle(context.getString(title))
                .setContentText(context.getString(desc))
                .setContentIntent(pMain)
                .build()

        if (canClose) {
            n.defaults = n.defaults or Notification.DEFAULT_SOUND
        }
        n.defaults = n.defaults or Notification.DEFAULT_LIGHTS
        n.ledARGB = 0xff00ff00.toInt()
        n.ledOnMS = 300
        n.ledOffMS = 1000
        n.flags = n.flags or Notification.FLAG_SHOW_LIGHTS
        if (!canClose) {
            n.flags = n.flags or Notification.FLAG_ONGOING_EVENT
        }
        return n
    }

}