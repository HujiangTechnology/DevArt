package com.hujiang.devart.utils

import android.app.AlarmManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import java.util.*

/**
 * Created by rarnu on 3/28/16.
 */
object AlarmUtils {

    fun startAlarmOnce(context: Context, type: Int, index: Int, hour: Int, minute: Int, action: String) {
        val calendar = Calendar.getInstance()
        calendar.timeInMillis = System.currentTimeMillis()
        calendar.set(Calendar.HOUR_OF_DAY, hour)
        calendar.set(Calendar.MINUTE, minute)
        calendar.set(Calendar.SECOND, 0)
        calendar.set(Calendar.MILLISECOND, 0)

        if (calendar.timeInMillis < System.currentTimeMillis()) {
            calendar.timeInMillis = System.currentTimeMillis() + (24 * 60 * 60 * 1000)
            calendar.set(Calendar.HOUR_OF_DAY, hour)
            calendar.set(Calendar.MINUTE, minute)
            calendar.set(Calendar.SECOND, 0)
            calendar.set(Calendar.MILLISECOND, 0)
        }

        val intent = Intent(action)
        intent.putExtra("index", index)
        val pendingIntent = PendingIntent.getBroadcast(context, index, intent, 0)
        val am = context.getSystemService(Context.ALARM_SERVICE) as AlarmManager
        am.set(type, calendar.timeInMillis, pendingIntent)
    }

    fun startAlarmOnce(context: Context, type: Int, index: Int, interval: Long, action: String) {
        val c = Calendar.getInstance()
        c.add(Calendar.MILLISECOND, interval.toInt())
        val intent = Intent(action)
        intent.putExtra("index", index)
        val pendingIntent = PendingIntent.getBroadcast(context, index, intent, 0)
        val am = context.getSystemService(Context.ALARM_SERVICE) as AlarmManager
        am.setExact(type, c.timeInMillis, pendingIntent)
    }

    fun startAlarmRepeat(context: Context, type: Int, index: Int, interval: Long, action: String) {
        val intent = Intent(action)
        intent.putExtra("index", index)
        val pendingIntent = PendingIntent.getBroadcast(context, index, intent, 0)
        val am = context.getSystemService(Context.ALARM_SERVICE) as AlarmManager
        am.setRepeating(type, System.currentTimeMillis(), interval, pendingIntent)
    }

    fun cancelAlarm(context: Context, index: Int, action: String) {
        val intent = Intent(action)
        intent.putExtra("index", index)
        val sender = PendingIntent.getBroadcast(context, index, intent, 0)
        val am = context.getSystemService(Context.ALARM_SERVICE) as AlarmManager
        am.cancel(sender)
    }

}