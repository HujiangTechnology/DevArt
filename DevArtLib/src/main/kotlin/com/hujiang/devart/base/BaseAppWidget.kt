package com.hujiang.devart.base

import android.app.PendingIntent
import android.appwidget.AppWidgetManager
import android.appwidget.AppWidgetProvider
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.graphics.Bitmap
import android.widget.RemoteViews

/**
 * Created by rarnu on 3/27/16.
 */
abstract class BaseAppWidget: AppWidgetProvider() {

    protected var views: RemoteViews? = null

    protected abstract fun getWidgetLayoutResId(): Int

    protected abstract fun initComponents(context: Context?, views: RemoteViews?)

    protected abstract fun initEvents(context: Context?, views: RemoteViews?)

    protected abstract fun initLogic(context: Context?, views: RemoteViews?)

    abstract fun onWidgetClick(context: Context?, action: String?)

    protected fun reinit(context: Context?) {
        views = RemoteViews(context?.packageName, getWidgetLayoutResId())
    }

    protected fun sync(context: Context?, clz: Class<*>?) {
        val cn = ComponentName(context, clz)
        AppWidgetManager.getInstance(context).updateAppWidget(cn, views)
    }

    override fun onUpdate(context: Context?, appWidgetManager: AppWidgetManager?, appWidgetIds: IntArray?) {
        views = RemoteViews(context?.packageName, getWidgetLayoutResId())
        init(context, views)
        appWidgetManager?.updateAppWidget(appWidgetIds, views)
    }

    private fun init(context: Context?, views: RemoteViews?) {
        initComponents(context, views)
        initEvents(context, views)
        initLogic(context, views)
    }

     fun reUpdate(context: Context?, clz: Class<*>?) {
        views = RemoteViews(context?.packageName, getWidgetLayoutResId())
        val manager = AppWidgetManager.getInstance(context)
        val cn = ComponentName(context, clz)
        init(context, views)
        manager.updateAppWidget(cn, views)
    }

    override fun onReceive(context: Context?, intent: Intent?) {
        super.onReceive(context, intent)
        val action = intent?.action
        if (action != null) {
            if (action != AppWidgetManager.ACTION_APPWIDGET_UPDATE
                    && action != AppWidgetManager.ACTION_APPWIDGET_DELETED
                    && action != AppWidgetManager.ACTION_APPWIDGET_ENABLED
                    && action != AppWidgetManager.ACTION_APPWIDGET_OPTIONS_CHANGED
                    && action != AppWidgetManager.ACTION_APPWIDGET_DISABLED) {
                onWidgetClick(context, action)
            }
        }
    }

    /**
     * @param context
     * @param mode    1:activity  2:service  3:broadcast
     * @param inEvent
     * @param viewId
     */
    fun registerEvent(context: Context?, mode: Int, inEvent: Intent?, viewId: Int) {
        var pi: PendingIntent? = null
        when (mode) {
            1 -> pi = PendingIntent.getActivity(context, 0, inEvent, 0)
            2 -> pi = PendingIntent.getService(context, 0, inEvent, 0)
            3 -> pi = PendingIntent.getBroadcast(context, 0, inEvent, 0)
        }
        views?.setOnClickPendingIntent(viewId, pi)
    }

    /**
     * @param context
     * @param action  must be registered in AndroidManifest
     * @param viewId
     */
    fun registerEvent(context: Context?, action: String?, viewId: Int) {
        val inClick = Intent(action)
        val pi = PendingIntent.getBroadcast(context, 0, inClick, 0)
        views?.setOnClickPendingIntent(viewId, pi)
    }

    fun setViewText(@Suppress("UNUSED_PARAMETER") context: Context?, componentId: Int, text: String?) = views?.setTextViewText(componentId, text)

    fun setViewImage(@Suppress("UNUSED_PARAMETER") context: Context?, componentId: Int, bmp: Bitmap?) = views?.setImageViewBitmap(componentId, bmp)

    fun setViewProgress(@Suppress("UNUSED_PARAMETER") context: Context?, componentId: Int, max: Int, progress: Int, indeterminate: Boolean) = views?.setProgressBar(componentId, max, progress, indeterminate)

    fun setViewVisibility(@Suppress("UNUSED_PARAMETER") context: Context?, componentId: Int, visibility: Int) = views?.setViewVisibility(componentId, visibility)

    fun setViewTextColor(@Suppress("UNUSED_PARAMETER") context: Context?, componentId: Int, color: Int) = views?.setTextColor(componentId, color)

}