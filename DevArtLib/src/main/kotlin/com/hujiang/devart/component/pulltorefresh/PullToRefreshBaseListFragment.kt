package com.hujiang.devart.component.pulltorefresh

import android.app.ListFragment
import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.AbsListView
import android.widget.ListView

/**
 * Created by rarnu on 3/31/16.
 */
abstract class PullToRefreshBaseListFragment<T: PullToRefreshBase<out AbsListView>>: ListFragment() {

    private var _pullToRefreshListView: T? = null

    protected abstract fun onCreatePullToRefreshListView(inflater: LayoutInflater?, savedInstanceState: Bundle?): T

    override fun onCreateView(inflater: LayoutInflater?, container: ViewGroup?, savedInstanceState: Bundle?): View? {
        val layout = super.onCreateView(inflater, container, savedInstanceState)
        val lv = layout.findViewById(android.R.id.list) as ListView
        val parent = lv.parent as ViewGroup
        val lvIndex = parent.indexOfChild(lv)
        parent.removeViewAt(lvIndex)
        _pullToRefreshListView = onCreatePullToRefreshListView(inflater, savedInstanceState)
        parent.addView(_pullToRefreshListView, lvIndex, lv.layoutParams)
        return layout
    }

    fun getPullToRefreshListView(): T? = _pullToRefreshListView

}