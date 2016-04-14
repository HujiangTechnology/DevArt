package com.hujiang.devart.component.glassbar

import android.widget.AbsListView
import android.widget.ListView

/**
 * Created by rarnu on 4/14/16.
 */
class ListViewScrollObserver: AbsListView.OnScrollListener {

    interface OnListViewScrollListener {
        fun onScrollUpDownChanged(delta: Int, scrollPosition: Int, exact: Boolean)
        fun onScrollIdle()
    }

    private var _listener: OnListViewScrollListener? = null
    private var _lastFirstVisibleItem = 0
    private var _lastTop = 0
    private var _scrollPosition = 0
    private var _lastHeight = 0

    constructor(listView: ListView?) {
        listView?.setOnScrollListener(this)
    }

    fun setOnScrollUpAndDownListener(listener: OnListViewScrollListener?) {
        _listener = listener
    }

    override fun onScroll(view: AbsListView?, firstVisibleItem: Int, visibleItemCount: Int, totalItemCount: Int) {
        val firstChild = view?.getChildAt(0) ?: return
        val top = firstChild.top
        val height = firstChild.height
        var delta: Int
        var skipped = 0
        if (_lastFirstVisibleItem == firstVisibleItem) {
            delta = _lastTop - top
        } else if (firstVisibleItem > _lastFirstVisibleItem) {
            skipped = firstVisibleItem - _lastFirstVisibleItem - 1
            delta = skipped * height + _lastHeight + _lastTop - top
        } else {
            skipped = _lastFirstVisibleItem - firstVisibleItem - 1
            delta = skipped * -height + _lastTop - (height + top)
        }
        val exact = skipped == 0
        _scrollPosition += -delta
        _listener?.onScrollUpDownChanged(-delta, _scrollPosition, exact)
        _lastFirstVisibleItem = firstVisibleItem
        _lastTop = top
        _lastHeight = firstChild.height
    }

    override fun onScrollStateChanged(view: AbsListView?, scrollState: Int) {
        if (_listener != null && scrollState == AbsListView.OnScrollListener.SCROLL_STATE_IDLE) {
            _listener?.onScrollIdle()
        }
    }
}