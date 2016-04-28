package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.content.res.TypedArray
import android.graphics.Canvas
import android.os.Build
import android.util.AttributeSet
import android.view.Gravity
import android.view.MotionEvent
import android.view.View
import android.widget.FrameLayout
import android.widget.ListAdapter
import android.widget.ListView
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/31/16.
 */
class PullToRefreshListView: PullToRefreshAdapterViewBase<ListView> {

    private var _headerLoadingView: LoadingLayout? = null
    private var _footerLoadingView: LoadingLayout? = null
    private var _lvFooterLoadingFrame: FrameLayout? = null
    private var _listViewExtrasEnabled = false

    constructor(context: Context): super(context)

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

    constructor(context: Context, mode: Mode): super(context, mode)

    constructor(context: Context, mode: Mode, style: AnimationStyle): super(context, mode, style)

    override fun getPullToRefreshScrollDirection(): Orientation = Orientation.VERTICAL

    override fun createRefreshableView(context: Context, attrs: AttributeSet?): ListView {
        val lv = createListView(context, attrs)
        lv.id = android.R.id.list
        return lv
    }

    protected fun createListView(context: Context, attrs: AttributeSet?): ListView {
        var lv: ListView
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.GINGERBREAD) {
            lv = InternalListViewSDK9(context, attrs)
        } else {
            lv = InternalListView(context, attrs)
        }
        return lv
    }

    override fun handleStyledAttributes(a: TypedArray?) {
        super.handleStyledAttributes(a)
        _listViewExtrasEnabled = a!!.getBoolean(R.styleable.PullToRefresh_ptrListViewExtrasEnabled, true)
        if (_listViewExtrasEnabled) {
            val lp = FrameLayout.LayoutParams(FrameLayout.LayoutParams.MATCH_PARENT, FrameLayout.LayoutParams.WRAP_CONTENT, Gravity.CENTER_HORIZONTAL)
            val frame = FrameLayout(context)
            _headerLoadingView = createLoadingLayout(context, Mode.PULL_FROM_START, a)
            _headerLoadingView?.visibility = View.GONE
            frame.addView(_headerLoadingView, lp)
            _refreshableView?.addHeaderView(frame, null, false)
            _lvFooterLoadingFrame = FrameLayout(context)
            _footerLoadingView = createLoadingLayout(context, Mode.PULL_FROM_END, a)
            _footerLoadingView?.visibility = View.GONE
            _lvFooterLoadingFrame?.addView(_footerLoadingView, lp)
            if (!a.hasValue(R.styleable.PullToRefresh_ptrScrollingWhileRefreshingEnabled)) {
                setScrollingWhileRefreshingEnabled(true)
            }
        }
    }

    override fun createLoadingLayoutProxy(includeStart: Boolean, includeEnd: Boolean): LoadingLayoutProxy? {
        val proxy = super.createLoadingLayoutProxy(includeStart, includeEnd)
        if (_listViewExtrasEnabled) {
            val mode = getMode()
            if (includeStart && mode.showHeaderLoadingLayout()) {
                proxy?.addLayout(_headerLoadingView)
            }
            if (includeEnd && mode.showFooterLoadingLayout()) {
                proxy?.addLayout(_footerLoadingView)
            }
        }
        return proxy
    }

    override fun onReset() {
        if (!_listViewExtrasEnabled) {
            super.onReset()
            return
        }
        var originalLoadingLayout: LoadingLayout?
        var listViewLoadingLayout: LoadingLayout?
        var scrollToHeight: Int
        var selection: Int
        var scrollLvToEdge: Boolean

        when (getCurrentMode()) {
            Mode.MANUAL_REFRESH_ONLY,
            Mode.PULL_FROM_END -> {
                originalLoadingLayout = getFooterLayout()
                listViewLoadingLayout = _footerLoadingView
                selection = _refreshableView!!.count - 1
                scrollToHeight = getFooterSize()
                scrollLvToEdge = Math.abs(_refreshableView!!.lastVisiblePosition - selection) <= 1
            }
            else -> {
                originalLoadingLayout = getHeaderLayout()
                listViewLoadingLayout = _headerLoadingView
                scrollToHeight = -getHeaderSize()
                selection = 0
                scrollLvToEdge = Math.abs(_refreshableView!!.firstVisiblePosition - selection) <= 1
            }
        }
        if (listViewLoadingLayout!!.visibility == View.VISIBLE) {
            originalLoadingLayout?.showInvisibleViews()
            listViewLoadingLayout.visibility = View.GONE
            if (scrollLvToEdge && getState() != State.MANUAL_REFRESHING) {
                _refreshableView?.setSelection(selection)
                setHeaderScroll(scrollToHeight)
            }
        }
        super.onReset()
    }

    override fun onRefreshing(doScroll: Boolean) {
        val adapter = _refreshableView!!.adapter
        if (!_listViewExtrasEnabled || !getShowViewWhileRefreshing() || adapter == null || adapter.isEmpty) {
            super.onRefreshing(doScroll)
            return
        }
        super.onRefreshing(false)
        var origLoadingView: LoadingLayout?
        var listViewLoadingView: LoadingLayout?
        var oppositeListViewLoadingView: LoadingLayout?
        var selection: Int
        var scrollToY: Int
        when (getCurrentMode()) {
            Mode.MANUAL_REFRESH_ONLY,
            Mode.PULL_FROM_END -> {
                origLoadingView = getFooterLayout()
                listViewLoadingView = _footerLoadingView
                oppositeListViewLoadingView = _headerLoadingView
                selection = _refreshableView!!.count - 1
                scrollToY = scrollY - getFooterSize()
            }
            else -> {
                origLoadingView = getHeaderLayout()
                listViewLoadingView = _headerLoadingView
                oppositeListViewLoadingView = _footerLoadingView
                selection = 0
                scrollToY = scrollY + getHeaderSize()
            }
        }
        origLoadingView?.reset()
        origLoadingView?.hideAllViews()
        oppositeListViewLoadingView?.visibility = View.GONE
        listViewLoadingView?.visibility = View.VISIBLE
        listViewLoadingView?.refreshing()
        if (doScroll) {
            disableLoadingLayoutVisibilityChanges()
            setHeaderScroll(scrollToY)
            _refreshableView?.setSelection(selection)
            smoothScrollTo(0)
        }
    }

    inner class InternalListViewSDK9: InternalListView {

        constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

        override fun overScrollBy(deltaX: Int, deltaY: Int, scrollX: Int, scrollY: Int, scrollRangeX: Int, scrollRangeY: Int, maxOverScrollX: Int, maxOverScrollY: Int, isTouchEvent: Boolean): Boolean {
            val returnValue = super.overScrollBy(deltaX, deltaY, scrollX, scrollY, scrollRangeX, scrollRangeY, maxOverScrollX, maxOverScrollY, isTouchEvent)
            OverscrollHelper.overScrollBy(this@PullToRefreshListView, deltaX, scrollX, deltaY, scrollY, isTouchEvent)
            return returnValue
        }
    }

    inner open class InternalListView: ListView, EmptyViewMethodAccessor {

        private var _addedLvFooter = false

        constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

        override fun dispatchDraw(canvas: Canvas?) {
            try {
                super.dispatchDraw(canvas)
            } catch (e: Exception) {
            }
        }

        override fun dispatchTouchEvent(ev: MotionEvent?): Boolean {
            try {
                return super.dispatchTouchEvent(ev)
            } catch (e: Exception) {
                return false
            }
        }

        override fun setAdapter(adapter: ListAdapter?) {
            if (null != _lvFooterLoadingFrame && !_addedLvFooter) {
                addFooterView(_lvFooterLoadingFrame, null, false)
                _addedLvFooter = true
            }
            super.setAdapter(adapter)
        }

        override fun setEmptyView(emptyView: View?) {
            this@PullToRefreshListView.setEmptyView(emptyView)
        }

        override fun setEmptyViewInternal(emptyView: View?) {
            super.setEmptyView(emptyView)
        }
    }

}