package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.content.res.TypedArray
import android.util.AttributeSet
import android.view.Gravity
import android.view.View
import android.view.ViewGroup
import android.widget.*
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/30/16.
 */
abstract class PullToRefreshAdapterViewBase<T: AbsListView>: PullToRefreshBase<T>, AbsListView.OnScrollListener {

    companion object {
        private fun convertEmptyViewLayoutParams(lp: ViewGroup.LayoutParams?): FrameLayout.LayoutParams? {
            var newLp: FrameLayout.LayoutParams? = null
            if (lp != null) {
                newLp = FrameLayout.LayoutParams(lp)
                if (lp is LinearLayout.LayoutParams) {
                    newLp.gravity = lp.gravity
                } else {
                    newLp.gravity = Gravity.CENTER
                }
            }
            return newLp
        }
    }

    private var _lastItemVisible = false
    private var _onScrollListener: AbsListView.OnScrollListener? = null
    private var _onLastItemVisibleListener: OnLastItemVisibleListener? = null
    private var _emptyView: View? = null
    private var _indicatorIvTop: IndicatorLayout? = null
    private var _indicatorIvBottom: IndicatorLayout? = null
    private var _showIndicator = false
    private var _scrollEmptyView = true

    constructor(context: Context): super(context) {
        _refreshableView?.setOnScrollListener(this)
    }

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        _refreshableView?.setOnScrollListener(this)
    }

    constructor(context: Context, mode: Mode): super(context, mode) {
        _refreshableView?.setOnScrollListener(this)
    }

    constructor(context: Context, mode: Mode, animStyle: AnimationStyle): super(context, mode, animStyle) {
        _refreshableView?.setOnScrollListener(this)
    }

    fun getShowIndicator(): Boolean = _showIndicator

    override fun onScroll(view: AbsListView?, firstVisibleItem: Int, visibleItemCount: Int, totalItemCount: Int) {
        if (_onLastItemVisibleListener != null) {
            _lastItemVisible = (totalItemCount > 0) && (firstVisibleItem + visibleItemCount >= totalItemCount - 1)
        }
        if (getShowIndicatorInternal()) {
            updateIndicatorViewsVisibility()
        }
        _onScrollListener?.onScroll(view, firstVisibleItem, visibleItemCount, totalItemCount)
    }

    private fun getShowIndicatorInternal(): Boolean = _showIndicator && isPullToRefreshEnabled()

    private fun updateIndicatorViewsVisibility() {
        if (_indicatorIvTop != null) {
            if (!isRefreshing() && isReadyForPullStart()) {
                if (!_indicatorIvTop!!.isVisible()) {
                    _indicatorIvTop?.show()
                }
            } else {
                if (_indicatorIvTop!!.isVisible()) {
                    _indicatorIvTop?.hide()
                }
            }
        }
        if (_indicatorIvBottom != null) {
            if (!isRefreshing() && isReadyForPullEnd()) {
                if (!_indicatorIvBottom!!.isVisible()) {
                    _indicatorIvBottom?.show()
                }
            } else {
                if (_indicatorIvBottom!!.isVisible()) {
                    _indicatorIvBottom?.hide()
                }
            }
        }
    }

    override fun onScrollStateChanged(view: AbsListView?, scrollState: Int) {
        if (scrollState == AbsListView.OnScrollListener.SCROLL_STATE_IDLE && _onLastItemVisibleListener != null && _lastItemVisible) {
            _onLastItemVisibleListener?.onLastItemVisible()
        }
        _onScrollListener?.onScrollStateChanged(view, scrollState)
    }

    fun setAdapter(adapter: ListAdapter?) {
        (_refreshableView as AdapterView<ListAdapter>).adapter = adapter
    }

    fun setEmptyView(newEmptyView: View?) {
        val refreshableViewWrapper = getRefreshableViewWrapper()
        if (newEmptyView != null) {
            newEmptyView.isClickable = true
            val newEmptyViewParent = newEmptyView.parent
            if (newEmptyViewParent != null && newEmptyViewParent is ViewGroup) {
                newEmptyViewParent.removeView(newEmptyView)
            }
            val lp = convertEmptyViewLayoutParams(newEmptyView.layoutParams)
            if (lp != null) {
                refreshableViewWrapper?.addView(newEmptyView, lp)
            } else {
                refreshableViewWrapper?.addView(newEmptyView)
            }
        }
        if (_refreshableView is EmptyViewMethodAccessor) {
            (_refreshableView as EmptyViewMethodAccessor).setEmptyViewInternal(newEmptyView)
        } else {
            _refreshableView?.emptyView = newEmptyView
        }
        _emptyView = newEmptyView
    }

    override fun onPullToRefresh() {
        super.onPullToRefresh()
        if (getShowIndicatorInternal()) {
            when (getCurrentMode()) {
                Mode.PULL_FROM_END -> _indicatorIvBottom?.pullToRefresh()
                Mode.PULL_FROM_START -> _indicatorIvTop?.pullToRefresh()
                else -> { }
            }
        }
    }

    override fun onRefreshing(doScroll: Boolean) {
        super.onRefreshing(doScroll)
        if (getShowIndicatorInternal()) {
            updateIndicatorViewsVisibility()
        }
    }

    override fun onReleaseToRefresh() {
        super.onReleaseToRefresh()
        if (getShowIndicatorInternal()) {
            when (getCurrentMode()) {
                Mode.PULL_FROM_END -> _indicatorIvBottom?.releaseToRefresh()
                Mode.PULL_FROM_START -> _indicatorIvTop?.releaseToRefresh()
                else -> { }
            }
        }
    }

    override fun onReset() {
        super.onReset()
        if (getShowIndicatorInternal()) {
            updateIndicatorViewsVisibility()
        }
    }

    override fun handleStyledAttributes(a: TypedArray?) {
        _showIndicator = a!!.getBoolean(R.styleable.PullToRefresh_ptrShowIndicator, !isPullToRefreshOverScrollEnabled())
    }

    override fun onScrollChanged(l: Int, t: Int, oldl: Int, oldt: Int) {
        super.onScrollChanged(l, t, oldl, oldt)
        if (_emptyView != null && !_scrollEmptyView) {
            _emptyView?.scrollTo(-l, -t)
        }
    }

    override fun updateUIForMode() {
        super.updateUIForMode()
        if (getShowIndicatorInternal()) {
            addIndicatorViews()
        } else {
            removeIndicatorViews()
        }
    }

    private fun removeIndicatorViews() {
        if (_indicatorIvTop != null) {
            getRefreshableViewWrapper()?.removeView(_indicatorIvTop)
            _indicatorIvTop = null
        }
        if (_indicatorIvBottom != null) {
            getRefreshableViewWrapper()?.removeView(_indicatorIvBottom)
            _indicatorIvBottom = null
        }
    }

    private fun addIndicatorViews() {
        val mode = getMode()
        val refreshableViewWrapper = getRefreshableViewWrapper()
        if (mode.showHeaderLoadingLayout() && _indicatorIvTop == null) {
            _indicatorIvTop = IndicatorLayout(context, Mode.PULL_FROM_START)
            val params = FrameLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT)
            params.rightMargin = resources.getDimensionPixelSize(R.dimen.indicator_right_padding)
            params.gravity = Gravity.TOP or Gravity.RIGHT
            refreshableViewWrapper?.addView(_indicatorIvTop, params)
        } else if (!mode.showHeaderLoadingLayout() && _indicatorIvTop != null) {
            refreshableViewWrapper?.removeView(_indicatorIvTop)
            _indicatorIvTop = null
        }
        if (mode.showFooterLoadingLayout() && _indicatorIvBottom == null) {
            _indicatorIvBottom = IndicatorLayout(context, Mode.PULL_FROM_END)
            val params = FrameLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT,ViewGroup.LayoutParams.WRAP_CONTENT)
            params.rightMargin = resources.getDimensionPixelSize(R.dimen.indicator_right_padding)
            params.gravity = Gravity.BOTTOM or Gravity.RIGHT
            refreshableViewWrapper?.addView(_indicatorIvBottom, params)
        } else if (!mode.showFooterLoadingLayout() && _indicatorIvBottom != null) {
            refreshableViewWrapper?.removeView(_indicatorIvBottom)
            _indicatorIvBottom = null
        }
    }

    private fun isFirstItemVisible(): Boolean {
        val adapter = _refreshableView?.adapter
        if (adapter == null || adapter.isEmpty) {
            return true
        } else {
            if (_refreshableView!!.firstVisiblePosition <= 1) {
                val firstVisibleChild = _refreshableView?.getChildAt(0)
                if (firstVisibleChild != null) {
                    return firstVisibleChild.top >= _refreshableView!!.top
                }
            }
        }
        return false
    }

    private fun isLastItemVisible(): Boolean {
        val adapter = _refreshableView?.adapter
        if (adapter == null || adapter.isEmpty) {
            return true
        } else {
            val lastItemPosition = _refreshableView!!.count - 1
            val lastVisiblePosition = _refreshableView!!.lastVisiblePosition
            if (lastVisiblePosition >= lastItemPosition - 1) {
                val childIndex = lastVisiblePosition - _refreshableView!!.firstVisiblePosition
                val lastVisibleChild = _refreshableView!!.getChildAt(childIndex)
                if (lastVisibleChild != null) {
                    return lastVisibleChild.bottom <= _refreshableView!!.bottom
                }
            }
        }
        return false
    }

    fun setOnItemClickListener(listener: AdapterView.OnItemClickListener?) {
        _refreshableView?.onItemClickListener = listener
    }

    fun setOnLastItemVisibleListener(listener: OnLastItemVisibleListener?) {
        _onLastItemVisibleListener = listener
    }

    fun setOnScrollListener(listener: AbsListView.OnScrollListener?) {
        _onScrollListener = listener
    }

    fun setScrollEmptyView(doScroll: Boolean) {
        _scrollEmptyView = doScroll
    }

    fun setShowIndicator(showIndicator: Boolean) {
        _showIndicator = showIndicator
        if (getShowIndicatorInternal()) {
            addIndicatorViews()
        } else {
            removeIndicatorViews()
        }
    }

    override fun isReadyForPullStart(): Boolean = isFirstItemVisible()

    override fun isReadyForPullEnd(): Boolean = isLastItemVisible()
}