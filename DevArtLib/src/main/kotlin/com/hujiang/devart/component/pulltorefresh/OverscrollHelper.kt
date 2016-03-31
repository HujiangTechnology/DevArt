package com.hujiang.devart.component.pulltorefresh

import android.view.View

/**
 * Created by rarnu on 3/30/16.
 */
object OverscrollHelper {

    val DEFAULT_OVERSCROLL_SCALE = 1.0f

    fun overScrollBy(view: PullToRefreshBase<*>?, deltaX: Int, scrollX: Int, deltaY: Int, scrollY: Int, isTouchEvent: Boolean) =
            overScrollBy(view, deltaX, scrollX, deltaY, scrollY, 0, isTouchEvent)

    fun overScrollBy(view: PullToRefreshBase<*>?, deltaX: Int, scrollX: Int, deltaY: Int, scrollY: Int, scrollRange: Int, isTouchEvent: Boolean) =
            overScrollBy(view, deltaX, scrollX, deltaY, scrollY, scrollRange, 0, DEFAULT_OVERSCROLL_SCALE, isTouchEvent)


    fun overScrollBy(view: PullToRefreshBase<*>?, deltaX: Int, scrollX: Int, deltaY: Int, scrollY: Int, scrollRange: Int, fuzzyThreshold: Int, scaleFactor: Float, isTouchEvent: Boolean) {

        var deltaValue = 0
        var currentScrollValue = 0
        var scrollValue = 0
        when (view!!.getPullToRefreshScrollDirection()) {
            PullToRefreshBase.Orientation.HORIZONTAL -> {
                deltaValue = deltaX
                scrollValue = scrollX
                currentScrollValue = view.scrollX
            }
            else -> {
                deltaValue = deltaY
                scrollValue = scrollY
                currentScrollValue = view.scrollY
            }
        }

        if (view.isPullToRefreshOverScrollEnabled() && !view.isRefreshing()) {
            val mode = view.getMode()
            if (mode.permitsPullToRefresh() && !isTouchEvent && deltaValue != 0) {
                val newScrollValue = (deltaValue + scrollValue)
                if (newScrollValue < (0 - fuzzyThreshold)) {
                    if (mode.showHeaderLoadingLayout()) {
                        if (currentScrollValue == 0) {
                            view.setState(PullToRefreshBase.State.OVERSCROLLING)
                        }
                        view.setHeaderScroll((scaleFactor * (currentScrollValue + newScrollValue)).toInt())
                    }
                } else if (newScrollValue > (scrollRange + fuzzyThreshold)) {
                    if (mode.showFooterLoadingLayout()) {
                        if (currentScrollValue == 0) {
                            view.setState(PullToRefreshBase.State.OVERSCROLLING)
                        }
                        view.setHeaderScroll((scaleFactor * (currentScrollValue + newScrollValue - scrollRange)).toInt())
                    }
                } else if (Math.abs(newScrollValue) <= fuzzyThreshold
                        || Math.abs(newScrollValue - scrollRange) <= fuzzyThreshold) {
                    view.setState(PullToRefreshBase.State.RESET)
                }
            } else if (isTouchEvent && PullToRefreshBase.State.OVERSCROLLING == view.getState()) {
                view.setState(PullToRefreshBase.State.RESET)
            }
        }
    }

    fun isAndroidOverScrollEnabled(v: View?): Boolean = v!!.overScrollMode != View.OVER_SCROLL_NEVER

}