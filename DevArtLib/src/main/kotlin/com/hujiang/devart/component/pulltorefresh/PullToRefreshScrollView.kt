package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.os.Build
import android.util.AttributeSet
import android.widget.ScrollView
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/31/16.
 */
class PullToRefreshScrollView: PullToRefreshBase<ScrollView> {

    constructor(context: Context): super(context)

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

    constructor(context: Context, mode: Mode): super(context, mode)

    constructor(context: Context, mode: Mode, style: AnimationStyle): super(context, mode, style)

    override fun getPullToRefreshScrollDirection(): Orientation = Orientation.VERTICAL

    override fun createRefreshableView(context: Context, attrs: AttributeSet?): ScrollView {
        var scrollView: ScrollView
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.GINGERBREAD) {
            scrollView = InternalScrollViewSDK9(context, attrs)
        } else {
            scrollView = ScrollView(context, attrs)
        }
        scrollView.id = R.id.scrollview
        return scrollView
    }

    override fun isReadyForPullEnd(): Boolean {
        val scrollViewChild = _refreshableView?.getChildAt(0)
        if (scrollViewChild != null) {
            return _refreshableView!!.scrollY >= (scrollViewChild.height - height)
        }
        return false
    }

    override fun isReadyForPullStart(): Boolean = _refreshableView!!.scrollY == 0

    inner class InternalScrollViewSDK9: ScrollView {

        constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

        override fun overScrollBy(deltaX: Int, deltaY: Int, scrollX: Int, scrollY: Int, scrollRangeX: Int, scrollRangeY: Int, maxOverScrollX: Int, maxOverScrollY: Int, isTouchEvent: Boolean): Boolean {
            val returnValue = super.overScrollBy(deltaX, deltaY, scrollX, scrollY, scrollRangeX, scrollRangeY, maxOverScrollX, maxOverScrollY, isTouchEvent)
            OverscrollHelper.overScrollBy(this@PullToRefreshScrollView, deltaX, scrollX, deltaY, scrollY, getScrollRange(), isTouchEvent)
            return returnValue
        }

        private fun getScrollRange(): Int {
            var scrollRange = 0
            if (childCount > 0) {
                val child = getChildAt(0)
                scrollRange = Math.max(0, child.height - (height - paddingBottom - paddingTop))
            }
            return scrollRange
        }
    }
}