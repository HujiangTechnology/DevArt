package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.support.v4.view.ViewPager
import android.util.AttributeSet
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/31/16.
 */
class PullToRefreshViewPager: PullToRefreshBase<ViewPager> {

    constructor(context: Context): super(context)

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

    override fun getPullToRefreshScrollDirection(): Orientation = Orientation.HORIZONTAL

    override fun createRefreshableView(context: Context, attrs: AttributeSet?): ViewPager {
        val viewPager = ViewPager(context, attrs)
        viewPager.id = R.id.viewpager
        return viewPager
    }

    override fun isReadyForPullEnd(): Boolean {
        val refreshableView = getRefreshableView()
        val adapter = refreshableView?.adapter
        if (adapter != null) {
            return refreshableView?.currentItem == adapter.count - 1
        }
        return false
    }

    override fun isReadyForPullStart(): Boolean {
        val refreshableView = getRefreshableView()
        val adapter = refreshableView?.adapter
        if (adapter != null) {
            return refreshableView?.currentItem == 0
        }
        return false
    }
}