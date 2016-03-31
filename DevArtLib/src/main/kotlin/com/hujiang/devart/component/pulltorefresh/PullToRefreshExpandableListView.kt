package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.os.Build
import android.util.AttributeSet
import android.view.View
import android.widget.ExpandableListView

/**
 * Created by rarnu on 3/31/16.
 */
class PullToRefreshExpandableListView: PullToRefreshAdapterViewBase<ExpandableListView> {


    constructor(context: Context): super(context)

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

    constructor(context: Context, mode: Mode): super(context, mode)

    constructor(context: Context, mode: Mode, style: AnimationStyle): super(context, mode, style)



    override fun getPullToRefreshScrollDirection(): Orientation =Orientation.VERTICAL

    override fun createRefreshableView(context: Context, attrs: AttributeSet?): ExpandableListView {
        var lv: ExpandableListView
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.GINGERBREAD) {
            lv = InternalExpandableListViewSDK9(context, attrs)
        } else {
            lv = InternalExpandableListView(context, attrs)
        }
        lv.id = android.R.id.list
        return lv
    }

    inner open class InternalExpandableListView: ExpandableListView, EmptyViewMethodAccessor {

        constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

        override fun setEmptyViewInternal(emptyView: View?) {
            super.setEmptyView(emptyView)
        }

        override fun setEmptyView(emptyView: View?) {
            this@PullToRefreshExpandableListView.setEmptyView(emptyView)
        }
    }

    inner class InternalExpandableListViewSDK9: InternalExpandableListView {

        constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

        override fun overScrollBy(deltaX: Int, deltaY: Int, scrollX: Int, scrollY: Int, scrollRangeX: Int, scrollRangeY: Int, maxOverScrollX: Int, maxOverScrollY: Int, isTouchEvent: Boolean): Boolean {
            val returnValue = super.overScrollBy(deltaX, deltaY, scrollX, scrollY, scrollRangeX, scrollRangeY, maxOverScrollX, maxOverScrollY, isTouchEvent)
            OverscrollHelper.overScrollBy(this@PullToRefreshExpandableListView, deltaX, scrollX, deltaY, scrollY, isTouchEvent)
            return returnValue
        }

    }
}