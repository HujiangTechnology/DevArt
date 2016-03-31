package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.os.Build
import android.util.AttributeSet
import android.view.View
import android.widget.GridView
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/31/16.
 */
class PullToRefreshGridView: PullToRefreshAdapterViewBase<GridView> {

    constructor(context: Context): super(context)

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

    constructor(context: Context, mode: Mode): super(context, mode)

    constructor(context: Context, mode: Mode, style: AnimationStyle): super(context, mode, style)

    override fun getPullToRefreshScrollDirection(): Orientation = Orientation.VERTICAL

    override fun createRefreshableView(context: Context, attrs: AttributeSet?): GridView {
        var gv: GridView
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.GINGERBREAD) {
            gv = InternalGridViewSDK9(context, attrs)
        } else {
            gv = InternalGridView(context, attrs)
        }
        gv.id = R.id.gridview
        return gv
    }

    inner open class InternalGridView: GridView, EmptyViewMethodAccessor {

        constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

        override fun setEmptyViewInternal(emptyView: View?) {
            super.setEmptyView(emptyView)
        }

        override fun setEmptyView(emptyView: View?) {
            this@PullToRefreshGridView.setEmptyView(emptyView)
        }

    }

    inner class InternalGridViewSDK9: InternalGridView {

        constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

        override fun overScrollBy(deltaX: Int, deltaY: Int, scrollX: Int, scrollY: Int, scrollRangeX: Int, scrollRangeY: Int, maxOverScrollX: Int, maxOverScrollY: Int, isTouchEvent: Boolean): Boolean {
            val returnValue = super.overScrollBy(deltaX, deltaY, scrollX, scrollY, scrollRangeX, scrollRangeY, maxOverScrollX, maxOverScrollY, isTouchEvent)
            OverscrollHelper.overScrollBy(this@PullToRefreshGridView, deltaX, scrollX, deltaY, scrollY, isTouchEvent)
            return returnValue
        }
    }
}