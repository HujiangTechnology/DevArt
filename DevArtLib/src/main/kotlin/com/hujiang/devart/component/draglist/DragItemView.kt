package com.hujiang.devart.component.draglist

import android.content.Context
import android.view.Gravity
import android.view.ViewGroup
import android.widget.AbsListView

/**
 * Created by rarnu on 4/1/16.
 */
class DragItemView: ViewGroup {

    private var _gravity = Gravity.TOP

    constructor(context: Context): super(context) {
        layoutParams = AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT)
    }

    fun getGravity(): Int = _gravity

    fun setGravity(gravity: Int) {
        _gravity = gravity
    }

    override fun onLayout(changed: Boolean, l: Int, t: Int, r: Int, b: Int) {
        val child = getChildAt(0) ?: return
        if (_gravity == Gravity.TOP) {
            child.layout(0, 0, measuredWidth, child.measuredHeight)
        } else {
            child.layout(0, measuredHeight - child.measuredHeight, measuredWidth, measuredHeight)
        }
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        var height = MeasureSpec.getSize(heightMeasureSpec)
        val width = MeasureSpec.getSize(widthMeasureSpec)
        val heightMode = MeasureSpec.getMode(heightMeasureSpec)
        val child = getChildAt(0)
        if (child == null) {
            setMeasuredDimension(0, width)
            return
        }
        if (child.isLayoutRequested) {
            measureChild(child, widthMeasureSpec, MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED))
        }
        if (heightMode == MeasureSpec.UNSPECIFIED) {
            val lp = layoutParams
            if (lp.height > 0) {
                height = lp.height
            } else {
                height = child.measuredHeight
            }
        }
        setMeasuredDimension(width, height)
    }

}