package com.hujiang.devart.component.scroll

import android.content.Context
import android.util.AttributeSet
import android.view.View
import android.widget.ImageView
import android.widget.RelativeLayout
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/29/16.
 */
class PointBar: RelativeLayout {

    private var _layBase: RelativeLayout? = null
    private var _iPoint: Array<ImageView?>? = null

    constructor(context: Context): this(context, null)

    constructor(context: Context, attrs: AttributeSet?): this(context, attrs, 0)

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        init()
    }

    private fun init() {
        _layBase = inflate(context, R.layout.point_bar, null) as RelativeLayout
        _layBase?.layoutParams = RelativeLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT)
        addView(_layBase)
        _iPoint = arrayOfNulls<ImageView>(10)
        for (i in 0..9) {
            _iPoint!![i] = findViewById(resources.getIdentifier("imgP${i}", "id", context.packageName)) as ImageView?
        }
    }

    fun setPoint(point: Int) {
        for (i in 0.._iPoint!!.size - 1) {
            _iPoint!![i]?.setBackgroundResource(if (i == point) R.drawable.point_focus else R.drawable.point)
        }
    }

    fun setPointCount(count: Int) {
        for (i in 0.._iPoint!!.size - 1) {
            _iPoint!![i]?.visibility = if (i < count) View.VISIBLE else View.GONE
        }
    }
}