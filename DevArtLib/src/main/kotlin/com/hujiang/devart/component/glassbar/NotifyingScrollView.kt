package com.hujiang.devart.component.glassbar

import android.content.Context
import android.os.Build
import android.util.AttributeSet
import android.widget.ScrollView

/**
 * Created by rarnu on 4/14/16.
 */
class NotifyingScrollView: ScrollView {

    interface OnScrollChangedListener {
        fun onScrollChanged(who: ScrollView?, l: Int, t: Int, oldl: Int, oldt: Int)
    }

    private var _disableEdgeEffects = true
    private var _onScrollChangedListener: OnScrollChangedListener? = null

    constructor(context: Context): super(context)
    constructor(context: Context, attrs: AttributeSet?): super(context, attrs)
    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle)

    override fun onScrollChanged(l: Int, t: Int, oldl: Int, oldt: Int) {
        super.onScrollChanged(l, t, oldl, oldt)
        _onScrollChangedListener?.onScrollChanged(this, l, t, oldl, oldt)
    }

    fun setOnScrollChangedListener(listener: OnScrollChangedListener?) {
        _onScrollChangedListener = listener
    }

    override fun getTopFadingEdgeStrength(): Float {
        if (_disableEdgeEffects && Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB) {
            return 0.0f
        }
        return super.getTopFadingEdgeStrength()
    }

    override fun getBottomFadingEdgeStrength(): Float {
        if (_disableEdgeEffects && Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB) {
            return 0.0f
        }
        return super.getBottomFadingEdgeStrength()
    }


}