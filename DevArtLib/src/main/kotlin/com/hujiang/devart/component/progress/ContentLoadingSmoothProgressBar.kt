package com.hujiang.devart.component.progress

import android.content.Context
import android.util.AttributeSet
import android.view.View

/**
 * Created by rarnu on 4/15/16.
 */
class ContentLoadingSmoothProgressBar : SmoothProgressBar {

    companion object {
        private val MIN_SHOW_TIME = 500
        private val MIN_DELAY = 500
    }

    private var _startTime = -1
    private var _postedHide = false
    private var _postedShow = false
    private var _dismissed = false
    private val _delayedHide = Runnable {
        _postedHide = false
        _startTime = -1
        visibility = View.GONE
    }
    private val _delayedShow = Runnable {
        _postedShow = false
        if (!_dismissed) {
            _startTime = System.currentTimeMillis().toInt()
            visibility = View.VISIBLE
        }
    }

    constructor(context: Context): this(context, null)
    constructor(context: Context, attrs: AttributeSet?): super(context, attrs, 0)

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
        removeCallbacks()
    }

    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()
        removeCallbacks()
    }

    private fun removeCallbacks() {
        removeCallbacks(_delayedHide)
        removeCallbacks(_delayedShow)
    }

    fun hide() {
        _dismissed = true
        removeCallbacks(_delayedShow)
        val diff = System.currentTimeMillis() - _startTime
        if (diff >= MIN_SHOW_TIME || _startTime == -1) {
            visibility = View.GONE
        } else {
            if (!_postedHide) {
                postDelayed(_delayedHide, MIN_SHOW_TIME - diff)
                _postedHide = true
            }
        }
    }

    fun show() {
        _startTime = -1
        _dismissed = false
        removeCallbacks(_delayedHide)
        if (!_postedShow) {
            postDelayed(_delayedShow, MIN_DELAY.toLong())
            _postedShow = true
        }
    }

}