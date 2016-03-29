package com.hujiang.devart.component.scroll

import android.content.Context
import android.util.AttributeSet
import android.view.*
import android.widget.Scroller

/**
 * Created by rarnu on 3/29/16.
 */
class VScrollLayout : ViewGroup {

    companion object {
        val TOUCH_STATE_REST = 0
        val TOUCH_STATE_SCROLLING = 1
        val SNAP_VELOCITY = 50
    }

    private var _enableScroll = true
    var enableScroll: Boolean
        get() = _enableScroll
        set(value) { _enableScroll = value }
    private var _scroller: Scroller? = null
    private var _velocityTracker: VelocityTracker? = null
    private var _curScreen = 0
    var curScreen: Int = 0
        get() = _curScreen
    private var _defaultScreen = 0
    private var _screenChangeListener: OnScreenChangeListener? = null
    var screenChangeListener: OnScreenChangeListener?
        get() = _screenChangeListener
        set(value) { _screenChangeListener = value }
    private var _touchListener: OnScreenTouchListener? = null
    var touchListener: OnScreenTouchListener?
        get() = _touchListener
        set(value) { _touchListener = value }
    private var _touchState = TOUCH_STATE_REST
    var touchState: Int
        get() = _touchState
        set(value) {
            _touchState = value
            if (_touchState == TOUCH_STATE_SCROLLING) {
                _touchListener?.onActionScrolling(this)
            }
            if (_touchState == TOUCH_STATE_REST) {
                _touchListener?.onActionReset(this)
            }
        }
    private var _touchSlop = 0
    private var _lastMotionY = 0.0f
    private var _baseDelta = 5
    var baseDelta: Int
        get() = _baseDelta
        set(value) { _baseDelta = value }

    constructor(context: Context) : this(context, null)

    constructor(context: Context, attrs: AttributeSet?) : this(context, attrs, 0)

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int) : super(context, attrs, defStyle) {
        _scroller = Scroller(context)
        _curScreen = _defaultScreen
        _touchSlop = ViewConfiguration.get(context).scaledTouchSlop * 4
        touchState = TOUCH_STATE_REST
    }

    override fun onLayout(changed: Boolean, l: Int, t: Int, r: Int, b: Int) {
        var childTop = 0
        for (i in 0..childCount - 1) {
            val childView = getChildAt(i)
            if (childView.visibility != View.GONE) {
                val childHeight = childView.measuredHeight
                childView.layout(0, childTop, childView.measuredWidth, childTop + childHeight)
                childTop += childHeight
            }
        }
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec)
        val height = MeasureSpec.getSize(heightMeasureSpec)
        val widthMode = MeasureSpec.getMode(widthMeasureSpec)
        if (widthMode != MeasureSpec.EXACTLY) {
            throw IllegalStateException("exception_not_exactly_mode")
        }
        val heightMode = MeasureSpec.getMode(heightMeasureSpec)
        if (heightMode != MeasureSpec.EXACTLY) {
            throw IllegalStateException("exception_not_exactly_mode")
        }
        for (i in 0..childCount - 1) {
            getChildAt(i).measure(widthMeasureSpec, heightMeasureSpec)
        }
        scrollTo(0, _curScreen * height)
    }

    fun snapToDestination() = snapToScreen((scrollY + height / 2) / height)

    fun snapToScreen(whichScreen: Int) {
        val nwhichScreen = Math.max(0, Math.min(whichScreen, childCount - 1))
        if (scrollY != (nwhichScreen * height)) {
            val delta = nwhichScreen * height - scrollY
            _scroller?.startScroll(0, scrollY, 0, delta, 50) // Math.abs(delta) * 2)
            _curScreen = nwhichScreen
            invalidate()
            _screenChangeListener?.onScreenChange(this, _curScreen)

        }
    }

    fun setToScreen(whichScreen: Int) {
        val nwhichScreen = Math.max(0, Math.min(whichScreen, childCount - 1))
        _curScreen = nwhichScreen
        scrollTo(0, nwhichScreen * height)
    }

    override fun computeScroll() {
        if (_scroller!!.computeScrollOffset()) {
            scrollTo(_scroller!!.currX, _scroller!!.currY)
            postInvalidate()
        }
    }

    override fun onTouchEvent(event: MotionEvent?): Boolean {
        if (!_enableScroll) {
            return super.onTouchEvent(event)
        }
        if (_velocityTracker == null) {
            _velocityTracker = VelocityTracker.obtain()
        }
        _velocityTracker?.addMovement(event)
        val action = event!!.action
        val y = event.y

        when (action) {
            MotionEvent.ACTION_DOWN -> {
                if (!_scroller!!.isFinished) {
                    _scroller?.abortAnimation()
                }
                _lastMotionY = y
                return true
            }
            MotionEvent.ACTION_MOVE -> {
                val deltaY = (_lastMotionY - y).toInt()
                if (Math.abs(deltaY) > _baseDelta) {
                    touchState = TOUCH_STATE_SCROLLING
                    _lastMotionY = y
                    scrollBy(0, deltaY)
                }
                return true
            }
            MotionEvent.ACTION_UP -> {
                val velocityTracker = _velocityTracker
                velocityTracker?.computeCurrentVelocity(2000)
                val velocityY = velocityTracker!!.yVelocity.toInt()
                if (velocityY > SNAP_VELOCITY && _curScreen > 0) {
                    snapToScreen(_curScreen - 1)
                } else if (velocityY < -SNAP_VELOCITY && _curScreen < childCount - 1) {
                    snapToScreen(_curScreen + 1)
                } else {
                    snapToDestination()
                }
                if (_velocityTracker != null) {
                    _velocityTracker?.recycle()
                    _velocityTracker = null
                }
                touchState = TOUCH_STATE_REST
                return true
            }
            MotionEvent.ACTION_CANCEL -> {
                touchState = TOUCH_STATE_REST
                return true
            }
        }
        return false
    }

    override fun onInterceptTouchEvent(ev: MotionEvent?): Boolean {
        if (!_enableScroll) {
            return super.onInterceptTouchEvent(ev)
        }
        val action = ev!!.action
        if (action == MotionEvent.ACTION_MOVE && _touchState != TOUCH_STATE_REST) {
            return true
        }
        val y = ev.y
        when (action) {
            MotionEvent.ACTION_MOVE -> {
                val xDiff = Math.abs(_lastMotionY - y).toInt()
                if (xDiff > _touchSlop) {
                    touchState = TOUCH_STATE_SCROLLING
                }
            }
            MotionEvent.ACTION_DOWN -> {
                _lastMotionY = y
                touchState = if (_scroller!!.isFinished) TOUCH_STATE_REST else TOUCH_STATE_SCROLLING
            }
            MotionEvent.ACTION_CANCEL,
            MotionEvent.ACTION_UP -> touchState = TOUCH_STATE_REST
        }
        return _touchState != TOUCH_STATE_REST
    }

}