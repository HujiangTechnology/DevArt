package com.hujiang.devart.component.scroll

import android.content.Context
import android.util.AttributeSet
import android.view.*
import android.widget.Scroller
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/29/16.
 */
class HScrollLayout: ViewGroup {

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
    private var _lastMotionX = 0.0f
    private var _baseDelta = 5
    var baseDelta: Int
        get() = _baseDelta
        set(value) { _baseDelta = value }

    constructor(context: Context): this(context, null)

    constructor(context: Context, attrs: AttributeSet?): this(context, attrs, 0)

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        _scroller = Scroller(context)
        _curScreen = _defaultScreen
        _touchSlop = ViewConfiguration.get(context).scaledTouchSlop * 4
        touchState = TOUCH_STATE_REST
    }

    override fun onLayout(changed: Boolean, l: Int, t: Int, r: Int, b: Int) {
        var childLeft = 0
        for (i in 0..childCount - 1) {
            val childView = getChildAt(i)
            if (childView.visibility != View.GONE) {
                val childWidth = childView.measuredWidth
                childView.layout(childLeft, 0, childLeft + childWidth, childView.measuredHeight)
                childLeft += childWidth
            }
        }
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec)
        val width = MeasureSpec.getSize(widthMeasureSpec)
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
        scrollTo(_curScreen * width, 0)
    }

    fun snapToDestination() = snapToScreen((scrollX + width / 2) / width)

    fun snapToScreen(whichScreen: Int) {
        val nwhichScreen = Math.max(0, Math.min(whichScreen, childCount - 1))
        if (scrollX != (nwhichScreen * width)) {
            val delta = nwhichScreen * width - scrollX
            _scroller?.startScroll(scrollX, 0, delta, 0, 50) // Math.abs(delta) * 2)
            _curScreen = nwhichScreen
            invalidate()
            _screenChangeListener?.onScreenChange(this, _curScreen)
        }
    }

    fun setToScreen(whichScreen: Int) {
        val nwhichScreen = Math.max(0, Math.min(whichScreen, childCount - 1))
        _curScreen = nwhichScreen
        scrollTo(nwhichScreen * width, 0)
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
        val x = event.x
        when (action) {
            MotionEvent.ACTION_DOWN -> {
                if (!_scroller!!.isFinished) {
                    _scroller?.abortAnimation()
                }
                _lastMotionX = x
                return true
            }
            MotionEvent.ACTION_MOVE -> {
                val deltaX = (_lastMotionX - x).toInt()
                if (Math.abs(deltaX) > _baseDelta) {
                    touchState = TOUCH_STATE_SCROLLING
                    _lastMotionX = x
                    scrollBy(deltaX, 0)
                }
                return true
            }
            MotionEvent.ACTION_UP -> {
                val velocityTracker = _velocityTracker
                velocityTracker?.computeCurrentVelocity(2000)
                val velocityX = velocityTracker!!.xVelocity.toInt()
                if (velocityX > SNAP_VELOCITY && _curScreen > 0) {
                    snapToScreen(_curScreen - 1)
                } else if (velocityX < -SNAP_VELOCITY && _curScreen < childCount - 1) {
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
        if ((action == MotionEvent.ACTION_MOVE) && (_touchState != TOUCH_STATE_REST)) {
            return true
        }
        val x = ev.x
        when (action) {
            MotionEvent.ACTION_MOVE -> {
                val xDiff = Math.abs(_lastMotionX - x).toInt()
                if (xDiff > _touchSlop) {
                    touchState = TOUCH_STATE_SCROLLING
                }
            }
            MotionEvent.ACTION_DOWN -> {
                _lastMotionX = x
                touchState = if (_scroller!!.isFinished) TOUCH_STATE_REST else TOUCH_STATE_SCROLLING
            }

            MotionEvent.ACTION_CANCEL,
            MotionEvent.ACTION_UP -> touchState = TOUCH_STATE_REST
        }
        return _touchState != TOUCH_STATE_REST
    }

}