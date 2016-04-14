package com.hujiang.devart.component.arcmenu

import android.content.Context
import android.graphics.Rect
import android.util.AttributeSet
import android.view.View
import android.view.ViewGroup
import android.view.animation.*
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/14/16.
 */
class ArcLayout: ViewGroup {

    companion object {
        val DEFAULT_FROM_DEGREES = 270.0f
        val DEFAULT_TO_DEGREES = 360.0f
        private val MIN_RADIUS = 100

        private fun computeRadius(arcDegrees: Float, childCount: Int, childSize: Int, childPadding: Int, minRadius: Int): Int {
            if (childCount < 2) {
                return minRadius
            }
            val perDegrees = arcDegrees / (childCount - 1)
            val perHalfDegrees = perDegrees / 2
            val perSize = childSize + childPadding
            val radius = ((perSize / 2) / Math.sin(Math.toRadians(perHalfDegrees.toDouble()))).toInt()
            return Math.max(radius, minRadius)
        }

        private fun computeChildFrame(centerX: Int, centerY: Int, radius: Int, degrees: Float, size: Int): Rect {
            val childCenterX = centerX + radius * Math.cos(Math.toRadians(degrees.toDouble()))
            val childCenterY = centerY + radius * Math.sin(Math.toRadians(degrees.toDouble()))
            return Rect((childCenterX - size / 2).toInt(), (childCenterY - size / 2).toInt(), (childCenterX + size / 2).toInt(), (childCenterY + size / 2).toInt())
        }

        private fun computeStartOffset(childCount: Int, expanded: Boolean, index: Int, delayPercent: Float, duration: Long, interpolator: Interpolator?): Long {
            val delay = delayPercent * duration
            val viewDelay = (getTransformedIndex(expanded, childCount, index) * delay).toLong()
            val totalDelay = delay * childCount
            var normalizedDelay = viewDelay / totalDelay
            normalizedDelay = interpolator!!.getInterpolation(normalizedDelay)
            return (normalizedDelay * totalDelay).toLong()
        }

        private fun getTransformedIndex(expanded: Boolean, count: Int, index: Int): Int {
            if (expanded) {
                return count - 1 - index
            }
            return index
        }

        private fun createExpandAnimation(fromXDelta: Float, toXDelta: Float, fromYDelta: Float, toYDelta: Float, startOffset: Long, duration: Long, interpolator: Interpolator?): Animation? {
            val animation = RotateAndTranslateAnimation(0.0f, toXDelta, 0.0f, toYDelta, 0.0f, 720.0f)
            animation.startOffset = startOffset
            animation.duration = duration
            animation.interpolator = interpolator
            animation.fillAfter = true
            return animation
        }

        private fun createShrinkAnimation(fromXDelta: Float, toXDelta: Float, fromYDelta: Float, toYDelta: Float, startOffset: Long, duration: Long, interpolator: Interpolator?): Animation? {
            val animationSet = AnimationSet(false)
            animationSet.fillAfter = true
            val preDuration = duration / 2
            val rotateAnimation = RotateAnimation(0.0f, 360.0f, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f)
            rotateAnimation.startOffset = startOffset
            rotateAnimation.duration = preDuration
            rotateAnimation.interpolator = LinearInterpolator()
            rotateAnimation.fillAfter = true
            animationSet.addAnimation(rotateAnimation)
            val translateAnimation = RotateAndTranslateAnimation(0.0f, toXDelta, 0.0f, toYDelta, 360.0f, 720.0f)
            translateAnimation.startOffset = startOffset + preDuration
            translateAnimation.duration = duration - preDuration
            translateAnimation.interpolator = interpolator
            translateAnimation.fillAfter = true
            animationSet.addAnimation(translateAnimation)
            return animationSet
        }
    }

    private var _childSize = 0
    private var _childPadding = 5
    private var _layoutPadding = 10
    private var _fromDegrees = DEFAULT_FROM_DEGREES
    private var _toDegrees = DEFAULT_TO_DEGREES
    private var _radius = 0
    private var _expanded = false

    constructor(context: Context): super(context)

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        if (attrs != null) {
            val a = getContext().obtainStyledAttributes(attrs, R.styleable.ArcLayout, 0, 0)
            _fromDegrees = a.getFloat(R.styleable.ArcLayout_fromDegrees, DEFAULT_FROM_DEGREES)
            _toDegrees = a.getFloat(R.styleable.ArcLayout_toDegrees, DEFAULT_TO_DEGREES)
            _childSize = Math.max(a.getDimensionPixelSize(R.styleable.ArcLayout_childSize, 0), 0)
            a.recycle()
        }
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        _radius = computeRadius(Math.abs(_toDegrees - _fromDegrees), childCount, _childSize, _childPadding, MIN_RADIUS)
        val radius = _radius
        val size = radius * 2 + _childSize + _childPadding + _layoutPadding * 2
        setMeasuredDimension(size, size)
        val count = childCount
        for (i in 0..count - 1) {
            getChildAt(i).measure(MeasureSpec.makeMeasureSpec(_childSize, MeasureSpec.EXACTLY), MeasureSpec.makeMeasureSpec(_childSize, MeasureSpec.EXACTLY))
        }
    }

    override fun onLayout(changed: Boolean, l: Int, t: Int, r: Int, b: Int) {
        val centerX = width / 2
        val centerY = height / 2
        val radius = if (_expanded) _radius else 0
        val perDegrees = (_toDegrees - _fromDegrees) / (childCount - 1)
        var degrees = _fromDegrees
        for (i in 0..childCount - 1) {
            val frame = computeChildFrame(centerX, centerY, radius, degrees, _childSize)
            degrees += perDegrees
            getChildAt(i).layout(frame.left, frame.top, frame.right, frame.bottom)
        }
    }

    fun switchState(showAnimation: Boolean) {
        if (showAnimation) {
            for (i in 0..childCount - 1) {
                bindChildAnimation(getChildAt(i), i, 300)
            }
        }
        _expanded = !_expanded
        if (!showAnimation) {
            requestLayout()
        }
        invalidate()
    }

    private fun bindChildAnimation(child: View?, index: Int, duration: Long) {
        val expanded = _expanded
        val centerX = width / 2
        val centerY =height / 2;
        val radius = if (expanded) 0 else _radius
        val perDegrees = (_toDegrees - _fromDegrees) / (childCount - 1)
        val frame = computeChildFrame(centerX, centerY, radius, _fromDegrees + index * perDegrees, _childSize)
        val toXDelta = frame.left - child!!.left
        val toYDelta = frame.top - child.top
        val interpolator = if (_expanded) AccelerateInterpolator() else OvershootInterpolator(1.5f)
        val startOffset = computeStartOffset(childCount, _expanded, index, 0.1f, duration, interpolator)
        val animation = if (_expanded) createShrinkAnimation(0.0f, toXDelta.toFloat(), 0.0f, toYDelta.toFloat(), startOffset, duration, interpolator) else createExpandAnimation(0.0f, toXDelta.toFloat(), 0.0f, toYDelta.toFloat(), startOffset, duration, interpolator)
        val isLast = getTransformedIndex(expanded, childCount, index) == childCount - 1
        animation?.setAnimationListener(object: Animation.AnimationListener {
            override fun onAnimationStart(animation: Animation?) { }
            override fun onAnimationRepeat(animation: Animation?) { }
            override fun onAnimationEnd(animation: Animation?) {
                if (isLast) {
                    postDelayed({ onAllAnimationsEnd() }, 0)
                }
            }
        })
        child.animation = animation
    }

    private fun onAllAnimationsEnd() {
        for (i in 0..childCount - 1) {
            getChildAt(i).clearAnimation()
        }
        requestLayout()
    }

    fun setArc(fromDegrees: Float, toDegrees: Float) {
        if (_fromDegrees == fromDegrees && _toDegrees == toDegrees) {
            return
        }
        _fromDegrees = fromDegrees
        _toDegrees = toDegrees
        requestLayout()
    }

    fun isExpanded(): Boolean = _expanded

    fun setChildSize(size: Int) {
        if (_childSize == size || size < 0) {
            return
        }
        _childSize = size
        requestLayout()
    }

    fun getChildSize(): Int = _childSize

}