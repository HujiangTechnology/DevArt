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
class RayLayout: ViewGroup {

    companion object {

        private fun computeChildGap(width: Float, childCount: Int, childSize: Int, minGap: Int): Int {
            return Math.max((width / childCount - childSize).toInt(), minGap)
        }

        private fun computeChildFrame(expanded: Boolean, paddingLeft: Int, childIndex: Int, gap: Int, size: Int): Rect {
            val left = if (expanded) (paddingLeft + childIndex * (gap + size) + gap) else ((paddingLeft - size) / 2)
            return Rect(left, 0, left + size, size)
        }

        private fun computeStartOffset(childCount: Int, expanded: Boolean, index: Int, delayPercent: Float, duration: Long, interpolator: Interpolator?): Long {
            val delay = delayPercent * duration
            val viewDelay = (getTransformedIndex(expanded, childCount, index) * delay).toLong()
            val totalDelay = delay * childCount
            var normalizedDelay = viewDelay / totalDelay
            normalizedDelay = interpolator!!.getInterpolation(normalizedDelay)
            return (normalizedDelay * totalDelay).toLong()
        }

        private fun getTransformedIndex(expanded: Boolean, count: Int, index: Int): Int = count - 1 - index

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
    private var _childGap = 0
    private var _leftHolderWidth = 0
    private var _expanded = false

    constructor(context: Context): super(context)

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        if (attrs != null) {
            var a = getContext().obtainStyledAttributes(attrs, R.styleable.RayLayout, 0, 0)
            _childSize = Math.max(a.getDimensionPixelSize(R.styleable.RayLayout_childSize, 0), 0)
            _leftHolderWidth = Math.max(a.getDimensionPixelSize(R.styleable.RayLayout_leftHolderWidth, 0), 0)
            a.recycle()
        }
    }

    override fun getSuggestedMinimumHeight(): Int = _childSize

    override fun getSuggestedMinimumWidth(): Int = _leftHolderWidth + _childSize * childCount

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        super.onMeasure(widthMeasureSpec, MeasureSpec.makeMeasureSpec(suggestedMinimumHeight, MeasureSpec.EXACTLY))
        _childGap = computeChildGap((measuredWidth - _leftHolderWidth).toFloat(), childCount, _childSize, 0)
        for (i in 0..childCount - 1) {
            getChildAt(i).measure(MeasureSpec.makeMeasureSpec(_childSize, MeasureSpec.EXACTLY), MeasureSpec.makeMeasureSpec(_childSize, MeasureSpec.EXACTLY))
        }
    }

    override fun onLayout(changed: Boolean, l: Int, t: Int, r: Int, b: Int) {
        val paddingLeft = _leftHolderWidth
        for (i in 0..childCount - 1) {
            val frame = computeChildFrame(_expanded, paddingLeft, i, _childGap, _childSize)
            getChildAt(i).layout(frame.left, frame.top, frame.right, frame.bottom)
        }
    }

    private fun bindChildAnimation(child: View?, index: Int, duration: Long) {
        val expanded = _expanded
        val frame = computeChildFrame(!expanded, _leftHolderWidth, index, _childGap, _childSize)
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

    private fun onAllAnimationsEnd() {
        for (i in 0..childCount - 1) {
            getChildAt(i).clearAnimation()
        }
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