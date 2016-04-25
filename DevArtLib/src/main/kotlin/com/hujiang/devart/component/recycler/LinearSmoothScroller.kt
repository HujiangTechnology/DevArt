package com.hujiang.devart.component.recycler

import android.content.Context
import android.graphics.PointF
import android.util.DisplayMetrics
import android.view.View
import android.view.animation.DecelerateInterpolator
import android.view.animation.LinearInterpolator

/**
 * Created by rarnu on 4/24/16.
 */
abstract class LinearSmoothScroller: RecyclerView.SmoothScroller {

    companion object {
        val MILLISECONDS_PER_INCH = 25.0f
        val TARGET_SEEK_SCROLL_DISTANCE_PX = 10000
        val SNAP_TO_START = -1
        val SNAP_TO_END = 1
        val SNAP_TO_ANY = 0
        var TARGET_SEEK_EXTRA_SCROLL_RATIO = 1.2f
    }

    val _linearInterpolator = LinearInterpolator()
    val _decelerateInterpolator = DecelerateInterpolator()
    var _targetVector: PointF? = null
    var MILLISECONDS_PER_PX = 0.0f
    var _interimTargetDx = 0
    var _interimTargetDy = 0

    constructor(context: Context) {
        MILLISECONDS_PER_PX = calculateSpeedPerPixel(context.getResources().getDisplayMetrics())
    }

    override fun onStart() { }

    override fun onTargetFound(targetView: View?, state: RecyclerView.State?, action: Action?) {
        val dx = calculateDxToMakeVisible(targetView, getHorizontalSnapPreference())
        val dy = calculateDyToMakeVisible(targetView, getVerticalSnapPreference())
        val distance = Math.sqrt((dx * dx + dy * dy).toDouble()).toInt()
        val time = calculateTimeForDeceleration(distance)
        if (time > 0) {
            action?.update(-dx, -dy, time, _decelerateInterpolator)
        }
    }

    fun calculateDyToMakeVisible(view: View?, snapPreference: Int): Int {
        val layoutManager = getLayoutManager()
        if (!layoutManager!!.canScrollVertically()) {
            return 0
        }
        val params = view?.getLayoutParams() as RecyclerView.LayoutParams
        val top = layoutManager.getDecoratedTop(view) - params.topMargin
        val bottom = layoutManager.getDecoratedBottom(view) + params.bottomMargin
        val start = layoutManager.getPaddingTop()
        val end = layoutManager.getHeight() - layoutManager.getPaddingBottom()
        return calculateDtToFit(top, bottom, start, end, snapPreference)
    }

    override fun onSeekTargetStep(dx: Int, dy: Int, state: RecyclerView.State?, action: Action?) {
        if (getChildCount() == 0) {
            stop()
            return
        }
        _interimTargetDx = clampApplyScroll(_interimTargetDx, dx)
        _interimTargetDy = clampApplyScroll(_interimTargetDy, dy)
        if (_interimTargetDx == 0 && _interimTargetDy == 0) {
            updateActionForInterimTarget(action)
        }
    }

    fun clampApplyScroll(tmpDt: Int, dt: Int): Int {
        var ntmpDt = tmpDt
        val before = ntmpDt
        ntmpDt -= dt
        if (before * ntmpDt <= 0) {
            return 0
        }
        return ntmpDt
    }

    override fun onStop() {
        _interimTargetDx =0
        _interimTargetDy = 0
        _targetVector = null
    }

    protected fun calculateSpeedPerPixel(displayMetrics: DisplayMetrics?): Float = MILLISECONDS_PER_INCH / displayMetrics!!.densityDpi

    protected fun calculateTimeForDeceleration(dx: Int): Int = Math.ceil(calculateTimeForScrolling(dx) / 0.3356).toInt()

    protected fun calculateTimeForScrolling(dx: Int): Int = Math.ceil((Math.abs(dx) * MILLISECONDS_PER_PX).toDouble()).toInt()

    protected fun getHorizontalSnapPreference(): Int = if (_targetVector == null || _targetVector!!.x == 0.0f) SNAP_TO_ANY else if (_targetVector!!.x > 0) SNAP_TO_END else SNAP_TO_START

    protected fun getVerticalSnapPreference(): Int = if (_targetVector == null || _targetVector!!.y == 0.0f) SNAP_TO_ANY else if (_targetVector!!.y > 0) SNAP_TO_END else SNAP_TO_START

    protected fun updateActionForInterimTarget(action: Action?) {
        val scrollVector = computeScrollVectorForPosition(getTargetPosition())
        if (scrollVector == null || (scrollVector.x == 0.0f && scrollVector.y == 0.0f)) {
            val target = getTargetPosition()
            action?.jumpTo(target)
            stop()
            return
        }
        normalize(scrollVector)
        _targetVector = scrollVector
        _interimTargetDx = (TARGET_SEEK_SCROLL_DISTANCE_PX * scrollVector.x).toInt()
        _interimTargetDy = (TARGET_SEEK_SCROLL_DISTANCE_PX * scrollVector.y).toInt()
        val time = calculateTimeForScrolling(TARGET_SEEK_SCROLL_DISTANCE_PX)
        action?.update((_interimTargetDx * TARGET_SEEK_EXTRA_SCROLL_RATIO).toInt(), (_interimTargetDy * TARGET_SEEK_EXTRA_SCROLL_RATIO).toInt(), (time * TARGET_SEEK_EXTRA_SCROLL_RATIO).toInt(), _linearInterpolator)
    }

    abstract fun computeScrollVectorForPosition(targetPosition: Int): PointF?

    fun calculateDxToMakeVisible(view: View?, snapPreference: Int): Int {
        val layoutManager = getLayoutManager()
        if (!layoutManager!!.canScrollHorizontally()) {
            return 0
        }
        val params = view?.getLayoutParams() as RecyclerView.LayoutParams
        val left = layoutManager.getDecoratedLeft(view) - params.leftMargin
        val right = layoutManager.getDecoratedRight(view) + params.rightMargin
        val start = layoutManager.getPaddingLeft()
        val end = layoutManager.getWidth() - layoutManager.getPaddingRight()
        return calculateDtToFit(left, right, start, end, snapPreference)
    }

    fun calculateDtToFit(viewStart: Int, viewEnd: Int, boxStart: Int, boxEnd: Int, snapPreference: Int): Int {
        when (snapPreference) {
            SNAP_TO_START -> return boxStart - viewStart
            SNAP_TO_END -> return boxEnd - viewEnd
            SNAP_TO_ANY -> {
                val dtStart = boxStart - viewStart
                if (dtStart > 0) {
                    return dtStart
                }
                val dtEnd = boxEnd - viewEnd
                if (dtEnd < 0) {
                    return dtEnd
                }
            }
            else -> throw IllegalArgumentException("snap preference should be one of the constants defined in SmoothScroller, starting with SNAP_")
        }
        return 0
    }
}