package com.hujiang.devart.component.swipe

import android.content.Context
import android.graphics.Rect
import android.os.Build
import android.support.v4.widget.ViewDragHelper
import android.util.AttributeSet
import android.view.GestureDetector
import android.view.MotionEvent
import android.view.View
import android.view.ViewGroup
import android.widget.AdapterView
import android.widget.BaseAdapter
import android.widget.FrameLayout
import android.widget.ListAdapter
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/31/16.
 */
class SwipeLayout: FrameLayout {

    enum class Mode {Single, Multiple }

    enum class DragEdge { Left, Right, Top, Bottom }

    enum class ShowMode { LayDown, PullOut }

    enum class Status { Middle, Open, Close }

    private var _dragHelper: ViewDragHelper? = null
    private var _dragDistance = 0
    private var _dragEdge = DragEdge.Left
    private var _showMode = ShowMode.LayDown
    private var _horizontalSwipeOffset = 0.0f
    private var _verticalSwipeOffset = 0.0f
    private var _swipeListeners = arrayListOf<SwipeListener>()
    private var _swipeDeniers = arrayListOf<SwipeDenier>()
    private var _revealListeners = hashMapOf<View, MutableList<OnRevealListener>>()
    private var _showEntirely = hashMapOf<View, Boolean>()
    private var _doubleClickListener: DoubleClickListener? = null
    private var _swipeEnabled = true
    private var _eventCounter = 0
    private var _onLayoutListeners: MutableList<OnLayout>? = null
    private var _touchConsumedByChild = false
    private var _gestureDetector = GestureDetector(context, SwipeDetector())
    private var _sX = -1.0f
    private var _sY = -1.0f

    constructor(context: Context?): this(context, null)

    constructor(context: Context?, attrs: AttributeSet?): this(context, attrs, 0)

    constructor(context: Context?, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        _dragHelper = ViewDragHelper.create(this, _dragHelperCallback)
        val a = context!!.obtainStyledAttributes(attrs, R.styleable.SwipeLayout)
        var ordinal = a.getInt(R.styleable.SwipeLayout_drag_edge, DragEdge.Right.ordinal)
        _horizontalSwipeOffset = a.getDimension(R.styleable.SwipeLayout_horizontalSwipeOffset, 0.0f)
        _verticalSwipeOffset = a.getDimension(R.styleable.SwipeLayout_verticalSwipeOffset, 0.0f)
        _dragEdge = DragEdge.values()[ordinal]
        ordinal = a.getInt(R.styleable.SwipeLayout_show_mode, ShowMode.PullOut.ordinal)
        _showMode = ShowMode.values()[ordinal]
    }

    private val _dragHelperCallback = object : ViewDragHelper.Callback() {

        override fun clampViewPositionHorizontal(child: View?, left: Int, dx: Int): Int {
            if (child == getSurfaceView()) {
                when (_dragEdge) {
                    DragEdge.Top,
                    DragEdge.Bottom -> return paddingLeft
                    DragEdge.Left -> {
                        if (left < paddingLeft) {
                            return paddingLeft
                        }
                        if (left > paddingLeft + _dragDistance) {
                            return paddingLeft + _dragDistance
                        }
                    }
                    DragEdge.Right -> {
                        if (left > paddingLeft) {
                            return paddingLeft
                        }
                        if (left < paddingLeft - _dragDistance) {
                            return paddingLeft - _dragDistance
                        }
                    }
                }
            } else if (child == getBottomView()) {
                when (_dragEdge) {
                    DragEdge.Top,
                    DragEdge.Bottom -> return paddingLeft
                    DragEdge.Left -> {
                        if (_showMode == ShowMode.PullOut) {
                            if (left > paddingLeft) {
                                return paddingLeft
                            }
                        }
                    }
                    DragEdge.Right -> {
                        if (_showMode == ShowMode.PullOut) {
                            if (left < measuredWidth - _dragDistance) {
                                return measuredWidth - _dragDistance
                            }
                        }
                    }
                }
            }
            return left
        }

        override fun clampViewPositionVertical(child: View?, top: Int, dy: Int): Int {
            if (child == getSurfaceView()) {
                when (_dragEdge) {
                    DragEdge.Left,
                    DragEdge.Right -> return paddingTop
                    DragEdge.Top -> {
                        if (top < paddingTop) {
                            return paddingTop
                        }
                        if (top > paddingTop + _dragDistance) {
                            return paddingTop + _dragDistance
                        }
                    }
                    DragEdge.Bottom -> {
                        if (top < paddingTop - _dragDistance) {
                            return paddingTop - _dragDistance
                        }
                        if (top > paddingTop) {
                            return paddingTop
                        }
                    }
                }
            } else {
                when (_dragEdge) {
                    DragEdge.Left,
                    DragEdge.Right -> return paddingTop
                    DragEdge.Top -> {
                        if (_showMode == ShowMode.PullOut) {
                            if (top > paddingTop) {
                                return paddingTop
                            }
                        } else {
                            if (getSurfaceView().top + dy < paddingTop) {
                                return paddingTop
                            }
                            if (getSurfaceView().top + dy > paddingTop + _dragDistance) {
                                return paddingTop + _dragDistance
                            }
                        }
                    }
                    DragEdge.Bottom -> {
                        if (_showMode == ShowMode.PullOut) {
                            if (top < measuredHeight - _dragDistance) {
                                return measuredHeight - _dragDistance
                            }
                        } else {
                            if (getSurfaceView().top + dy >= paddingTop) {
                                return paddingTop
                            }
                            if (getSurfaceView().top + dy <= paddingTop - _dragDistance) {
                                return paddingTop - _dragDistance
                            }
                        }
                    }
                }
            }
            return top
        }

        override fun tryCaptureView(child: View?, pointerId: Int): Boolean {
            return child == getSurfaceView() || child == getBottomView()
        }

        override fun getViewHorizontalDragRange(child: View?): Int {
            return _dragDistance
        }

        override fun getViewVerticalDragRange(child: View?): Int {
            return _dragDistance
        }

        override fun onViewReleased(releasedChild: View?, xvel: Float, yvel: Float) {

            super.onViewReleased(releasedChild, xvel, yvel)
            for (l in _swipeListeners)
            l.onHandRelease(this@SwipeLayout, xvel, yvel)
            if (releasedChild == getSurfaceView()) {
                processSurfaceRelease(xvel, yvel)
            } else if (releasedChild == getBottomView()) {
                if (getShowMode() == ShowMode.PullOut) {
                    processBottomPullOutRelease(xvel, yvel)
                } else if (getShowMode() == ShowMode.LayDown) {
                    processBottomLayDownMode(xvel, yvel)
                }
            }
            invalidate()
        }

        override fun onViewPositionChanged(changedView: View?, left: Int, top: Int, dx: Int, dy: Int) {
            val evLeft = getSurfaceView().left
            val evRight = getSurfaceView().right
            val evTop = getSurfaceView().top
            val evBottom = getSurfaceView().bottom
            if (changedView == getSurfaceView()) {
                if (_showMode == ShowMode.PullOut) {
                    if (_dragEdge == DragEdge.Left || _dragEdge == DragEdge.Right) {
                        getBottomView().offsetLeftAndRight(dx)
                    } else {
                        getBottomView().offsetTopAndBottom(dy)
                    }
                }
            } else if (changedView == getBottomView()) {
                if (_showMode == ShowMode.PullOut) {
                    getSurfaceView().offsetLeftAndRight(dx)
                    getSurfaceView().offsetTopAndBottom(dy)
                } else {
                    val rect = computeBottomLayDown(_dragEdge)
                    getBottomView().layout(rect.left, rect.top, rect.right, rect.bottom)
                    var newLeft = getSurfaceView().left + dx
                    var newTop = getSurfaceView().top + dy
                    if (_dragEdge == DragEdge.Left && newLeft < paddingLeft) {
                        newLeft = paddingLeft
                    } else if (_dragEdge == DragEdge.Right && newLeft > paddingLeft) {
                        newLeft = paddingLeft
                    } else if (_dragEdge == DragEdge.Top && newTop < paddingTop) {
                        newTop = paddingTop
                    } else if (_dragEdge == DragEdge.Bottom && newTop > paddingTop) {
                        newTop = paddingTop
                    }
                    getSurfaceView().layout(newLeft, newTop, newLeft + measuredWidth, newTop + measuredHeight)
                }
            }
            dispatchRevealEvent(evLeft, evTop, evRight, evBottom)
            dispatchSwipeEvent(evLeft, evTop, dx, dy)
            invalidate()
        }
    }

    fun getSurfaceView(): ViewGroup = getChildAt(1) as ViewGroup

    fun getBottomView(): ViewGroup = getChildAt(0) as ViewGroup

    private fun processSurfaceRelease(xvel: Float, yvel: Float) {
        if (xvel == 0.0f && getOpenStatus() == Status.Middle) {
            close()
        }
        if (_dragEdge == DragEdge.Left || _dragEdge == DragEdge.Right) {
            if (xvel > 0) {
                if (_dragEdge == DragEdge.Left) { open() } else { close() }
            }
            if (xvel < 0) {
                if (_dragEdge == DragEdge.Left) { close() } else { open() }
            }
        } else {
            if (yvel > 0) {
                if (_dragEdge == DragEdge.Top) { open() } else { close() }
            }
            if (yvel < 0) {
                if (_dragEdge == DragEdge.Top) { close() } else { open() }
            }
        }
    }

    private fun processBottomPullOutRelease(xvel: Float, yvel: Float) {
        if (xvel == 0.0f && getOpenStatus() == Status.Middle) {
            close()
        }
        if (_dragEdge == DragEdge.Left || _dragEdge == DragEdge.Right) {
            if (xvel > 0) {
                if (_dragEdge == DragEdge.Left) { open() } else { close() }
            }
            if (xvel < 0) {
                if (_dragEdge == DragEdge.Left) { close() } else { open() }
            }
        } else {
            if (yvel > 0) {
                if (_dragEdge == DragEdge.Top) { open() } else { close() }
            }

            if (yvel < 0) {
                if (_dragEdge == DragEdge.Top) { close() } else { open() }
            }
        }
    }

    private fun processBottomLayDownMode(xvel: Float, yvel: Float) {
        if (xvel == 0.0f && getOpenStatus() == Status.Middle) {
            close()
        }
        var l = paddingLeft
        var t = paddingTop
        if (xvel < 0 && _dragEdge == DragEdge.Right) { l -= _dragDistance }
        if (xvel > 0 && _dragEdge == DragEdge.Left)  { l += _dragDistance }
        if (yvel > 0 && _dragEdge == DragEdge.Top) { t += _dragDistance }
        if (yvel < 0 && _dragEdge == DragEdge.Bottom) { t -= _dragDistance }
        _dragHelper?.smoothSlideViewTo(getSurfaceView(), l, t)
        invalidate()
    }

    fun setShowMode(mode: ShowMode) {
        _showMode = mode
        requestLayout()
    }

    fun getDragEdge(): DragEdge = _dragEdge

    fun getDragDistance(): Int = _dragDistance

    fun getShowMode(): ShowMode = _showMode

    private fun computeBottomLayDown(dragEdge: DragEdge): Rect {
        var bl = paddingLeft
        var bt = paddingTop
        var br: Int
        var bb: Int
        if (dragEdge == DragEdge.Right) {
            bl = measuredWidth - _dragDistance
        } else if (dragEdge == DragEdge.Bottom) {
            bt = measuredHeight - _dragDistance
        }
        if (dragEdge == DragEdge.Left || dragEdge == DragEdge.Right) {
            br = bl + _dragDistance
            bb = bt + measuredHeight
        } else {
            br = bl + measuredWidth
            bb = bt + _dragDistance
        }
        return Rect(bl, bt, br, bb)
    }

    protected fun dispatchRevealEvent(surfaceLeft: Int, surfaceTop: Int, surfaceRight: Int, surfaceBottom: Int) {
        if (_revealListeners.isEmpty()) {
            return
        }
        for (entry in _revealListeners.entries) {
            val child = entry.key
            val rect = getRelativePosition(child)
            if (isViewShowing(child, rect, _dragEdge, surfaceLeft, surfaceTop, surfaceRight, surfaceBottom)) {
                _showEntirely.put(child, false)
                var distance = 0
                var fraction = 0.0f
                if (getShowMode() == ShowMode.LayDown) {
                    when (_dragEdge) {
                        DragEdge.Left -> {
                            distance = rect.left - surfaceLeft
                            fraction = distance * 1.0f / child.width
                        }
                        DragEdge.Right -> {
                            distance = rect.right - surfaceRight
                            fraction = distance * 1.0f / child.width
                        }
                        DragEdge.Top -> {
                            distance = rect.top - surfaceTop
                            fraction = distance * 1.0f / child.height
                        }
                        DragEdge.Bottom -> {
                            distance = rect.bottom - surfaceBottom
                            fraction = distance * 1.0f / child.height
                        }
                    }
                } else if (getShowMode() == ShowMode.PullOut) {
                    when (_dragEdge) {
                        DragEdge.Left -> {
                            distance = rect.right - paddingLeft
                            fraction = distance * 1.0f / child.width
                        }
                        DragEdge.Right -> {
                            distance = rect.left - width
                            fraction = distance * 1.0f / child.width
                        }
                        DragEdge.Top -> {
                            distance = rect.bottom - paddingTop
                            fraction = distance * 1.0f / child.height
                        }
                        DragEdge.Bottom -> {
                            distance = rect.top - height
                            fraction = distance * 1.0f / child.height
                        }
                    }
                }
                for (l in entry.value) {
                    l.onReveal(child, _dragEdge, Math.abs(fraction), distance)
                    if (Math.abs(fraction).toInt() == 1) {
                        _showEntirely.put(child, true)
                    }
                }
            }
            if (isViewTotallyFirstShowed(child, rect, _dragEdge, surfaceLeft, surfaceTop, surfaceRight, surfaceBottom)) {
                _showEntirely.put(child, true)
                for (l in entry.value) {
                    if (_dragEdge == DragEdge.Left || _dragEdge == DragEdge.Right) {
                        l.onReveal(child, _dragEdge, 1.0f, child.width)
                    } else {
                        l.onReveal(child, _dragEdge, 1.0f, child.height)
                    }
                }
            }

        }
    }

    protected fun dispatchSwipeEvent(surfaceLeft: Int, surfaceTop: Int, dx: Int, dy: Int) {
        val edge = getDragEdge()
        var open = true
        if (edge == DragEdge.Left) {
            if (dx < 0) { open = false }
        } else if (edge == DragEdge.Right) {
            if (dx > 0) { open = false }
        } else if (edge == DragEdge.Top) {
            if (dy < 0) { open = false }
        } else if (edge == DragEdge.Bottom) {
            if (dy > 0) { open = false }
        }
        dispatchSwipeEvent(surfaceLeft, surfaceTop, open)
    }

    protected fun dispatchSwipeEvent(surfaceLeft: Int, surfaceTop: Int, open: Boolean) {
        safeBottomView()
        val status = getOpenStatus()
        if (!_swipeListeners.isEmpty()) {
            _eventCounter++
            for (l in _swipeListeners) {
                if (_eventCounter == 1) {
                    if (open) {
                        l.onStartOpen(this)
                    } else {
                        l.onStartClose(this)
                    }
                }
                l.onUpdate(this@SwipeLayout, surfaceLeft - paddingLeft, surfaceTop - paddingTop)
            }
            if (status == Status.Close) {
                for (l in _swipeListeners) {
                    l.onClose(this@SwipeLayout)
                }
                _eventCounter = 0
            }
            if (status == Status.Open) {
                getBottomView().isEnabled = true
                for (l in _swipeListeners) {
                    l.onOpen(this@SwipeLayout)
                }
                _eventCounter = 0
            }
        }
    }

    fun getOpenStatus(): Status {
        val surfaceLeft = getSurfaceView().left
        val surfaceTop = getSurfaceView().top
        if (surfaceLeft == paddingLeft && surfaceTop == paddingTop) {
            return Status.Close
        }
        if (surfaceLeft == (paddingLeft - _dragDistance) || surfaceLeft == (paddingLeft + _dragDistance) || surfaceTop == (paddingTop - _dragDistance) || surfaceTop == (paddingTop + _dragDistance)) {
            return Status.Open
        }
        return Status.Middle
    }

    fun close() {
        close(true, true)
    }

    fun close(smooth: Boolean) {
        close(smooth, true)
    }

    fun close(smooth: Boolean, notify: Boolean) {
        val surface = getSurfaceView()
        if (smooth) {
            _dragHelper?.smoothSlideViewTo(surface, paddingLeft, paddingTop)
        } else {
            val rect = computeSurfaceLayoutArea(false)
            val dx = rect.left - surface.left
            val dy = rect.top - surface.top
            surface.layout(rect.left, rect.top, rect.right, rect.bottom)
            if (notify) {
                dispatchRevealEvent(rect.left, rect.top, rect.right, rect.bottom)
                dispatchSwipeEvent(rect.left, rect.top, dx, dy)
            } else {
                safeBottomView()
            }
        }
        invalidate()
    }

    fun open() {
        open(true, true)
    }

    fun open(smooth: Boolean) {
        open(smooth, true)
    }

    fun open(smooth: Boolean, notify: Boolean) {
        val surface = getSurfaceView()
        val bottom = getBottomView()
        val rect = computeSurfaceLayoutArea(true)
        if (smooth) {
            _dragHelper?.smoothSlideViewTo(surface, rect.left, rect.top)
        } else {
            val dx = rect.left - surface.left
            val dy = rect.top - surface.top
            surface.layout(rect.left, rect.top, rect.right, rect.bottom)
            if (getShowMode() == ShowMode.PullOut) {
                val bRect = computeBottomLayoutAreaViaSurface(ShowMode.PullOut, rect)
                bottom.layout(bRect.left, bRect.top, bRect.right, bRect.bottom)
            }
            if (notify) {
                dispatchRevealEvent(rect.left, rect.top, rect.right, rect.bottom)
                dispatchSwipeEvent(rect.left, rect.top, dx, dy)
            } else {
                safeBottomView()
            }
        }
        invalidate()
    }

    protected fun getRelativePosition(child: View?): Rect {
        var t = child!!
        val r = Rect(t.left, t.top, 0, 0)
        while (t.parent != null && t != rootView) {
            t = t.parent as View
            if (t == this) {
                break
            }
            r.left += t.left
            r.top += t.top
        }
        r.right = r.left + child.measuredWidth
        r.bottom = r.top + child.measuredHeight
        return r
    }

    protected fun isViewShowing(@Suppress("UNUSED_PARAMETER") child: View?, relativePosition: Rect, availableEdge: DragEdge, surfaceLeft: Int, surfaceTop: Int, surfaceRight: Int, surfaceBottom: Int): Boolean {
        val childLeft = relativePosition.left
        val childRight = relativePosition.right
        val childTop = relativePosition.top
        val childBottom = relativePosition.bottom
        if (getShowMode() == ShowMode.LayDown) {
            when (availableEdge) {
                DragEdge.Right -> if (surfaceRight > childLeft && surfaceRight <= childRight) { return true }
                DragEdge.Left -> if (surfaceLeft < childRight && surfaceLeft >= childLeft) { return true }
                DragEdge.Top -> if (surfaceTop >= childTop && surfaceTop < childBottom) { return true }
                DragEdge.Bottom -> if (surfaceBottom > childTop && surfaceBottom <= childBottom) { return true }
            }
        } else if (getShowMode() == ShowMode.PullOut) {
            when (availableEdge) {
                DragEdge.Right -> if (childLeft <= width && childRight > width) { return true }
                DragEdge.Left -> if (childRight >= paddingLeft && childLeft < paddingLeft) { return true }
                DragEdge.Top -> if (childTop < paddingTop && childBottom >= paddingTop) { return true }
                DragEdge.Bottom -> if (childTop < height && childTop >= paddingTop) { return true }
            }
        }
        return false
    }

    protected fun isViewTotallyFirstShowed(child: View?, relativePosition: Rect, edge: DragEdge, surfaceLeft: Int, surfaceTop: Int, surfaceRight: Int, surfaceBottom: Int): Boolean {
        if (_showEntirely[child]!!) {
            return false
        }
        val childLeft = relativePosition.left
        val childRight = relativePosition.right
        val childTop = relativePosition.top
        val childBottom = relativePosition.bottom
        var r = false
        if (getShowMode() == ShowMode.LayDown) {
            if ((edge == DragEdge.Right && surfaceRight <= childLeft) || (edge == DragEdge.Left && surfaceLeft >= childRight) || (edge == DragEdge.Top && surfaceTop >= childBottom) || (edge == DragEdge.Bottom && surfaceBottom <= childTop)) {
                r = true
            }
        } else if (getShowMode() == ShowMode.PullOut) {
            if ((edge == DragEdge.Right && childRight <= width) || (edge == DragEdge.Left && childLeft >= paddingLeft) || (edge == DragEdge.Top && childTop >= paddingTop) || (edge == DragEdge.Bottom && childBottom <= height)) {
                r = true
            }
        }
        return r
    }

    private fun safeBottomView() {
        val status = getOpenStatus()
        val bottom = getBottomView()

        if (status == Status.Close) {
            if (bottom.visibility != INVISIBLE)
                bottom.visibility = INVISIBLE
        } else {
            if (bottom.visibility != VISIBLE)
                bottom.visibility = VISIBLE
        }
    }

    private fun computeSurfaceLayoutArea(open: Boolean): Rect {
        var l = paddingLeft
        var t = paddingTop
        if (open) {
            if (_dragEdge == DragEdge.Left)  {
                l = paddingLeft + _dragDistance
            } else if (_dragEdge == DragEdge.Right) {
                l = paddingLeft - _dragDistance
            } else if (_dragEdge == DragEdge.Top) {
                t = paddingTop + _dragDistance
            } else {
                t = paddingTop - _dragDistance
            }
        }
        return Rect(l, t, l + measuredWidth, t + measuredHeight)
    }

    private fun computeBottomLayoutAreaViaSurface(mode: ShowMode, surfaceArea: Rect): Rect {
        val rect = surfaceArea
        var bl = rect.left
        var bt = rect.top
        var br = rect.right
        var bb = rect.bottom
        if (mode == ShowMode.PullOut) {
            if (_dragEdge == DragEdge.Left) {
                bl = rect.left - _dragDistance
            } else if (_dragEdge == DragEdge.Right) {
                bl = rect.right
            } else if (_dragEdge == DragEdge.Top) {
                bt = rect.top - _dragDistance
            } else {
                bt = rect.bottom
            }
            if (_dragEdge == DragEdge.Left || _dragEdge == DragEdge.Right) {
                bb = rect.bottom
                br = bl + getBottomView().measuredWidth
            } else {
                bb = bt + getBottomView().measuredHeight
                br = rect.right
            }
        } else if (mode == ShowMode.LayDown) {
            if (_dragEdge == DragEdge.Left) {
                br = bl + _dragDistance
            } else if (_dragEdge == DragEdge.Right) {
                bl = br - _dragDistance
            } else if (_dragEdge == DragEdge.Top) {
                bb = bt + _dragDistance
            } else {
                bt = bb - _dragDistance
            }
        }
        return Rect(bl, bt, br, bb)
    }

    override fun computeScroll() {
        super.computeScroll()
        if (_dragHelper!!.continueSettling(true)) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                postInvalidateOnAnimation()
            } else {
                postInvalidate()
            }
        }
    }

    override fun onLayout(changed: Boolean, left: Int, top: Int, right: Int, bottom: Int) {
        if (childCount != 2) {
            throw IllegalStateException("You need 2  views in SwipeLayout")
        }
        if ((getChildAt(0) !is ViewGroup) || (getChildAt(1) !is ViewGroup)) {
            throw IllegalArgumentException("The 2 children in SwipeLayout must be an instance of ViewGroup")
        }
        if (_showMode == ShowMode.PullOut) {
            layoutPullOut()
        } else if (_showMode == ShowMode.LayDown) {
            layoutLayDown()
        }
        safeBottomView()
        if (_onLayoutListeners != null)
            for (i in 0.._onLayoutListeners!!.size -1) {
            _onLayoutListeners!![i].onLayout(this)
        }
    }

    fun layoutPullOut() {
        var rect = computeSurfaceLayoutArea(false)
        getSurfaceView().layout(rect.left, rect.top, rect.right, rect.bottom)
        rect = computeBottomLayoutAreaViaSurface(ShowMode.PullOut, rect)
        getBottomView().layout(rect.left, rect.top, rect.right, rect.bottom)
        bringChildToFront(getSurfaceView())
    }

    fun layoutLayDown() {
        var rect = computeSurfaceLayoutArea(false)
        getSurfaceView().layout(rect.left, rect.top, rect.right, rect.bottom)
        rect = computeBottomLayoutAreaViaSurface(ShowMode.LayDown, rect)
        getBottomView().layout(rect.left, rect.top, rect.right, rect.bottom)
        bringChildToFront(getSurfaceView())
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec)
        if (_dragEdge == DragEdge.Left || _dragEdge == DragEdge.Right) {
            _dragDistance = getBottomView().measuredWidth - dp2px(_horizontalSwipeOffset)
        } else {
            _dragDistance = getBottomView().measuredHeight - dp2px(_verticalSwipeOffset)
        }
    }

    private fun dp2px(dp: Float): Int = (dp * context.resources.displayMetrics.density + 0.5f).toInt()

    override fun onInterceptTouchEvent(ev: MotionEvent?): Boolean {
        if (!isEnabled || !isEnabledInAdapterView()) {
            return true
        }
        if (!isSwipeEnabled()) {
            return false
        }
        for (denier in _swipeDeniers) {
            if (denier.shouldDenySwipe(ev)) {
                return false
            }
        }
        val action = ev!!.actionMasked
        when (action) {
            MotionEvent.ACTION_DOWN -> {
                val status = getOpenStatus()
                if (status == Status.Close) {
                    _touchConsumedByChild = childNeedHandleTouchEvent(getSurfaceView(), ev) != null
                } else if (status == Status.Open) {
                    _touchConsumedByChild = childNeedHandleTouchEvent(getBottomView(), ev) != null
                }
            }
            MotionEvent.ACTION_UP,
            MotionEvent.ACTION_CANCEL -> _touchConsumedByChild = false
        }
        if (_touchConsumedByChild) {
            return false
        }
        return _dragHelper!!.shouldInterceptTouchEvent(ev)
    }

    private fun isEnabledInAdapterView(): Boolean {
        val adapterView = getAdapterView()
        var enable = true
        if (adapterView != null) {
            val adapter = adapterView.adapter
            if (adapter != null) {
                val p = adapterView.getPositionForView(this@SwipeLayout)
                if (adapter is BaseAdapter) {
                    enable = adapter.isEnabled(p)
                } else if (adapter is ListAdapter) {
                    enable = adapter.isEnabled(p)
                }
            }
        }
        return enable
    }

    fun isSwipeEnabled(): Boolean = _swipeEnabled

    private fun getAdapterView(): AdapterView<*>? {
        var t = parent
        while (t != null) {
            if (t is AdapterView<*>) {
                return t
            }
            t = t.parent
        }
        return null
    }

    private fun childNeedHandleTouchEvent(v: ViewGroup?, event: MotionEvent?): View? {
        if (v == null) {
            return null
        }
        if (v.onTouchEvent(event)) {
            return v
        }
        for (i in childCount - 1 downTo 0) {
            val child = v.getChildAt(i)
            if (child is ViewGroup) {
                val grandChild = childNeedHandleTouchEvent(child, event)
                if (grandChild != null) {
                    return grandChild
                }
            } else {
                if (childNeedHandleTouchEvent(child, event)) {
                    return v.getChildAt(i)
                }
            }
        }
        return null
    }

    private fun childNeedHandleTouchEvent(v: View?, event: MotionEvent?): Boolean {
        if (v == null) {
            return false
        }
        val loc = IntArray(2)
        v.getLocationOnScreen(loc)
        val left = loc[0]
        val top = loc[1]
        if (event!!.rawX > left && event.rawX < left + v.width && event.rawY > top && event.rawY < top + v.height) {
            if (v.hasOnClickListeners()) {
                v.callOnClick()
                return true
            } else {
                return v.onTouchEvent(event)
            }
        }
        return false
    }

    override fun onTouchEvent(event: MotionEvent?): Boolean {
        if (!isEnabledInAdapterView() || !isEnabled) {
            return true
        }
        if (!isSwipeEnabled()) {
            return super.onTouchEvent(event)
        }
        val action = event!!.actionMasked
        _gestureDetector.onTouchEvent(event)
        val status = getOpenStatus()
        var touching: ViewGroup? = null
        if (status == Status.Close) {
            touching = getSurfaceView()
        } else if (status == Status.Open) {
            touching = getBottomView()
        }
        when (action) {
            MotionEvent.ACTION_DOWN -> {
                _dragHelper?.processTouchEvent(event)
                parent.requestDisallowInterceptTouchEvent(true)
                _sX = event.rawX
                _sY = event.rawY
                touching?.isPressed = true
                return true
            }
            MotionEvent.ACTION_MOVE -> {
                val distanceX = event.rawX - _sX
                val distanceY = event.rawY - _sY
                var angle = Math.abs(distanceY / distanceX)
                angle = Math.toDegrees(Math.atan(angle.toDouble())).toFloat()
                var doNothing = false
                if (_dragEdge == DragEdge.Right) {
                    var suitable = (status == Status.Open && distanceX > 0) || (status == Status.Close && distanceX < 0)
                    suitable = suitable || (status == Status.Middle)
                    if (angle > 30 || !suitable) {
                        doNothing = true
                    }
                }
                if (_dragEdge == DragEdge.Left) {
                    var suitable = (status == Status.Open && distanceX < 0) || (status == Status.Close && distanceX > 0)
                    suitable = suitable || status == Status.Middle
                    if (angle > 30 || !suitable) {
                        doNothing = true
                    }
                }
                if (_dragEdge == DragEdge.Top) {
                    var suitable = (status == Status.Open && distanceY < 0) || (status == Status.Close && distanceY > 0)
                    suitable = suitable || status == Status.Middle
                    if (angle < 60 || !suitable) {
                        doNothing = true
                    }
                }
                if (_dragEdge == DragEdge.Bottom) {
                    var suitable = (status == Status.Open && distanceY > 0) || (status == Status.Close && distanceY < 0)
                    suitable = suitable || status == Status.Middle
                    if (angle < 60 || !suitable) {
                        doNothing = true
                    }
                }
                if (doNothing) {
                    parent.requestDisallowInterceptTouchEvent(false)
                    return false
                } else {
                    touching?.isPressed = false
                    parent.requestDisallowInterceptTouchEvent(true)
                    _dragHelper?.processTouchEvent(event)
                }
            }
            MotionEvent.ACTION_UP,
            MotionEvent.ACTION_CANCEL -> {
                _sX = -1.0f
                _sY = -1.0f
                touching?.isPressed = false
            }
            else -> {
                parent.requestDisallowInterceptTouchEvent(true)
                _dragHelper?.processTouchEvent(event)
            }
        }
        return true
    }


    inner class SwipeDetector: GestureDetector.SimpleOnGestureListener() {

        override fun onDown(e: MotionEvent?): Boolean = true

        override fun onSingleTapUp(e: MotionEvent?): Boolean {
            if (_doubleClickListener == null) {
                performAdapterViewItemClick(e)
            }
            return true
        }

        override fun onSingleTapConfirmed(e: MotionEvent?): Boolean {
            if (_doubleClickListener != null) {
                performAdapterViewItemClick(e)
            }
            return true
        }

        override fun onLongPress(e: MotionEvent?) {
            performLongClick()
        }

        override fun onDoubleTap(e: MotionEvent?): Boolean {
            if (_doubleClickListener != null) {
                var target: View
                val bottom = getBottomView()
                val surface = getSurfaceView()
                if (e!!.x > bottom.left && e.x < bottom.right && e.y > bottom.top && e.y < bottom.bottom) {
                    target = bottom
                } else {
                    target = surface
                }
                _doubleClickListener?.onDoubleClick(this@SwipeLayout, target == surface)
            }
            return true
        }
    }

    private fun performAdapterViewItemClick(@Suppress("UNUSED_PARAMETER") e: MotionEvent?) {
        var t = parent
        while (t != null) {
            if (t is AdapterView<*>) {
                val view = t
                val p = view.getPositionForView(this@SwipeLayout)
                if (p != AdapterView.INVALID_POSITION && view.performItemClick(view.getChildAt(p - view.firstVisiblePosition), p, view.adapter.getItemId(p))) {
                    return
                }
            } else {
                if (t is View && t.performClick()) {
                    return
                }
            }
            t = t.parent
        }
    }

    fun addSwipeListener(l: SwipeListener) = _swipeListeners.add(l)

    fun removeSwipeListener(l: SwipeListener) = _swipeListeners.remove(l)

    fun addSwipeDenier(denier: SwipeDenier) = _swipeDeniers.add(denier)

    fun removeSwipeDenier(denier: SwipeDenier) = _swipeDeniers.remove(denier)

    fun removeAllSwipeDeniers() = _swipeDeniers.clear()

    fun addRevealListener(childId: Int, l: OnRevealListener) {
        val child = findViewById(childId) ?: throw IllegalArgumentException("Child does not belong to SwipeListener.")
        if (!_showEntirely.containsKey(child)) {
            _showEntirely.put(child, false)
        }
        if (_revealListeners.get(child) == null)
            _revealListeners.put(child, arrayListOf<OnRevealListener>())
        _revealListeners[child]?.add(l)
    }

    fun addRevealListener(childIds: IntArray, l: OnRevealListener) {
        for (i in childIds) {
            addRevealListener(i, l)
        }
    }

    fun removeRevealListener(childId: Int, l: OnRevealListener) {
        val child = findViewById(childId) ?: return
        _showEntirely.remove(child)
        if (_revealListeners.containsKey(child)) {
            _revealListeners[child]?.remove(l)
        }
    }

    fun removeAllRevealListeners(childId: Int) {
        val child = findViewById(childId)
        if (child != null) {
            _revealListeners.remove(child)
            _showEntirely.remove(child)
        }
    }

    fun addOnLayoutListener(l: OnLayout) {
        if (_onLayoutListeners == null) {
            _onLayoutListeners = arrayListOf<OnLayout>()
        }
        _onLayoutListeners?.add(l)
    }

    fun removeOnLayoutListener(l: OnLayout) = _onLayoutListeners?.remove(l)

    fun setSwipeEnabled(enabled: Boolean) {
        _swipeEnabled = enabled
    }

    private fun insideAdapterView(): Boolean = getAdapterView() != null

    fun setDragEdge(dragEdge: DragEdge) {
        _dragEdge = dragEdge
        requestLayout()
    }

    fun setDragDistance(max: Int) {
        if (max < 0) {
            throw IllegalArgumentException("Drag distance can not be < 0")
        }
        _dragDistance = dp2px(max.toFloat())
        requestLayout()
    }

    fun toggle() = toggle(true)

    fun toggle(smooth: Boolean) {
        if (getOpenStatus() == Status.Open) {
            close(smooth)
        } else if (getOpenStatus() == Status.Close) {
            open(smooth)
        }
    }

    fun setOnDoubleClickListener(doubleClickListener: DoubleClickListener?) {
        _doubleClickListener = doubleClickListener
    }
}