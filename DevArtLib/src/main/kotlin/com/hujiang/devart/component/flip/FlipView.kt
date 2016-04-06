package com.hujiang.devart.component.flip

import android.animation.Animator
import android.animation.AnimatorListenerAdapter
import android.animation.ValueAnimator
import android.content.Context
import android.database.DataSetObserver
import android.graphics.*
import android.util.AttributeSet
import android.view.MotionEvent
import android.view.VelocityTracker
import android.view.View
import android.view.ViewConfiguration
import android.view.animation.AccelerateDecelerateInterpolator
import android.view.animation.DecelerateInterpolator
import android.widget.FrameLayout
import android.widget.ListAdapter
import android.widget.Scroller
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/6/16.
 */
class FlipView: FrameLayout {

    companion object {
        private val INVALID_PAGE_POSITION = -1
        private val INVALID_FLIP_DISTANCE = -1
        private val PEAK_ANIM_DURATION = 600
        private val MAX_SINGLE_PAGE_FLIP_ANIM_DURATION = 300
        private val FLIP_DISTANCE_PER_PAGE = 180
        private val MAX_SHADOW_ALPHA = 180
        private val MAX_SHADE_ALPHA = 130
        private val MAX_SHINE_ALPHA = 100
        private val INVALID_POINTER = -1
        private val VERTICAL_FLIP = 0
        private val HORIZONTAL_FLIP = 1
    }

    private var _isFlippingVertically = true
    private var _scroller: Scroller? = null
    private val _flipInterpolator = DecelerateInterpolator()
    private var _peakAnim: ValueAnimator? = null
    private var _peakInterpolator = AccelerateDecelerateInterpolator()
    private var _isFlipping = false
    private var _isUnableToFlip = false
    private var _isFlippingEnabled = true
    private var _lastTouchAllowed = true
    private var _touchSlop = 0
    private var _isOverFlipping = false
    private var _lastX = -1.0f
    private var _lastY = -1.0f
    private var _activePointerId = INVALID_POINTER
    private var _velocityTracker: VelocityTracker? = null
    private var _minimumVelocity = 0
    private var _maximumVelocity = 0
    private var _recycler = Recycler()
    private var _adapter: ListAdapter? = null
    private var _pageCount = 0
    private var _previousPage = Page()
    private var _currentPage = Page()
    private var _nextPage = Page()
    private var _emptyView: View? = null
    private var _onFlipListener: OnFlipListener? = null
    private var _onOverFlipListener: OnOverFlipListener? = null
    private var _flipDistance = INVALID_FLIP_DISTANCE.toFloat()
    private var _currentPageIndex = INVALID_PAGE_POSITION
    private var _lastDispatchedPageEventIndex = 0
    private var _currentPageId = 0L
    private var _overFlipMode = OverFlipMode.GLOW
    private var _overFlipper: OverFlipper? = null
    private var _topRect = Rect()
    private var _bottomRect = Rect()
    private var _rightRect = Rect()
    private var _leftRect = Rect()
    private var _camera = Camera()
    private var _matrix = Matrix()
    private var _shadowPaint = Paint()
    private var _shadePaint = Paint()
    private var _shinePaint = Paint()

    private var _dataSetObserver = object: DataSetObserver() {
        override fun onChanged() {
            dataSetChanged()
        }
        override fun onInvalidated() {
            dataSetInvalidated()
        }
    }

    constructor(context: Context): this(context, null)

    constructor(context: Context, attrs: AttributeSet?): this(context, attrs, 0)

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        val a = context.obtainStyledAttributes(attrs, R.styleable.FlipView)
        _isFlippingVertically = a.getInt(R.styleable.FlipView_orientation, VERTICAL_FLIP) == VERTICAL_FLIP
        setOverFlipMode(OverFlipMode.values()[a.getInt(R.styleable.FlipView_overFlipMode, 0)])
        a.recycle()
        init()
    }

    private fun init() {
        val configuration = ViewConfiguration.get(context)
        _scroller = Scroller(context, _flipInterpolator)
        _touchSlop = configuration.scaledPagingTouchSlop
        _minimumVelocity = configuration.scaledMinimumFlingVelocity
        _maximumVelocity = configuration.scaledMaximumFlingVelocity
        _shadowPaint.color = Color.BLACK
        _shadowPaint.style = Paint.Style.FILL
        _shadePaint.color = Color.BLACK
        _shadePaint.style = Paint.Style.FILL
        _shinePaint.color = Color.WHITE
        _shinePaint.style = Paint.Style.FILL
    }

    fun isFlippingVertically(): Boolean = _isFlippingVertically

    fun setOverFlipMode(overFlipMode: OverFlipMode) {
        _overFlipMode = overFlipMode
        _overFlipper = OverFlipperFactory.create(this, _overFlipMode)
    }

    private fun dataSetChanged() {
        val currentPage = _currentPageIndex
        var newPosition = currentPage
        if (_adapter!!.hasStableIds() && currentPage != INVALID_PAGE_POSITION) {
            newPosition = getNewPositionOfCurrentPage()
        } else if (currentPage == INVALID_PAGE_POSITION) {
            newPosition = 0
        }
        recycleActiveViews()
        _recycler.setViewTypeCount(_adapter!!.viewTypeCount)
        _recycler.invalidateScraps()
        _pageCount = _adapter!!.count
        newPosition = Math.min(_pageCount - 1, if (newPosition == INVALID_PAGE_POSITION) 0 else newPosition)
        if (newPosition != INVALID_PAGE_POSITION) {
            _currentPageIndex = INVALID_PAGE_POSITION
            _flipDistance = INVALID_FLIP_DISTANCE.toFloat()
            flipTo(newPosition)
        } else {
            _flipDistance = INVALID_FLIP_DISTANCE.toFloat()
            _pageCount = 0
            setFlipDistance(0.0f)
        }
        updateEmptyStatus()
    }

    private fun updateEmptyStatus() {
        val empty = _adapter == null || _pageCount == 0
        if (empty) {
            if (_emptyView != null) {
                _emptyView?.visibility = View.VISIBLE
                visibility = View.GONE
            } else {
                visibility = View.VISIBLE
            }
        } else {
            if (_emptyView != null) {
                _emptyView?.visibility = View.GONE
            }
            visibility = View.VISIBLE
        }
    }

    fun flipTo(page: Int) {
        if (page < 0 || page > _pageCount - 1) {
            throw IllegalArgumentException("That page does not exist")
        }
        endFlip()
        setFlipDistance(page * FLIP_DISTANCE_PER_PAGE.toFloat())
    }

    private fun endFlip(): Boolean {
        val wasflipping = _isFlipping
        _isFlipping = false
        _isUnableToFlip = false
        _lastTouchAllowed = false
        if (_velocityTracker != null) {
            _velocityTracker?.recycle()
            _velocityTracker = null
        }
        return wasflipping
    }

    private fun setFlipDistance(flipDistance: Float) {
        if (_pageCount < 1) {
            _flipDistance = 0.0f
            _currentPageIndex = INVALID_PAGE_POSITION
            _currentPageId = -1
            recycleActiveViews()
            return
        }
        if (flipDistance == _flipDistance) {
            return
        }
        _flipDistance = flipDistance
        val currentPageIndex = Math.round(_flipDistance / FLIP_DISTANCE_PER_PAGE).toInt()
        if (_currentPageIndex != currentPageIndex) {
            _currentPageIndex = currentPageIndex
            _currentPageId = _adapter!!.getItemId(_currentPageIndex)
            recycleActiveViews()
            if (_currentPageIndex > 0) {
                fillPageForIndex(_previousPage, _currentPageIndex - 1)
                addView(_previousPage.v)
            }
            if (_currentPageIndex >= 0 && _currentPageIndex < _pageCount) {
                fillPageForIndex(_currentPage, _currentPageIndex)
                addView(_currentPage.v)
            }
            if (_currentPageIndex < _pageCount - 1) {
                fillPageForIndex(_nextPage, _currentPageIndex + 1)
                addView(_nextPage.v)
            }
        }
        invalidate()
    }

    private fun fillPageForIndex(p: Page, i: Int) {
        p.position = i
        p.viewType = _adapter!!.getItemViewType(p.position)
        p.v = getView(p.position, p.viewType)
        p.valid = true
    }

    private fun getView(index: Int, viewType: Int): View? {
        val scrap = _recycler.getScrapView(index, viewType)
        var v: View?
        if (scrap == null || !scrap.valid) {
            v = _adapter!!.getView(index, scrap?.scrap, this)
        } else {
            v = scrap.scrap
        }
        return v
    }

    private fun recycleActiveViews() {
        if (_previousPage.valid) {
            removeView(_previousPage.v)
            _recycler.addScrapView(_previousPage.v, _previousPage.position, _previousPage.viewType)
            _previousPage.valid = false
        }
        if (_currentPage.valid) {
            removeView(_currentPage.v)
            _recycler.addScrapView(_currentPage.v, _currentPage.position, _currentPage.viewType)
            _currentPage.valid = false
        }
        if (_nextPage.valid) {
            removeView(_nextPage.v)
            _recycler.addScrapView(_nextPage.v, _nextPage.position, _nextPage.viewType)
            _nextPage.valid = false
        }
    }

    private fun getNewPositionOfCurrentPage(): Int {
        if (_currentPageId == _adapter!!.getItemId(_currentPageIndex)) {
            return _currentPageIndex
        }
        for (i in 0.._adapter!!.count - 1) {
            if (_currentPageId == _adapter!!.getItemId(i)) {
                return i
            }
        }
        return _currentPageIndex
    }

    private fun dataSetInvalidated() {
        if (_adapter != null) {
            _adapter?.unregisterDataSetObserver(_dataSetObserver)
            _adapter = null
        }
        _recycler = Recycler()
        removeAllViews()
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        val width = getDefaultSize(0, widthMeasureSpec)
        val height = getDefaultSize(0, heightMeasureSpec)
        measureChildren(widthMeasureSpec, heightMeasureSpec)
        setMeasuredDimension(width, height)
    }

    override fun measureChildren(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        val width = getDefaultSize(0, widthMeasureSpec)
        val height = getDefaultSize(0, heightMeasureSpec)
        val childWidthMeasureSpec = MeasureSpec.makeMeasureSpec(width,MeasureSpec.EXACTLY)
        val childHeightMeasureSpec = MeasureSpec.makeMeasureSpec(height,MeasureSpec.EXACTLY)
        for (i in 0..childCount - 1) {
            val child = getChildAt(i)
            measureChild(child, childWidthMeasureSpec, childHeightMeasureSpec)
        }
    }

    override fun measureChild(child: View?, parentWidthMeasureSpec: Int, parentHeightMeasureSpec: Int) {
        child?.measure(parentWidthMeasureSpec, parentHeightMeasureSpec)
    }

    override fun onLayout(changed: Boolean, left: Int, top: Int, right: Int, bottom: Int) {
        layoutChildren()
        _topRect.top = 0
        _topRect.left = 0
        _topRect.right = width
        _topRect.bottom = height / 2
        _bottomRect.top = height / 2
        _bottomRect.left = 0
        _bottomRect.right = width
        _bottomRect.bottom = height
        _leftRect.top = 0
        _leftRect.left = 0
        _leftRect.right = width / 2
        _leftRect.bottom = height
        _rightRect.top = 0
        _rightRect.left = getWidth() / 2
        _rightRect.right = width
        _rightRect.bottom = height
    }

    private fun layoutChildren() {
        for (i in 0..childCount - 1) {
            val child = getChildAt(i)
            layoutChild(child)
        }
    }

    private fun layoutChild(child: View?) = child?.layout(0, 0, width, height)

    override fun onInterceptTouchEvent(ev: MotionEvent?): Boolean {
        if (!_isFlippingEnabled) {
            return false
        }
        if (_pageCount < 1) {
            return false
        }
        val action = ev!!.action and MotionEvent.ACTION_MASK
        if (action == MotionEvent.ACTION_CANCEL || action == MotionEvent.ACTION_UP) {
            _isFlipping = false
            _isUnableToFlip = false
            _activePointerId = INVALID_POINTER
            if (_velocityTracker != null) {
                _velocityTracker?.recycle()
                _velocityTracker = null
            }
            return false
        }
        if (action != MotionEvent.ACTION_DOWN) {
            if (_isFlipping) {
                return true
            } else if (_isUnableToFlip) {
                return false
            }
        }
        when (action) {
            MotionEvent.ACTION_MOVE -> {
                val activePointerId = _activePointerId
                if (activePointerId != INVALID_POINTER) {
                    val pointerIndex = ev.findPointerIndex(activePointerId)
                    if (pointerIndex == -1) {
                        _activePointerId = INVALID_POINTER
                    } else {
                        val x = ev.getX(pointerIndex)
                        val dx = x - _lastX
                        val xDiff = Math.abs(dx)
                        val y = ev.getY(pointerIndex)
                        val dy = y - _lastY
                        val yDiff = Math.abs(dy)

                        if ((_isFlippingVertically && yDiff > _touchSlop && yDiff > xDiff) || (!_isFlippingVertically && xDiff > _touchSlop && xDiff > yDiff)) {
                            _isFlipping = true
                            _lastX = x
                            _lastY = y
                        } else if ((_isFlippingVertically && xDiff > _touchSlop) || (!_isFlippingVertically && yDiff > _touchSlop)) {
                            _isUnableToFlip = true
                        }
                    }
                }
            }
            MotionEvent.ACTION_DOWN -> {
                _activePointerId = ev.action and MotionEvent.ACTION_POINTER_INDEX_MASK
                _lastX = ev.getX(_activePointerId)
                _lastY = ev.getY(_activePointerId)
                _isFlipping = !_scroller!!.isFinished or (_peakAnim != null)
                _isUnableToFlip = false
                _lastTouchAllowed = true
            }

            MotionEvent.ACTION_POINTER_UP -> onSecondaryPointerUp(ev)
        }
        if (!_isFlipping) {
            trackVelocity(ev)
        }
        return _isFlipping
    }

    private fun trackVelocity(ev: MotionEvent?) {
        if (_velocityTracker == null) {
            _velocityTracker = VelocityTracker.obtain()
        }
        _velocityTracker?.addMovement(ev)
    }

    private fun onSecondaryPointerUp(ev: MotionEvent?) {
        val pointerIndex = ev!!.actionIndex
        val pointerId = ev.getPointerId(pointerIndex)
        if (pointerId == _activePointerId) {
            val newPointerIndex = if (pointerIndex == 0) 1 else 0
            _lastX = ev.getX(newPointerIndex)
            _activePointerId = ev.getPointerId(newPointerIndex)
            _velocityTracker?.clear()
        }
    }

    override fun onTouchEvent(ev: MotionEvent?): Boolean {
        if (!_isFlippingEnabled) {
            return false
        }
        if (_pageCount < 1) {
            return false
        }
        if (!_isFlipping && !_lastTouchAllowed) {
            return false
        }
        val action = ev!!.action
        if (action == MotionEvent.ACTION_UP || action == MotionEvent.ACTION_CANCEL || action == MotionEvent.ACTION_OUTSIDE) {
            _lastTouchAllowed = false
        } else {
            _lastTouchAllowed = true
        }
        trackVelocity(ev)
        when (action and MotionEvent.ACTION_MASK) {
            MotionEvent.ACTION_DOWN -> {
                if (endScroll() || endPeak()) {
                    _isFlipping = true
                }
                _lastX = ev.x
                _lastY = ev.y
                _activePointerId = ev.getPointerId(0)
            }
            MotionEvent.ACTION_MOVE -> {
                if (!_isFlipping) {
                    val pointerIndex = ev.findPointerIndex(_activePointerId)
                    if (pointerIndex == -1) {
                        _activePointerId = INVALID_POINTER
                    } else {
                        val x = ev.getX(pointerIndex)
                        val xDiff = Math.abs(x - _lastX)
                        val y = ev.getY(pointerIndex)
                        val yDiff = Math.abs(y - _lastY)
                        if ((_isFlippingVertically && yDiff > _touchSlop && yDiff > xDiff) || (!_isFlippingVertically && xDiff > _touchSlop && xDiff > yDiff)) {
                            _isFlipping = true
                            _lastX = x
                            _lastY = y
                        }
                    }
                }
                if (_isFlipping) {
                    val activePointerIndex = ev.findPointerIndex(_activePointerId)
                    if (activePointerIndex == -1) {
                        _activePointerId = INVALID_POINTER
                    } else {
                        val x = ev.getX(activePointerIndex)
                        val deltaX = _lastX - x
                        val y = ev.getY(activePointerIndex)
                        val deltaY = _lastY - y
                        _lastX = x
                        _lastY = y
                        var deltaFlipDistance: Float
                        if (_isFlippingVertically) {
                            deltaFlipDistance = deltaY
                        } else {
                            deltaFlipDistance = deltaX
                        }
                        deltaFlipDistance /= ((if (isFlippingVertically()) height else width) / FLIP_DISTANCE_PER_PAGE)
                        setFlipDistance(_flipDistance + deltaFlipDistance)
                        val minFlipDistance = 0
                        val maxFlipDistance = (_pageCount - 1) * FLIP_DISTANCE_PER_PAGE
                        val isOverFlipping = _flipDistance < minFlipDistance || _flipDistance > maxFlipDistance
                        if (isOverFlipping) {
                            _isOverFlipping = true
                            setFlipDistance(_overFlipper!!.calculate(_flipDistance, minFlipDistance.toFloat(), maxFlipDistance.toFloat()))
                            if (_onOverFlipListener != null) {
                                val overFlip = _overFlipper!!.getTotalOverFlip()
                                _onOverFlipListener?.onOverFlip(this, _overFlipMode, overFlip < 0, Math.abs(overFlip), FLIP_DISTANCE_PER_PAGE.toFloat())
                            }
                        } else if (_isOverFlipping) {
                            _isOverFlipping = false
                            if (_onOverFlipListener != null) {
                                _onOverFlipListener?.onOverFlip(this, _overFlipMode, false, 0.0f, FLIP_DISTANCE_PER_PAGE.toFloat())
                                _onOverFlipListener?.onOverFlip(this, _overFlipMode, true, 0.0f, FLIP_DISTANCE_PER_PAGE.toFloat())
                            }
                        }
                    }
                }
            }
            MotionEvent.ACTION_UP,
            MotionEvent.ACTION_CANCEL -> {
                if (_isFlipping) {
                    val velocityTracker = _velocityTracker
                    velocityTracker?.computeCurrentVelocity(1000, _maximumVelocity.toFloat())
                    var velocity = 0
                    if (isFlippingVertically()) {
                        velocity = velocityTracker!!.getYVelocity(_activePointerId).toInt()
                    } else {
                        velocity = velocityTracker!!.getXVelocity(_activePointerId).toInt()
                    }
                    smoothFlipTo(getNextPage(velocity))
                    _activePointerId = INVALID_POINTER
                    endFlip()
                    _overFlipper?.overFlipEnded()
                }
            }
            MotionEvent.ACTION_POINTER_DOWN -> {
                val index = ev.actionIndex
                val x = ev.getX(index)
                val y = ev.getY(index)
                _lastX = x
                _lastY = y
                _activePointerId = ev.getPointerId(index)
            }
            MotionEvent.ACTION_POINTER_UP -> {
                onSecondaryPointerUp(ev)
                val index = ev.findPointerIndex(_activePointerId)
                val x = ev.getX(index)
                val y = ev.getY(index)
                _lastX = x
                _lastY = y
            }
        }
        if (_activePointerId == INVALID_POINTER) {
            _lastTouchAllowed = false
        }
        return true
    }

    private fun getNextPage(velocity: Int): Int {
        var nextPage: Int
        if (velocity > _minimumVelocity) {
            nextPage = getCurrentPageFloor()
        } else if (velocity < -_minimumVelocity) {
            nextPage = getCurrentPageCeil()
        } else {
            nextPage = getCurrentPageRound()
        }
        return Math.min(Math.max(nextPage, 0), _pageCount - 1)
    }

    private fun getCurrentPageRound(): Int = Math.round(_flipDistance / FLIP_DISTANCE_PER_PAGE)

    private fun getCurrentPageFloor(): Int = Math.floor(_flipDistance * 1.0 / FLIP_DISTANCE_PER_PAGE).toInt()

    private fun getCurrentPageCeil(): Int = Math.ceil(_flipDistance * 1.0 / FLIP_DISTANCE_PER_PAGE).toInt()

    fun smoothFlipTo(page: Int) {
        if (page < 0 || page > _pageCount - 1) {
            throw IllegalArgumentException("That page does not exist")
        }
        val start = _flipDistance.toInt()
        val delta = page * FLIP_DISTANCE_PER_PAGE - start
        endFlip()
        _scroller?.startScroll(0, start, 0, delta, getFlipDuration(delta))
        invalidate()
    }

    private fun getFlipDuration(deltaFlipDistance: Int): Int {
        val distance = Math.abs(deltaFlipDistance)
        return (MAX_SINGLE_PAGE_FLIP_ANIM_DURATION * Math.sqrt(distance * 1.0 / FLIP_DISTANCE_PER_PAGE)).toInt()
    }

    private fun endScroll(): Boolean {
        val wasScrolling = !_scroller!!.isFinished
        _scroller?.abortAnimation()
        return wasScrolling
    }

    private fun endPeak(): Boolean {
        val wasPeaking = _peakAnim != null
        if (_peakAnim != null) {
            _peakAnim?.cancel()
            _peakAnim = null
        }
        return wasPeaking
    }

    override fun dispatchDraw(canvas: Canvas?) {
        if (_pageCount < 1) {
            return
        }
        if (!_scroller!!.isFinished && _scroller!!.computeScrollOffset()) {
            setFlipDistance(_scroller!!.currY.toFloat())
        }
        if (_isFlipping || !_scroller!!.isFinished || _peakAnim != null) {
            showAllPages()
            drawPreviousHalf(canvas)
            drawNextHalf(canvas)
            drawFlippingHalf(canvas)
        } else {
            endScroll()
            setDrawWithLayer(_currentPage.v, false)
            hideOtherPages(_currentPage)
            drawChild(canvas, _currentPage.v, 0)
            if (_lastDispatchedPageEventIndex != _currentPageIndex) {
                _lastDispatchedPageEventIndex = _currentPageIndex
                postFlippedToPage(_currentPageIndex)
            }
        }
        if (_overFlipper!!.draw(canvas)) {
            invalidate()
        }
    }

    private fun postFlippedToPage(page: Int) = post { _onFlipListener?.onFlippedToPage(this@FlipView, page, _adapter!!.getItemId(page)) }

    private fun hideOtherPages(p: Page?) {
        if (_previousPage != p && _previousPage.valid && _previousPage.v!!.visibility != GONE) {
            _previousPage.v?.visibility = GONE
        }
        if (_currentPage != p && _currentPage.valid && _currentPage.v!!.visibility != GONE) {
            _currentPage.v?.visibility = GONE
        }
        if (_nextPage != p && _nextPage.valid && _nextPage.v!!.visibility != GONE) {
            _nextPage.v?.visibility = GONE
        }
        p?.v?.visibility = VISIBLE
    }

    private fun setDrawWithLayer(v: View?, drawWithLayer: Boolean) {
        if (isHardwareAccelerated) {
            if (v!!.layerType != LAYER_TYPE_HARDWARE && drawWithLayer) {
                v.setLayerType(LAYER_TYPE_HARDWARE, null)
            } else if (v.layerType != LAYER_TYPE_NONE && !drawWithLayer) {
                v.setLayerType(LAYER_TYPE_NONE, null)
            }
        }
    }

    private fun drawFlippingHalf(canvas: Canvas?) {
        canvas?.save()
        _camera.save()
        val degreesFlipped = getDegreesFlipped()
        if (degreesFlipped > 90) {
            canvas?.clipRect(if (isFlippingVertically()) _topRect else _leftRect)
            if (_isFlippingVertically) {
                _camera.rotateX(degreesFlipped - 180)
            } else {
                _camera.rotateY(180 - degreesFlipped)
            }
        } else {
            canvas?.clipRect(if (isFlippingVertically()) _bottomRect else _rightRect)
            if (_isFlippingVertically) {
                _camera.rotateX(degreesFlipped)
            } else {
                _camera.rotateY(-degreesFlipped)
            }
        }
        _camera.getMatrix(_matrix)
        positionMatrix()
        canvas?.concat(_matrix)
        setDrawWithLayer(_currentPage.v, true)
        drawChild(canvas, _currentPage.v, 0)
        drawFlippingShadeShine(canvas)
        _camera.restore()
        canvas?.restore()
    }

    private fun drawFlippingShadeShine(canvas: Canvas?) {
        val degreesFlipped = getDegreesFlipped()
        if (degreesFlipped < 90) {
            val alpha = (degreesFlipped / 90.0f * MAX_SHINE_ALPHA).toInt()
            _shinePaint.alpha = alpha
            canvas?.drawRect(if (isFlippingVertically()) _bottomRect else _rightRect, _shinePaint)
        } else {
            val alpha = ((Math.abs(degreesFlipped - 180) / 90.0f) * MAX_SHADE_ALPHA).toInt()
            _shadePaint.alpha = alpha
            canvas?.drawRect(if (isFlippingVertically()) _topRect else _leftRect, _shadePaint)
        }
    }

    private fun positionMatrix() {
        _matrix.preScale(0.25f, 0.25f)
        _matrix.postScale(4.0f, 4.0f)
        _matrix.preTranslate(-width * 1.0f / 2, -height * 1.0f / 2)
        _matrix.postTranslate(width * 1.0f / 2, height * 1.0f / 2)
    }

    private fun drawNextHalf(canvas: Canvas?) {
        canvas?.save()
        canvas?.clipRect(if (isFlippingVertically()) _bottomRect else _rightRect)
        val degreesFlipped = getDegreesFlipped()
        val p = if (degreesFlipped > 90) _currentPage else _nextPage
        if (p.valid) {
            setDrawWithLayer(p.v, true)
            drawChild(canvas, p.v, 0)
        }
        drawNextShadow(canvas)
        canvas?.restore()
    }

    private fun drawNextShadow(canvas: Canvas?) {
        val degreesFlipped = getDegreesFlipped()
        if (degreesFlipped < 90) {
            val alpha = ((Math.abs(degreesFlipped - 90) / 90.0f) * MAX_SHADOW_ALPHA).toInt()
            _shadowPaint.alpha = alpha
            canvas?.drawPaint(_shadowPaint)
        }
    }

    private fun drawPreviousHalf(canvas: Canvas?) {
        canvas?.save()
        canvas?.clipRect(if (isFlippingVertically()) _topRect else _leftRect)
        val degreesFlipped = getDegreesFlipped()
        val p = if (degreesFlipped > 90) _previousPage else _currentPage
        if (p.valid) {
            setDrawWithLayer(p.v, true)
            drawChild(canvas, p.v, 0)
        }
        drawPreviousShadow(canvas)
        canvas?.restore()
    }

    private fun drawPreviousShadow(canvas: Canvas?) {
        val degreesFlipped = getDegreesFlipped()
        if (degreesFlipped > 90) {
            val alpha = (((degreesFlipped - 90) / 90.0f) * MAX_SHADOW_ALPHA).toInt()
            _shadowPaint.alpha = alpha
            canvas?.drawPaint(_shadowPaint)
        }
    }

    private fun getDegreesFlipped(): Float {
        var localFlipDistance = _flipDistance % FLIP_DISTANCE_PER_PAGE
        if (localFlipDistance < 0) {
            localFlipDistance += FLIP_DISTANCE_PER_PAGE
        }
        return (localFlipDistance / FLIP_DISTANCE_PER_PAGE) * 180
    }

    private fun showAllPages() {
        if (_previousPage.valid && _previousPage.v!!.visibility != VISIBLE) {
            _previousPage.v?.visibility = VISIBLE
        }
        if (_currentPage.valid && _currentPage.v!!.visibility != VISIBLE) {
            _currentPage.v?.visibility = VISIBLE
        }
        if (_nextPage.valid && _nextPage.v!!.visibility != VISIBLE) {
            _nextPage.v?.visibility = VISIBLE
        }
    }

    fun setAdapter(adapter: ListAdapter?) {
        _adapter?.unregisterDataSetObserver(_dataSetObserver)
        removeAllViews()
        _adapter = adapter
        _pageCount = if (adapter == null) 0 else _adapter!!.count
        if (adapter != null) {
            _adapter?.registerDataSetObserver(_dataSetObserver)
            _recycler.setViewTypeCount(_adapter!!.viewTypeCount)
            _recycler.invalidateScraps()
        }
        _currentPageIndex = INVALID_PAGE_POSITION
        _flipDistance = INVALID_FLIP_DISTANCE.toFloat()
        setFlipDistance(0.toFloat())
        updateEmptyStatus()
    }

    fun setEmptyView(emptyView: View?) {
        _emptyView = emptyView
        updateEmptyStatus()
    }

    private fun peak(next: Boolean, once: Boolean) {
        val baseFlipDistance = _currentPageIndex * FLIP_DISTANCE_PER_PAGE.toFloat()
        if (next) {
            _peakAnim = ValueAnimator.ofFloat(baseFlipDistance, baseFlipDistance + FLIP_DISTANCE_PER_PAGE / 4)
        } else {
            _peakAnim = ValueAnimator.ofFloat(baseFlipDistance, baseFlipDistance - FLIP_DISTANCE_PER_PAGE / 4)
        }
        _peakAnim?.interpolator = _peakInterpolator
        _peakAnim?.addUpdateListener { animation -> setFlipDistance(animation?.animatedValue as Float) }
        _peakAnim?.addListener(object: AnimatorListenerAdapter() {
            override fun onAnimationEnd(animation: Animator?) {
                endPeak()
            }
        })
        _peakAnim?.setDuration(PEAK_ANIM_DURATION.toLong())
        _peakAnim?.repeatMode = ValueAnimator.REVERSE
        _peakAnim?.repeatCount = if (once) 1 else ValueAnimator.INFINITE
        _peakAnim?.start()
    }

    fun getAdapter(): ListAdapter? = _adapter

    fun getPageCount(): Int = _pageCount

    fun getCurrentPage(): Int = _currentPageIndex

    fun flipBy(delta: Int) = flipTo(_currentPageIndex + delta)

    fun smoothFlipBy(delta: Int) = smoothFlipTo(_currentPageIndex + delta)

    fun peakNext(once: Boolean) {
        if (_currentPageIndex < _pageCount - 1) {
            peak(true, once)
        }
    }

    fun peakPrevious(once: Boolean) {
        if (_currentPageIndex > 0) {
            peak(false, once)
        }
    }

    fun setOnFlipListener(onFlipListener: OnFlipListener?) {
        _onFlipListener = onFlipListener
    }

    fun setOnOverFlipListener(onOverFlipListener: OnOverFlipListener?) {
        _onOverFlipListener = onOverFlipListener
    }

    fun getOverFlipMode(): OverFlipMode = _overFlipMode

    interface OnFlipListener {
        fun onFlippedToPage(v: FlipView?, position: Int, id: Long)
    }

    interface OnOverFlipListener {
        fun onOverFlip(v: FlipView?, mode: OverFlipMode, overFlippingPrevious: Boolean, overFlipDistance: Float, flipDistancePerPage: Float)
    }

    class Page {
        var v: View? = null
        var position = 0
        var viewType = 0
        var valid = false
    }
}