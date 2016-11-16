package com.hujiang.devart.component.sliding

import android.content.Context
import android.graphics.Canvas
import android.graphics.Rect
import android.os.Build
import android.support.v4.view.ViewPager
import android.util.AttributeSet
import android.view.*
import android.view.animation.Interpolator
import android.widget.Scroller

/**
 * Created by rarnu on 3/24/16.
 */
class CustomViewAbove: ViewGroup {

    companion object {
        private val USE_CACHE = false
        private val MAX_SETTLE_DURATION = 600
        private val MIN_DISTANCE_FOR_FLING = 25
        private val INTERPOLATOR = Interpolator { input ->
            val t = input - 1.0f
            t * t * t * t * t + 1.0f
        }
        private val INVALID_POINTER = -1
    }

    protected var _activePointerId = INVALID_POINTER
    protected var _velocityTracker: VelocityTracker? = null
    protected var _maximumVelocity = 0
    protected var _touchMode = SlidingMenu.TOUCHMODE_MARGIN
    var touchMode: Int
        get() = _touchMode
        set(value) { _touchMode = value }
    private var _content: View? = null
    var content: View?
        get() = _content
        set(value) {
            if (_content != null) {
                this.removeView(_content)
            }
            _content = value
            addView(_content)
        }
    private var _curItem = 0
    var currentItem: Int
        get() = _curItem
        set(value) = setCurrentItemInternal(value, true, false)
    private var _scroller: Scroller? = null
    private var _scrollingCacheEnabled = false
    var scrollingCacheEnabled: Boolean
        get() = _scrollingCacheEnabled
        set(value) {
            if (_scrollingCacheEnabled != value) {
                _scrollingCacheEnabled = value
                if (USE_CACHE) {
                    val size = childCount
                    for (i in 0..size - 1) {
                        val child = getChildAt(i)
                        if (child.visibility != GONE) {
                            child.isDrawingCacheEnabled = value
                        }
                    }
                }
            }
        }
    private var _scrolling = false
    private var _isBeingDragged = false
    private var _isUnableToDrag = false
    private var _touchSlop = 0
    private var _initialMotionX = 0.0f
    private var _lastMotionX = 0.0f
    private var _lastMotionY = 0.0f
    private var _minimumVelocity = 0
    private var _flingDistance = 0
    private var _viewBehind: CustomViewBehind? = null
    var viewBehind: CustomViewBehind?
        get() = _viewBehind
        set(value) { _viewBehind = value }
    private var _enabled = true
    var isSlideEnabled: Boolean
        get() = _enabled
        set(value) { _enabled = value }
    private var _onPageChangeListener: ViewPager.OnPageChangeListener? = null
    var onPageChangeListener: ViewPager.OnPageChangeListener?
        get() = _onPageChangeListener
        set(value) { _onPageChangeListener = value }
    private var _internalPageChangeListener: ViewPager.OnPageChangeListener? = null
    private var _closedListener: OnSlidingClosedListener? = null
    var closedListener: OnSlidingClosedListener?
        get() = _closedListener
        set(value) { _closedListener = value }
    private var _openedListener: OnSlidingOpenedListener? = null
    var openedListener: OnSlidingOpenedListener?
        get() = _openedListener
        set(value) { _openedListener = value }
    private var _ignoredViews = arrayListOf<View>()
    private var _quickReturn = false
    private var _scrollX = 0.0f

    constructor(context: Context): this(context, null)
    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        initCustomViewAbove()
    }

    private fun initCustomViewAbove() {
        setWillNotDraw(false)
        descendantFocusability = FOCUS_AFTER_DESCENDANTS
        isFocusable = true
        _scroller = Scroller(context, INTERPOLATOR)
        val configuration = ViewConfiguration.get(context)
        _touchSlop = configuration.scaledPagingTouchSlop
        _minimumVelocity = configuration.scaledMinimumFlingVelocity
        _maximumVelocity = configuration.scaledMaximumFlingVelocity

        setInternalPageChangeListener(object: SimpleOnSlidingPageChangeListener() {
            override fun onPageSelected(position: Int) {
                when(position) {
                    0, 2 -> _viewBehind?.childrenEnabled = true
                    1 -> _viewBehind?.childrenEnabled = false
                }

            }
        })

        val density = context.resources.displayMetrics.density
        _flingDistance = (MIN_DISTANCE_FOR_FLING * density).toInt()
    }

    fun setCurrentItem(item: Int, smoothScroll: Boolean) = setCurrentItemInternal(item, smoothScroll, false)

    private fun setCurrentItemInternal(item: Int, smoothScroll: Boolean, always: Boolean) = setCurrentItemInternal(item, smoothScroll, always, 0)

    private fun setCurrentItemInternal(item: Int, smoothScroll: Boolean, always: Boolean, velocity: Int) {
        if (!always && _curItem == item) {
            scrollingCacheEnabled = false
            return
        }

        val newitem = _viewBehind!!.getMenuPage(item)
        val dispatchSelected = _curItem != newitem
        _curItem = newitem
        val destX = getDestScrollX(_curItem)
        if (dispatchSelected) {
            _onPageChangeListener?.onPageSelected(newitem)
        }
        if (dispatchSelected) {
            _internalPageChangeListener?.onPageSelected(newitem)
        }
        if (smoothScroll) {
            smoothScrollTo(destX, 0, velocity)
        } else {
            completeScroll()
            scrollTo(destX, 0)
        }
    }

    private fun setInternalPageChangeListener(listener: ViewPager.OnPageChangeListener?): ViewPager.OnPageChangeListener? {
        val oldListener = _internalPageChangeListener
        _internalPageChangeListener = listener
        return oldListener
    }

    fun addIgnoredView(v: View?) {
        if (!_ignoredViews.contains(v)) {
            _ignoredViews.add(v!!)
        }
    }

    fun removeIgnoredView(v: View?) = _ignoredViews.remove(v)

    fun clearIgnoredViews() = _ignoredViews.clear()

    private fun distanceInfluenceForSnapDuration(f: Float): Float {
        var nf = (f - 0.5f).toDouble()
        nf *= (0.3f * Math.PI / 2.0f)
        return Math.sin(nf).toFloat()
    }

    fun getDestScrollX(page: Int): Int = when(page) {
        0, 2 -> _viewBehind!!.getMenuLeft(_content, page)
        1 -> _content!!.left
        else -> 0
    }

    private var leftBound: Int = 0
        get() = _viewBehind!!.getAbsLeftBound(_content)

    private var rightBound: Int = 0
        get() = _viewBehind!!.getAbsRightBound(_content)

    var contentLeft: Int = 0
        get() = _content!!.left + _content!!.paddingLeft

    var isMenuOpen: Boolean = false
        get() = (_curItem == 0 || _curItem == 2)

    private fun isInIgnoredView(ev: MotionEvent?): Boolean {
        val rect = Rect()
        for (v in _ignoredViews) {
            v.getHitRect(rect)
            if (rect.contains(ev!!.x.toInt(), ev.y.toInt())) {
                return true
            }
        }
        return false
    }

    var behindWidth: Int = 0
        get() = if(_viewBehind == null) { 0 } else { _viewBehind!!.behindWidth }

    fun getChildWidth(i: Int): Int = when(i) {
        0 -> behindWidth
        1 -> _content!!.width
        else -> 0
    }

    private fun smoothScrollTo(x: Int, y: Int) = smoothScrollTo(x, y, 0)

    private fun smoothScrollTo(x: Int, y: Int, velocity: Int) {
        if (childCount == 0) {
            scrollingCacheEnabled = false
            return
        }
        val sx = scrollX
        val sy = scrollY
        val dx = x - sx
        val dy = y - sy
        if (dx == 0 && dy == 0) {
            completeScroll()
            if (isMenuOpen) {
                _openedListener?.onSlidingOpened()
            } else {
                _closedListener?.onSlidingClosed()
            }
            return
        }
        scrollingCacheEnabled = true
        _scrolling = true

        val width = behindWidth
        val halfWidth = width * 1.0f / 2
        val distanceRatio = Math.min(1f, 1.0f * Math.abs(dx) / width)
        val distance = halfWidth + halfWidth * distanceInfluenceForSnapDuration(distanceRatio)

        var duration: Int
        val nvelocity = Math.abs(velocity)
        if (nvelocity > 0) {
            duration = 4 * Math.round(1000 * Math.abs(distance / nvelocity))
        } else {
            duration = MAX_SETTLE_DURATION
        }
        duration = Math.min(duration, MAX_SETTLE_DURATION)

        _scroller?.startScroll(sx, sy, dx, dy, duration)
        invalidate()
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        val width = getDefaultSize(0, widthMeasureSpec)
        val height = getDefaultSize(0, heightMeasureSpec)
        setMeasuredDimension(width, height)
        val contentWidth = getChildMeasureSpec(widthMeasureSpec, 0, width)
        val contentHeight = getChildMeasureSpec(heightMeasureSpec, 0, height)
        _content?.measure(contentWidth, contentHeight)
    }

    override fun onSizeChanged(w: Int, h: Int, oldw: Int, oldh: Int) {
        super.onSizeChanged(w, h, oldw, oldh)
        if (w != oldw) {
            completeScroll()
            scrollTo(getDestScrollX(_curItem), scrollY)
        }
    }

    override fun onLayout(changed: Boolean, l: Int, t: Int, r: Int, b: Int) {
        val width = r - l
        val height = b - t
        _content?.layout(0, 0, width, height)
    }

    fun setAboveOffset(i: Int) = _content?.setPadding(i, _content!!.paddingTop, _content!!.paddingRight, _content!!.paddingBottom)

    override fun computeScroll() {
        if (!_scroller!!.isFinished) {
            if (_scroller!!.computeScrollOffset()) {
                val oldX = scrollX
                val oldY = scrollY
                val x = _scroller!!.currX
                val y = _scroller!!.currY
                if (oldX != x || oldY != y) {
                    scrollTo(x, y)
                    pageScrolled(x)
                }
                invalidate()
                return
            }
        }
        completeScroll()
    }

    private fun pageScrolled(xpos: Int) {
        val widthWithMargin = width
        val position = (xpos / widthWithMargin).toInt()
        val offsetPixels = xpos % widthWithMargin
        val offset = offsetPixels * 1.0f / widthWithMargin
        onPageScrolled(position, offset, offsetPixels)
    }

    protected fun onPageScrolled(position: Int, offset: Float, offsetPixels: Int) {
        _onPageChangeListener?.onPageScrolled(position, offset, offsetPixels)
        _internalPageChangeListener?.onPageScrolled(position, offset, offsetPixels)
    }

    private fun completeScroll() {
        val needPopulate = _scrolling
        if (needPopulate) {
            scrollingCacheEnabled = false
            _scroller?.abortAnimation()
            val oldX = scrollX
            val oldY = scrollY
            val x = _scroller!!.currX
            val y = _scroller!!.currY
            if (oldX != x || oldY != y) {
                scrollTo(x, y)
            }
            if (isMenuOpen) {
                _openedListener?.onSlidingOpened()
            } else {
                _closedListener?.onSlidingClosed()
            }
        }
        _scrolling = false
    }

    private fun isTouchAllowed(ev: MotionEvent?): Boolean {
        val x = (ev!!.x + _scrollX).toInt()
        return if (isMenuOpen) {
            _viewBehind!!.menuOpenTouchAllowed(_content, _curItem, x.toFloat())
        } else {
            when(_touchMode) {
                SlidingMenu.TOUCHMODE_FULLSCREEN -> !isInIgnoredView(ev)
                SlidingMenu.TOUCHMODE_NONE -> false
                SlidingMenu.TOUCHMODE_MARGIN -> _viewBehind!!.marginTouchAllowed(_content, x)
                else -> false
            }
        }
    }

    private fun isSlideAllowed(dx: Float): Boolean = if (isMenuOpen) {
        _viewBehind!!.menuOpenSlideAllowed(dx)
    } else {
        _viewBehind!!.menuClosedSlideAllowed(dx)
    }

    private fun getPointerIndex(ev: MotionEvent?, id: Int): Int {
        var activePointerIndex = ev!!.findPointerIndex(id)
        if (activePointerIndex == -1) {
            _activePointerId = INVALID_POINTER
        }
        return activePointerIndex
    }

    override fun onInterceptTouchEvent(ev: MotionEvent?): Boolean {
        if (!_enabled) {
            return false
        }
        val action = ev!!.action and MotionEvent.ACTION_MASK

        if (action == MotionEvent.ACTION_CANCEL || action == MotionEvent.ACTION_UP || (action != MotionEvent.ACTION_DOWN && _isUnableToDrag)) {
            endDrag()
            return false
        }

        when(action) {
            MotionEvent.ACTION_MOVE -> determineDrag(ev)
            MotionEvent.ACTION_DOWN -> {
                val index = ev.actionIndex
                _activePointerId = ev.getPointerId(index)
                if (_activePointerId != INVALID_POINTER) {
                    _lastMotionX = ev.getX(index)
                    _initialMotionX = ev.getX(index)
                    _lastMotionY = ev.getY(index)
                    if (isTouchAllowed(ev)) {
                        _isBeingDragged = false
                        _isUnableToDrag = false
                        if (isMenuOpen && _viewBehind!!.menuTouchInQuickReturn(_content, _curItem, ev.x + _scrollX)) {
                            _quickReturn = true
                        }
                    } else {
                        _isUnableToDrag = true
                    }
                }

            }
            MotionEvent.ACTION_POINTER_UP -> onSecondaryPointerUp(ev)
        }

        if (!_isBeingDragged) {
            if (_velocityTracker == null) {
                _velocityTracker = VelocityTracker.obtain()
            }
            _velocityTracker?.addMovement(ev)
        }
        return _isBeingDragged || _quickReturn
    }


    override fun onTouchEvent(ev: MotionEvent?): Boolean {
        if (!_enabled) {
            return false
        }
        if (!_isBeingDragged && !isTouchAllowed(ev)) {
            return false
        }
        val action = ev!!.action
        if (_velocityTracker == null) {
            _velocityTracker = VelocityTracker.obtain()
        }
        _velocityTracker?.addMovement(ev)

        when (action and MotionEvent.ACTION_MASK) {
            MotionEvent.ACTION_DOWN -> {
                completeScroll()
                val index = ev.actionIndex
                _activePointerId = ev.getPointerId(index)
                _lastMotionX = ev.x
                _initialMotionX = ev.x
            }

            MotionEvent.ACTION_MOVE -> {
                if (!_isBeingDragged) {
                    determineDrag(ev)
                    if (_isUnableToDrag) {
                        return false
                    }
                }
                if (_isBeingDragged) {
                    val activePointerIndex = getPointerIndex(ev, _activePointerId)
                    if (_activePointerId != INVALID_POINTER) {
                        val x = ev.getX(activePointerIndex)
                        val deltaX = _lastMotionX - x
                        _lastMotionX = x
                        val oldScrollX = scrollX
                        var scrollX = oldScrollX + deltaX
                        if (scrollX < leftBound) {
                            scrollX = leftBound.toFloat()
                        } else if (scrollX > rightBound) {
                            scrollX = rightBound.toFloat()
                        }
                        _lastMotionX += scrollX - scrollX.toInt()
                        scrollTo(scrollX.toInt(), scrollY)
                        pageScrolled(scrollX.toInt())
                    }
                }
            }

            MotionEvent.ACTION_UP -> {
                if (_isBeingDragged) {
                    val velocityTracker = _velocityTracker
                    velocityTracker?.computeCurrentVelocity(1000, _maximumVelocity.toFloat())
                    val initialVelocity = velocityTracker!!.getXVelocity(_activePointerId).toInt()
                    val pageOffset = (scrollX - getDestScrollX(_curItem)) * 1.0f / behindWidth
                    val activePointerIndex = getPointerIndex(ev, _activePointerId)
                    if (_activePointerId != INVALID_POINTER) {
                        val x = ev.getX(activePointerIndex)
                        val totalDelta = (x - _initialMotionX).toInt()
                        val nextPage = determineTargetPage(pageOffset, initialVelocity, totalDelta)
                        setCurrentItemInternal(nextPage, true, true, initialVelocity)
                    } else {
                        setCurrentItemInternal(_curItem, true, true, initialVelocity)
                    }
                    _activePointerId = INVALID_POINTER
                    endDrag()
                } else if (_quickReturn && _viewBehind!!.menuTouchInQuickReturn(_content, _curItem, ev.x + _scrollX)) {
                    setCurrentItem(1, true)
                    endDrag()
                }
            }

            MotionEvent.ACTION_CANCEL -> {
                if (_isBeingDragged) {
                    setCurrentItemInternal(_curItem, true, true)
                    _activePointerId = INVALID_POINTER
                    endDrag()
                }
            }

            MotionEvent.ACTION_POINTER_DOWN -> {
                val indexx = ev.actionIndex
                _lastMotionX = ev.getX(indexx)
                _activePointerId = ev.getPointerId(indexx)

            }

            MotionEvent.ACTION_POINTER_UP -> {
                onSecondaryPointerUp(ev)
                val pointerIndex = getPointerIndex(ev, _activePointerId)
                if (_activePointerId != INVALID_POINTER) {
                    _lastMotionX = ev.getX(pointerIndex)
                }
            }
        }
        return true
    }

    private fun determineDrag(ev: MotionEvent?) {
        val activePointerId = _activePointerId
        val pointerIndex = getPointerIndex(ev, activePointerId)
        if (activePointerId == INVALID_POINTER) {
            return
        }
        val x = ev!!.getX(pointerIndex)
        val dx = x - _lastMotionX
        val xDiff = Math.abs(dx)
        val y = ev.getY(pointerIndex)
        val dy = y - _lastMotionY
        val yDiff = Math.abs(dy)
        val deltaDiff = if (isMenuOpen) { _touchSlop *1.0f / 2 } else { _touchSlop * 1.0f }
        if (xDiff > deltaDiff && xDiff > yDiff && isSlideAllowed(dx)) {
            startDrag()
            _lastMotionX = x
            _lastMotionY = y
            scrollingCacheEnabled = true

        } else if (xDiff > _touchSlop) {
            _isUnableToDrag = true
        }
    }

    override fun scrollTo(x: Int, y: Int) {
        super.scrollTo(x, y)
        _scrollX = x.toFloat()
        if (_enabled) {
            _viewBehind!!.scrollBehindTo(_content, x, y)
        }
        (parent as SlidingMenu).manageLayers(percentOpen)
    }

    private fun determineTargetPage(pageOffset: Float, velocity: Int, deltaX: Int): Int {
        var targetPage = _curItem
        if (Math.abs(deltaX) > _flingDistance && Math.abs(velocity) > _minimumVelocity) {
            if (velocity > 0 && deltaX > 0) {
                targetPage -= 1
            } else if (velocity < 0 && deltaX < 0) {
                targetPage += 1
            }
        } else {
            targetPage = Math.round(_curItem + pageOffset).toInt()
        }
        return targetPage
    }

    var percentOpen: Float = 0.0f
        get() = Math.abs(_scrollX - _content!!.left) * 1.0f / behindWidth

    override fun dispatchDraw(canvas: Canvas?) {
        super.dispatchDraw(canvas)
        _viewBehind!!.drawShadow(_content, canvas)
        _viewBehind!!.drawFade(_content, canvas, percentOpen)
        _viewBehind!!.drawSelector(_content, canvas, percentOpen)
    }


    private fun onSecondaryPointerUp(ev: MotionEvent?) {
        val pointerIndex = ev!!.actionIndex
        val pointerId = ev.getPointerId(pointerIndex)
        if (pointerId == _activePointerId) {
            val newPointerIndex = if (pointerIndex == 0) { 1 } else { 0 }
            _lastMotionX = ev.getX(newPointerIndex)
            _activePointerId = ev.getPointerId(newPointerIndex)
            _velocityTracker?.clear()
        }
    }

    private fun startDrag() {
        _isBeingDragged = true
        _quickReturn = false
    }

    private fun endDrag() {
        _quickReturn = false
        _isBeingDragged = false
        _isUnableToDrag = false
        _activePointerId = INVALID_POINTER
        if (_velocityTracker != null) {
            _velocityTracker?.recycle()
            _velocityTracker = null
        }
    }

    protected fun canScroll(v: View?, checkV: Boolean, dx: Int, x: Int, y: Int): Boolean {
        if (v is ViewGroup) {
            val scrollX = v.getScrollX()
            val scrollY = v.getScrollY()
            val count = v.childCount
            for (i in count - 1 downTo 0) {
                val child = v.getChildAt(i)
                if (x + scrollX >= child.left &&
                        x + scrollX < child.right &&
                        y + scrollY >= child.top &&
                        y + scrollY < child.bottom
                        && canScroll(child, true, dx, x + scrollX - child.left, y + scrollY - child.top)) {
                    return true
                }
            }
        }
        return checkV && v!!.canScrollHorizontally(-dx)
    }

    override fun dispatchKeyEvent(event: KeyEvent?): Boolean = super.dispatchKeyEvent(event) || executeKeyEvent(event)

    fun executeKeyEvent(event: KeyEvent?): Boolean {
        var handled = false
        if (event!!.action == KeyEvent.ACTION_DOWN) {
            when (event.keyCode) {
                KeyEvent.KEYCODE_DPAD_LEFT -> handled = arrowScroll(FOCUS_LEFT)
                KeyEvent.KEYCODE_DPAD_RIGHT -> handled = arrowScroll(FOCUS_RIGHT)
                KeyEvent.KEYCODE_TAB -> {
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB) {
                        if (event.hasNoModifiers()) {
                            handled = arrowScroll(FOCUS_FORWARD)
                        } else if (event.hasModifiers(KeyEvent.META_SHIFT_ON)) {
                            handled = arrowScroll(FOCUS_BACKWARD)
                        }
                    }
                }
            }
        }
        return handled
    }

    fun arrowScroll(direction: Int): Boolean {
        var currentFocused = findFocus()
        if (currentFocused == this) {
            currentFocused = null
        }
        var handled = false
        val nextFocused = FocusFinder.getInstance().findNextFocus(this, currentFocused, direction)
        if (nextFocused != null && nextFocused != currentFocused) {
            if (direction == View.FOCUS_LEFT) {
                handled = nextFocused.requestFocus()
            } else if (direction == View.FOCUS_RIGHT) {
                if (currentFocused != null && nextFocused.left <= currentFocused.left) {
                    handled = pageRight()
                } else {
                    handled = nextFocused.requestFocus()
                }
            }
        } else if (direction == FOCUS_LEFT || direction == FOCUS_BACKWARD) {
            handled = pageLeft()
        } else if (direction == FOCUS_RIGHT || direction == FOCUS_FORWARD) {
            handled = pageRight()
        }
        if (handled) {
            playSoundEffect(SoundEffectConstants.getContantForFocusDirection(direction))
        }
        return handled
    }

    private fun pageLeft(): Boolean {
        if (_curItem > 0) {
            setCurrentItem(_curItem - 1, true)
            return true
        }
        return false
    }

    private fun pageRight(): Boolean {
        if (_curItem < 1) {
            setCurrentItem(_curItem + 1, true)
            return true
        }
        return false
    }

    inner open class SimpleOnSlidingPageChangeListener: ViewPager.OnPageChangeListener {
        override fun onPageScrollStateChanged(state: Int) {

        }

        override fun onPageScrolled(position: Int, positionOffset: Float, positionOffsetPixels: Int) {

        }

        override fun onPageSelected(position: Int) {

        }

    }
}