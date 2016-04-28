package com.hujiang.devart.component.miscviews

import android.content.Context
import android.database.DataSetObserver
import android.os.Build
import android.os.Parcel
import android.os.Parcelable
import android.support.v4.view.PagerAdapter
import android.support.v4.view.ViewPager
import android.util.AttributeSet
import android.view.*
import android.widget.Scroller

/**
 * Created by rarnu on 4/6/16.
 */
class DirectionalViewPager: ViewPager {

    companion object {
        private val XML_NS = "http://schemas.android.com/apk/res/android"
        private var USE_CACHE = false
        val HORIZONTAL = 0
        val VERTICAL = 1
        private val INVALID_POINTER = -1

    }

    private val _items = arrayListOf<ItemInfo>()
    private var _adapter: PagerAdapter? = null
    private var _curItem = 0
    private var _restoredCurItem = -1
    private var _restoredAdapterState: Parcelable? = null
    private var _restoredClassLoader: ClassLoader? = null
    private var _scroller: Scroller? = null
    private var _observer: PagerObserver? = null
    private var _childWidthMeasureSpec = 0
    private var _childHeightMeasureSpec = 0
    private var _inLayout = false
    private var _scrollingCacheEnabled = false
    private var _populatePending = false
    private var _scrolling = false
    private var _isBeingDragged = false
    private var _isUnableToDrag = false
    private var _touchSlop = 0
    private var _initialMotion = 0.0f
    private var _lastMotionX = 0.0f
    private var _lastMotionY = 0.0f
    private var _orientation = HORIZONTAL
    private var _activePointerId = INVALID_POINTER
    private var _velocityTracker: VelocityTracker? = null
    private var _minimumVelocity = 0
    private var _maximumVelocity = 0
    private var _onPageChangeListener: OnPageChangeListener? = null
    private var _scrollState = SCROLL_STATE_IDLE

    constructor(context: Context): super(context) {
        initViewPager()
    }

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        initViewPager()
        val orientation = attrs?.getAttributeIntValue(XML_NS, "orientation", -1)
        if (orientation != -1) {
            setOrientation(orientation!!)
        }
    }

    private fun initViewPager() {
        setWillNotDraw(false)
        _scroller = Scroller(context)
        val configuration = ViewConfiguration.get(context)
        _touchSlop = configuration.scaledPagingTouchSlop
        _minimumVelocity = configuration.scaledMinimumFlingVelocity
        _maximumVelocity = configuration.scaledMaximumFlingVelocity
    }

    fun getOrientation(): Int = _orientation

    fun setOrientation(orientation: Int) {
        if (orientation == _orientation) {
            return
        }
        completeScroll()
        _initialMotion = 0.0f
        _lastMotionX = 0.0f
        _lastMotionY = 0.0f
        _velocityTracker?.clear()
        _orientation = orientation
        if (_orientation == HORIZONTAL) {
            scrollTo(_curItem * width, 0)
        } else {
            scrollTo(0, _curItem * height)
        }
        requestLayout()
    }

    private fun completeScroll() {
        var needPopulate = _scrolling
        if (needPopulate) {
            setScrollingCacheEnabled(false)
            _scroller?.abortAnimation()
            val oldX = scrollX
            val oldY = scrollY
            val x = _scroller!!.currX
            val y = _scroller!!.currY
            if (oldX != x || oldY != y) {
                scrollTo(x, y)
            }
            setScrollState(SCROLL_STATE_IDLE)
        }
        _populatePending = false
        _scrolling = false
        for (i in 0.._items.size - 1) {
            val ii = _items[i]
            if (ii.scrolling) {
                needPopulate = true
                ii.scrolling = false
            }
        }
        if (needPopulate) {
            populate()
        }
    }

    fun populate() {
        if (_adapter == null) {
            return
        }
        if (_populatePending) {
            return
        }
        if (windowToken == null) {
            return
        }
        _adapter?.startUpdate(this)
        val startPos = if (_curItem > 0) _curItem - 1 else _curItem
        val count = _adapter!!.count
        val endPos = if (_curItem < (count - 1)) _curItem + 1 else count - 1
        var lastPos = -1
        for (i in _items.size - 1 downTo 0) {
            val ii = _items[i]
            if ((ii.position < startPos || ii.position > endPos) && !ii.scrolling) {
                _items.removeAt(i)
                _adapter?.destroyItem(this, ii.position, ii.obj)
            } else if (lastPos < endPos && ii.position > startPos) {
                lastPos++
                if (lastPos < startPos) {
                    lastPos = startPos
                }
                while (lastPos <= endPos && lastPos < ii.position) {
                    addNewItem(lastPos, i)
                    lastPos++
                    // i++
                }
            }
            lastPos = ii.position
        }
        lastPos = if (_items.size > 0) _items[_items.size - 1].position else -1
        if (lastPos < endPos) {
            lastPos++
            lastPos = if (lastPos > startPos) lastPos else startPos
            while (lastPos <= endPos) {
                addNewItem(lastPos, -1)
                lastPos++
            }
        }
        _adapter?.finishUpdate(this)
    }

    fun addNewItem(position: Int, index: Int) {
        val ii = ItemInfo()
        ii.position = position
        ii.obj = _adapter?.instantiateItem(this, position)
        if (index < 0) {
            _items.add(ii)
        } else {
            _items.add(index, ii)
        }
    }

    private fun setScrollState(newState: Int) {
        if (_scrollState == newState) {
            return
        }
        _scrollState = newState
        _onPageChangeListener?.onPageScrollStateChanged(newState)
    }

    private fun setScrollingCacheEnabled(enabled: Boolean) {
        if (_scrollingCacheEnabled != enabled) {
            _scrollingCacheEnabled = enabled
            if (USE_CACHE) {
                val size = childCount
                for (i in 0..size - 1) {
                    val child = getChildAt(i)
                    if (child.visibility != GONE) {
                        child.isDrawingCacheEnabled = enabled
                    }
                }
            }
        }
    }

    fun dataSetChanged() {
        var needPopulate = _items.isEmpty() && _adapter!!.count > 0
        var newCurrItem = -1
        for (i in _items.size - 1 downTo 0) {
            val ii = _items[i]
            val newPos = _adapter!!.getItemPosition(ii.obj)
            if (newPos == PagerAdapter.POSITION_UNCHANGED) {
                continue
            }
            if (newPos == PagerAdapter.POSITION_NONE) {
                _items.removeAt(i)
                _adapter?.destroyItem(this, ii.position, ii.obj)
                needPopulate = true
                if (_curItem == ii.position) {
                    newCurrItem = Math.max(0, Math.min(_curItem, _adapter!!.count - 1))
                }
                continue
            }
            if (ii.position != newPos) {
                if (ii.position == _curItem) {
                    newCurrItem = newPos
                }
                ii.position = newPos
                needPopulate = true
            }
        }
        if (newCurrItem >= 0) {
            setCurrentItemInternal(newCurrItem, false, true)
            needPopulate = true
        }
        if (needPopulate) {
            populate()
            requestLayout()
        }
    }

    fun setCurrentItemInternal(item: Int, smoothScroll: Boolean, always: Boolean) {
        if (_adapter == null || _adapter!!.count <= 0) {
            setScrollingCacheEnabled(false)
            return
        }
        if (!always && _curItem == item && _items.size != 0) {
            setScrollingCacheEnabled(false)
            return
        }
        var nitem = item
        if (nitem < 0) {
            nitem = 0
        } else if (nitem >= _adapter!!.count) {
            nitem = _adapter!!.count - 1
        }
        if (item > (_curItem + 1) || item < (_curItem - 1)) {
            for (i in 0.._items.size - 1) {
                _items[i].scrolling = true
            }
        }
        val dispatchSelected = _curItem != nitem
        _curItem = nitem
        populate()
        if (smoothScroll) {
            if (_orientation == HORIZONTAL) {
                smoothScrollTo(width * item, 0)
            } else {
                smoothScrollTo(0, height * item)
            }
            if (dispatchSelected && _onPageChangeListener != null) {
                _onPageChangeListener?.onPageSelected(item)
            }
        } else {
            if (dispatchSelected && _onPageChangeListener != null) {
                _onPageChangeListener?.onPageSelected(item)
            }
            completeScroll()
            if (_orientation == HORIZONTAL) {
                scrollTo(width * item, 0)
            } else {
                scrollTo(0, height * item)
            }
        }
    }

    fun smoothScrollTo(x: Int, y: Int) {
        if (childCount == 0) {
            setScrollingCacheEnabled(false)
            return
        }
        val sx = scrollX
        val sy = scrollY
        val dx = x - sx
        val dy = y - sy
        if (dx == 0 && dy == 0) {
            completeScroll()
            return
        }
        setScrollingCacheEnabled(true)
        _scrolling = true
        setScrollState(SCROLL_STATE_SETTLING)
        _scroller?.startScroll(sx, sy, dx, dy)
        invalidate()
    }

    override fun setAdapter(adapter: PagerAdapter?) {
        _adapter = adapter
        if (_adapter != null) {
            if (_observer == null) {
                _observer = PagerObserver()
            }
            _adapter?.registerDataSetObserver(_observer)
            _populatePending = false
            if (_restoredCurItem >= 0) {
                _adapter?.restoreState(_restoredAdapterState, _restoredClassLoader)
                setCurrentItemInternal(_restoredCurItem, false, true)
                _restoredCurItem = -1
                _restoredAdapterState = null
                _restoredClassLoader = null
            } else {
                populate()
            }
        }
    }

    override fun getAdapter(): PagerAdapter? = _adapter

    override fun setCurrentItem(item: Int) {
        _populatePending = false
        setCurrentItemInternal(item, true, false)
    }

    override fun setOnPageChangeListener(listener: OnPageChangeListener?) {
        _onPageChangeListener = listener
    }

    override fun addView(child: View?, index: Int, params: ViewGroup.LayoutParams?) {
        if (_inLayout) {
            addViewInLayout(child, index, params)
            child?.measure(_childWidthMeasureSpec, _childHeightMeasureSpec)
        } else {
            super.addView(child, index, params)
        }
        if (USE_CACHE) {
            if (child!!.visibility != GONE) {
                child.isDrawingCacheEnabled = _scrollingCacheEnabled
            } else {
                child.isDrawingCacheEnabled = false
            }
        }
    }

    override fun onTouchEvent(ev: MotionEvent?): Boolean {
        if (ev!!.action == MotionEvent.ACTION_DOWN && ev.edgeFlags != 0) {
            return false
        }
        if (_adapter == null || _adapter!!.count == 0) {
            return false
        }
        if (_velocityTracker == null) {
            _velocityTracker = VelocityTracker.obtain()
        }
        _velocityTracker?.addMovement(ev)
        val action = ev.action
        when (action and MotionEvent.ACTION_MASK) {
            MotionEvent.ACTION_DOWN -> {
                completeScroll()
                if (_orientation == HORIZONTAL) {
                    _lastMotionX = ev.x
                    _initialMotion = _lastMotionX
                } else {
                    _lastMotionY = ev.y
                    _initialMotion = _lastMotionY
                }
                _activePointerId = ev.getPointerId(0)
            }
            MotionEvent.ACTION_MOVE -> {
                if (!_isBeingDragged) {
                    val pointerIndex = ev.findPointerIndex(_activePointerId)
                    val x = ev.getX(pointerIndex)
                    val y = ev.getY(pointerIndex)
                    val xDiff = Math.abs(x - _lastMotionX)
                    val yDiff = Math.abs(y - _lastMotionY)
                    var primaryDiff: Float
                    var secondaryDiff: Float
                    if (_orientation == HORIZONTAL) {
                        primaryDiff = xDiff
                        secondaryDiff = yDiff
                    } else {
                        primaryDiff = yDiff
                        secondaryDiff = xDiff
                    }
                    if (primaryDiff > _touchSlop && primaryDiff > secondaryDiff) {
                        _isBeingDragged = true
                        if (_orientation == HORIZONTAL) {
                            _lastMotionX = x
                        } else {
                            _lastMotionY = y
                        }
                        setScrollState(SCROLL_STATE_DRAGGING)
                        setScrollingCacheEnabled(true)
                    }
                }
                if (_isBeingDragged) {
                    val activePointerIndex = ev.findPointerIndex(_activePointerId)
                    val x = ev.getX(activePointerIndex)
                    val y = ev.getY(activePointerIndex)
                    var size: Int
                    var scroll: Float
                    if (_orientation == HORIZONTAL) {
                        size = width
                        scroll = scrollX + (_lastMotionX - x)
                        _lastMotionX = x
                    } else {
                        size = height
                        scroll = scrollY + (_lastMotionY - y)
                        _lastMotionY = y
                    }
                    val lowerBound = Math.max(0, (_curItem - 1) * size).toFloat()
                    val upperBound = Math.min(_curItem + 1, _adapter!!.count - 1) * size.toFloat()
                    if (scroll < lowerBound) {
                        scroll = lowerBound
                    } else if (scroll > upperBound) {
                        scroll = upperBound
                    }
                    if (_orientation == HORIZONTAL) {
                        _lastMotionX += scroll - scroll.toInt()
                        scrollTo(scroll.toInt(), scrollY)
                    } else {
                        _lastMotionY += scroll - scroll.toInt()
                        scrollTo(scrollX, scroll.toInt())
                    }
                    if (_onPageChangeListener != null) {
                        val position = (scroll / size).toInt()
                        val positionOffsetPixels = scroll % size
                        val positionOffset = positionOffsetPixels * 1.0f / size
                        _onPageChangeListener?.onPageScrolled(position, positionOffset, positionOffsetPixels.toInt())
                    }
                }
            }
            MotionEvent.ACTION_UP -> {
                if (_isBeingDragged) {
                    val velocityTracker = _velocityTracker
                    velocityTracker?.computeCurrentVelocity(1000, _maximumVelocity.toFloat())
                    var initialVelocity: Int
                    var lastMotion: Float
                    var sizeOverThree: Int
                    if (_orientation == HORIZONTAL) {
                        initialVelocity = velocityTracker!!.getXVelocity(_activePointerId).toInt()
                        lastMotion = _lastMotionX
                        sizeOverThree = width / 3
                    } else {
                        initialVelocity = velocityTracker!!.getYVelocity(_activePointerId).toInt()
                        lastMotion = _lastMotionY
                        sizeOverThree = height / 3
                    }
                    _populatePending = true
                    if ((Math.abs(initialVelocity) > _minimumVelocity) || Math.abs(_initialMotion - lastMotion) >= sizeOverThree) {
                        if (lastMotion > _initialMotion) {
                            setCurrentItemInternal(_curItem - 1, true, true)
                        } else {
                            setCurrentItemInternal(_curItem + 1, true, true)
                        }
                    } else {
                        setCurrentItemInternal(_curItem, true, true)
                    }
                    _activePointerId = INVALID_POINTER
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
                val index = ev.actionIndex
                if (_orientation == HORIZONTAL) {
                    _lastMotionX = ev.getX(index)
                } else {
                    _lastMotionY = ev.getY(index)
                }
                _activePointerId = ev.getPointerId(index)
            }
            MotionEvent.ACTION_POINTER_UP -> {
                onSecondaryPointerUp(ev)
                val index = ev.findPointerIndex(_activePointerId)
                if (_orientation == HORIZONTAL) {
                    _lastMotionX = ev.getX(index)
                } else {
                    _lastMotionY = ev.getY(index)
                }
            }
        }
        return true
    }

    private fun endDrag() {
        _isBeingDragged = false
        _isUnableToDrag = false
        if (_velocityTracker != null) {
            _velocityTracker?.recycle()
            _velocityTracker = null
        }
    }

    private fun onSecondaryPointerUp(ev: MotionEvent?) {
        val pointerIndex = ev!!.actionIndex
        val pointerId = ev.getPointerId(pointerIndex)
        if (pointerId == _activePointerId) {
            val newPointerIndex = if (pointerIndex == 0) 1 else 0
            if (_orientation == HORIZONTAL) {
                _lastMotionX = ev.getX(newPointerIndex)
            } else {
                _lastMotionY = ev.getY(newPointerIndex)
            }
            _activePointerId = ev.getPointerId(newPointerIndex)
            _velocityTracker?.clear()
        }
    }

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
        if (_adapter != null) {
            populate()
        }
    }

    override fun onInterceptTouchEvent(ev: MotionEvent?): Boolean {
        val action = ev!!.action and MotionEvent.ACTION_MASK
        if (action == MotionEvent.ACTION_CANCEL || action == MotionEvent.ACTION_UP) {
            _isBeingDragged = false
            _isUnableToDrag = false
            _activePointerId = INVALID_POINTER
            return false
        }
        if (action != MotionEvent.ACTION_DOWN) {
            if (_isBeingDragged) {
                return true
            }
            if (_isUnableToDrag) {
                return false
            }
        }
        when (action) {
            MotionEvent.ACTION_MOVE -> {
                val activePointerId = _activePointerId
                if (activePointerId != INVALID_POINTER) {
                    val pointerIndex = ev.findPointerIndex(activePointerId)
                    val x = ev.getX(pointerIndex)
                    val y = ev.getY(pointerIndex)
                    val xDiff = Math.abs(x - _lastMotionX)
                    val yDiff = Math.abs(y - _lastMotionY)
                    var primaryDiff: Float
                    var secondaryDiff: Float
                    if (_orientation == HORIZONTAL) {
                        primaryDiff = xDiff
                        secondaryDiff = yDiff
                    } else {
                        primaryDiff = yDiff
                        secondaryDiff = xDiff
                    }
                    if (primaryDiff > _touchSlop && primaryDiff > secondaryDiff) {
                        _isBeingDragged = true
                        setScrollState(SCROLL_STATE_DRAGGING)
                        if (_orientation == HORIZONTAL) {
                            _lastMotionX = x
                        } else {
                            _lastMotionY = y
                        }
                        setScrollingCacheEnabled(true)
                    } else {
                        if (secondaryDiff > _touchSlop) {
                            _isUnableToDrag = true
                        }
                    }
                }
            }

            MotionEvent.ACTION_DOWN -> {
                if (_orientation == HORIZONTAL) {
                    _lastMotionX = ev.x
                    _initialMotion = _lastMotionX
                    _lastMotionY = ev.y
                } else {
                    _lastMotionX = ev.x
                    _lastMotionY = ev.y
                    _initialMotion = ev.y
                }
                _activePointerId = ev.getPointerId(0)
                if (_scrollState == SCROLL_STATE_SETTLING) {
                    _isBeingDragged = true
                    _isUnableToDrag = false
                    setScrollState(SCROLL_STATE_DRAGGING)
                } else {
                    completeScroll()
                    _isBeingDragged = false
                    _isUnableToDrag = false
                }
            }
            MotionEvent.ACTION_POINTER_UP -> onSecondaryPointerUp(ev)
        }
        return _isBeingDragged
    }

    override fun computeScroll() {
        if (!_scroller!!.isFinished) {
            if (_scroller!!.computeScrollOffset()) {
                val oldX = scrollX
                val oldY = scrollY
                val x = _scroller!!.currX
                val y = _scroller!!.currY
                if (oldX != x || oldY != y) {
                    scrollTo(x, y)
                }
                if (_onPageChangeListener != null) {
                    var size: Int
                    var value: Int
                    if (_orientation == HORIZONTAL) {
                        size = width
                        value = x
                    } else {
                        size = height
                        value = y
                    }
                    val position = value / size
                    val offsetPixels = value % size
                    val offset = offsetPixels * 1.0f / size
                    _onPageChangeListener?.onPageScrolled(position, offset, offsetPixels)
                }
                invalidate()
                return
            }
        }
        completeScroll()
    }

    override fun onLayout(changed: Boolean, l: Int, t: Int, r: Int, b: Int) {
        _inLayout = true
        populate()
        _inLayout = false
        val count = childCount
        val size = if (_orientation == HORIZONTAL) r - l else b - t
        for (i in 0..count - 1) {
            val child = getChildAt(i)
            val ii = infoForChild(child)
            if (child.visibility != GONE && ii != null) {
                val off = size * ii.position
                var childLeft = paddingLeft
                var childTop = paddingTop
                if (_orientation == HORIZONTAL) {
                    childLeft += off
                } else {
                    childTop += off
                }
                child.layout(childLeft, childTop, childLeft + child.measuredWidth, childTop + child.measuredHeight)
            }
        }
    }

    private fun infoForChild(child: View?): ItemInfo? {
        for (i in 0.._items.size - 1) {
            val ii = _items[i]
            if (_adapter!!.isViewFromObject(child, ii.obj)) {
                return ii
            }
        }
        return null
    }

    override fun onSizeChanged(w: Int, h: Int, oldw: Int, oldh: Int) {
        super.onSizeChanged(w, h, oldw, oldh)
        if (_orientation == HORIZONTAL) {
            val scrollPos = _curItem * w
            if (scrollPos != scrollX) {
                completeScroll()
                scrollTo(scrollPos, scrollY)
            }
        } else {
            val scrollPos = _curItem * h
            if (scrollPos != scrollY) {
                completeScroll()
                scrollTo(scrollX, scrollPos)
            }
        }
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        setMeasuredDimension(getDefaultSize(0, widthMeasureSpec), getDefaultSize(0, heightMeasureSpec))
        _childWidthMeasureSpec = MeasureSpec.makeMeasureSpec(measuredWidth - paddingLeft - paddingRight, MeasureSpec.EXACTLY)
        _childHeightMeasureSpec = MeasureSpec.makeMeasureSpec(measuredHeight - paddingTop - paddingBottom, MeasureSpec.EXACTLY)
        _inLayout = true
        populate()
        _inLayout = false
        val size = childCount
        for (i in 0..size - 1) {
            val child = getChildAt(i)
            if (child.visibility != GONE) {
                child.measure(_childWidthMeasureSpec, _childHeightMeasureSpec)
            }
        }
    }

    override fun onSaveInstanceState(): Parcelable? {
        val superState = super.onSaveInstanceState()
        val ss = SavedState(superState)
        ss.position = _curItem
        ss.adapterState = _adapter?.saveState()
        return ss
    }

    override fun onRestoreInstanceState(state: Parcelable?) {
        if (state !is SavedState) {
            super.onRestoreInstanceState(state)
            return
        }
        val ss = state
        super.onRestoreInstanceState(ss.superState)
        if (_adapter != null) {
            _adapter?.restoreState(ss.adapterState, ss.loader)
            setCurrentItemInternal(ss.position, false, true)
        } else {
            _restoredCurItem = ss.position
            _restoredAdapterState = ss.adapterState
            _restoredClassLoader = ss.loader
        }
    }

    class ItemInfo {
        var obj: Any? = null
        var position = 0
        var scrolling = false
    }

    inner class PagerObserver: DataSetObserver {
        constructor() { }

        override fun onChanged() {
            dataSetChanged()
        }

        override fun onInvalidated() { }
    }

    class SavedState : BaseSavedState {

        companion object {
            val CREATOR = object: Parcelable.Creator<SavedState> {
                override fun createFromParcel(source: Parcel?): SavedState? = SavedState(source, null)

                override fun newArray(size: Int): Array<SavedState?>? = arrayOfNulls(size)
            }
        }

        private var _position = 0
        var position: Int
            get() = _position
            set(value) { _position = value }
        private var _adapterState: Parcelable? = null
        var adapterState: Parcelable?
            get() = _adapterState
            set(value) { _adapterState = value }
        private var _loader: ClassLoader? = null
        var loader: ClassLoader?
            get() = _loader
            set(value) { _loader = value }

        override fun writeToParcel(dest: Parcel?, flags: Int) {
            super.writeToParcel(dest, flags)
            dest?.writeInt(_position)
            dest?.writeParcelable(_adapterState, flags)
        }

        override fun toString(): String = "FragmentPager.SavedState{${Integer.toHexString(System.identityHashCode(this))} position=$_position}"

        constructor(superState: Parcelable?): super(superState)

        constructor(p: Parcel?, loader: ClassLoader?): super(p) {
            _position = p!!.readInt()
            _adapterState = p.readParcelable(loader)
            _loader = loader
            if (_loader == null) {
                _loader = javaClass.classLoader
            }
        }
    }
}