package com.hujiang.devart.component.draglist

import android.content.Context
import android.database.DataSetObserver
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Point
import android.os.SystemClock
import android.util.AttributeSet
import android.util.SparseBooleanArray
import android.view.Gravity
import android.view.MotionEvent
import android.view.View
import android.view.ViewGroup
import android.widget.AbsListView
import android.widget.BaseAdapter
import android.widget.ListAdapter
import android.widget.ListView
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/1/16.
 */
class DragListView: ListView {

    companion object {
        val DRAG_POS_X = 0x1
        val DRAG_NEG_X = 0x2
        val DRAG_POS_Y = 0x4
        val DRAG_NEG_Y = 0x8

        val STOP = -1
        val UP = 0
        val DOWN = 1

        private val IDLE = 0
        private val REMOVING = 1
        private val DROPPING = 2
        private val STOPPED = 3
        private val DRAGGING = 4
        private val NO_CANCEL = 0
        private val ON_TOUCH_EVENT = 1
        private val ON_INTERCEPT_TOUCH_EVENT = 2
        private val sCacheSize = 3

        private fun buildRunList(cip: SparseBooleanArray?, rangeStart: Int, rangeEnd: Int, runStart: IntArray?, runEnd: IntArray?): Int {
            var runCount = 0
            var i = findFirstSetIndex(cip, rangeStart, rangeEnd)
            if (i == -1) {
                return 0
            }
            var position = cip!!.keyAt(i)
            var currentRunStart = position
            var currentRunEnd = currentRunStart + 1
            while (i < cip.size() && position < rangeEnd) {
                if (!cip.valueAt(i)) {
                    i++
                    position = cip.keyAt(i)
                    continue
                }
                if (position == currentRunEnd) {
                    currentRunEnd++
                } else {
                    runStart!![runCount] = currentRunStart
                    runEnd!![runCount] = currentRunEnd
                    runCount++
                    currentRunStart = position
                    currentRunEnd = position + 1
                }
                i++
                position = cip.keyAt(i)
            }
            if (currentRunEnd == rangeEnd) {
                currentRunEnd = rangeStart
            }
            runStart!![runCount] = currentRunStart
            runEnd!![runCount] = currentRunEnd
            runCount++
            if (runCount > 1) {
                if (runStart[0] == rangeStart && runEnd[runCount - 1] == rangeStart) {
                    runStart[0] = runStart[runCount - 1]
                    runCount--
                }
            }
            return runCount
        }

        private fun findFirstSetIndex(sba: SparseBooleanArray?, rangeStart: Int, rangeEnd: Int): Int {
            val size = sba!!.size()
            var i = insertionIndexForKey(sba, rangeStart)
            while (i < size && sba.keyAt(i) < rangeEnd && !sba.valueAt(i)) {
                i++
            }
            if (i == size || sba.keyAt(i) >= rangeEnd) {
                return -1
            }
            return i
        }

        private fun insertionIndexForKey(sba: SparseBooleanArray?, key: Int): Int {
            var low = 0
            var high = sba!!.size()
            while (high - low > 0) {
                val middle = (low + high) shr 1
                if (sba.keyAt(middle) < key) {
                    low = middle + 1
                } else {
                    high = middle
                }
            }
            return low
        }

        private fun rotate(value: Int, offset: Int, lowerBound: Int, upperBound: Int): Int {
            val windowSize = upperBound - lowerBound
            var nvalue = value + offset
            if (nvalue < lowerBound) {
                nvalue += windowSize
            } else if (nvalue >= upperBound) {
                nvalue -= windowSize
            }
            return nvalue
        }
    }

    private var _floatView: View? = null
    private var _floatLoc = Point()
    private var _touchLoc = Point()
    private var _floatViewMid = 0
    private var _floatViewOnMeasured = false
    private var _observer: DataSetObserver? = null
    private var _floatAlpha = 1.0f
    private var _currFloatAlpha = 1.0f
    private var _floatPos = 0
    private var _firstExpPos = 0
    private var _secondExpPos = 0
    private var _animate = false
    private var _srcPos = 0
    private var _dragDeltaX = 0
    private var _dragDeltaY = 0
    private var _dragListener: DragListener? = null
    private var _dropListener: DropListener? = null
    private var _removeListener: RemoveListener? = null
    private var _dragEnabled = true
    private var _dragState = IDLE
    private var _itemHeightCollapsed = 1
    private var _floatViewHeight = 0
    private var _floatViewHeightHalf = 0
    private var _widthMeasureSpec = 0
    private var _sampleViewTypes = arrayOfNulls<View>(1)
    private var _dragScroller: DragScroller? = null
    private var _dragUpScrollStartFrac = 1.0f / 3.0f
    private var _dragDownScrollStartFrac = 1.0f / 3.0f
    private var _upScrollStartY = 0
    private var _downScrollStartY = 0
    private var _downScrollStartYF = 0.0f
    private var _upScrollStartYF = 0.0f
    private var _dragUpScrollHeight = 0.0f
    private var _dragDownScrollHeight = 0.0f
    private var _maxScrollSpeed = 0.5f
    private var _x = 0
    private var _y = 0
    private var _lastY = 0
    private var _dragFlags = 0
    private var _lastCallWasIntercept = false
    private var _inTouchEvent = false
    private var _floatViewManager: IFloatViewManager? = null
    private var _cancelEvent: MotionEvent? = null
    private var _cancelMethod = NO_CANCEL
    private var _slideRegionFrac = 0.25f
    private var _slideFrac = 0.0f
    private var _adapterWrapper: AdapterWrapper? = null
    private var _blockLayoutRequests = false
    private var _ignoreTouchEvent = false
    private var _childHeightCache = HeightCache(sCacheSize)
    private var _removeAnimator: RemoveAnimator? = null
    private var _liftAnimator: LiftAnimator? = null
    private var _dropAnimator: DropAnimator? = null
    private var _useRemoveVelocity = false
    private var _removeVelocityX = 0.0f
    private var _listViewIntercepted = false
    private var _scrollProfile: DragScrollProfile? = object : DragScrollProfile {
        override fun getSpeed(w: Float, t: Long): Float = _maxScrollSpeed * w
    }

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        var defaultDuration = 150
        var removeAnimDuration = defaultDuration
        var dropAnimDuration = defaultDuration

        if (attrs != null) {
            val a = context.obtainStyledAttributes(attrs, R.styleable.DragListView, 0, 0)
            _itemHeightCollapsed = Math.max(1, a.getDimensionPixelSize(R.styleable.DragListView_collapsed_height, 1))
            _floatAlpha = a.getFloat(R.styleable.DragListView_float_alpha, _floatAlpha)
            _currFloatAlpha = _floatAlpha
            _dragEnabled = a.getBoolean(R.styleable.DragListView_drag_enabled, _dragEnabled)
            _slideRegionFrac = Math.max(0.0f, Math.min(1.0f, 1.0f - a.getFloat(R.styleable.DragListView_slide_shuffle_speed, 0.75f)))
            _animate = _slideRegionFrac > 0.0f
            val frac = a.getFloat(R.styleable.DragListView_drag_scroll_start, _dragUpScrollStartFrac)
            setDragScrollStart(frac)
            _maxScrollSpeed = a.getFloat(R.styleable.DragListView_max_drag_scroll_speed, _maxScrollSpeed)
            removeAnimDuration = a.getInt(R.styleable.DragListView_remove_animation_duration, removeAnimDuration)
            dropAnimDuration = a.getInt(R.styleable.DragListView_drop_animation_duration, dropAnimDuration)
            val useDefault = a.getBoolean(R.styleable.DragListView_use_default_controller, true)
            if (useDefault) {
                val removeEnabled = a.getBoolean(R.styleable.DragListView_remove_enabled, false)
                val removeMode = a.getInt(R.styleable.DragListView_remove_mode, DragController.FLING_REMOVE)
                val sortEnabled = a.getBoolean(R.styleable.DragListView_sort_enabled, true)
                val dragInitMode = a.getInt(R.styleable.DragListView_drag_start_mode, DragController.ON_DOWN)
                val dragHandleId = a.getResourceId(R.styleable.DragListView_drag_handle_id, 0)
                val flingHandleId = a.getResourceId(R.styleable.DragListView_fling_handle_id, 0)
                val clickRemoveId = a.getResourceId(R.styleable.DragListView_click_remove_id, 0)
                val bgColor = a.getColor(R.styleable.DragListView_float_background_color, Color.BLACK)
                val controller = DragController(this, dragHandleId, dragInitMode, removeMode, clickRemoveId, flingHandleId)
                controller.setRemoveEnabled(removeEnabled)
                controller.setSortEnabled(sortEnabled)
                controller.setBackgroundColor(bgColor)
                _floatViewManager = controller
                setOnTouchListener(controller)
            }
            a.recycle()
        }
        _dragScroller = DragScroller()
        val smoothness = 0.5f
        if (removeAnimDuration > 0) {
            _removeAnimator = RemoveAnimator(smoothness, removeAnimDuration)
        }
        if (dropAnimDuration > 0) {
            _dropAnimator = DropAnimator(smoothness, dropAnimDuration)
        }
        _cancelEvent = MotionEvent.obtain(0, 0, MotionEvent.ACTION_CANCEL, 0f, 0f, 0f, 0f, 0, 0f, 0f, 0, 0)
        _observer = object : DataSetObserver() {
            private fun cancel() {
                if (_dragState == DRAGGING) {
                    cancelDrag()
                }
            }
            override fun onChanged() = cancel()

            override fun onInvalidated() = cancel()
        }
    }

    fun setDragScrollStart(heightFraction: Float) {
        setDragScrollStarts(heightFraction, heightFraction)
    }

    fun setDragScrollStarts(upperFrac: Float, lowerFrac: Float) {
        if (lowerFrac > 0.5f) {
            _dragDownScrollStartFrac = 0.5f
        } else {
            _dragDownScrollStartFrac = lowerFrac
        }
        if (upperFrac > 0.5f) {
            _dragUpScrollStartFrac = 0.5f
        } else {
            _dragUpScrollStartFrac = upperFrac
        }
        if (height != 0) {
            updateScrollStarts()
        }
    }

    fun cancelDrag() {
        if (_dragState == DRAGGING) {
            _dragScroller?.stopScrolling(true)
            destroyFloatView()
            clearPositions()
            adjustAllItems()
            if (_inTouchEvent) {
                _dragState = STOPPED
            } else {
                _dragState = IDLE
            }
        }
    }

    private fun adjustAllItems() {
        val first = firstVisiblePosition
        val last = lastVisiblePosition
        val begin = Math.max(0, headerViewsCount - first)
        val end = Math.min(last - first, count - 1 - footerViewsCount - first)
        for (i in begin..end) {
            val v = getChildAt(i)
            if (v != null) {
                adjustItem(first + i, v, false)
            }
        }
    }

    private fun adjustItem(position: Int, v: View?, invalidChildHeight: Boolean) {
        val lp = v!!.layoutParams
        var height: Int
        if (position != _srcPos && position != _firstExpPos && position != _secondExpPos) {
            height = ViewGroup.LayoutParams.WRAP_CONTENT
        } else {
            height = calcItemHeight(position, v, invalidChildHeight)
        }
        if (height != lp.height) {
            lp.height = height
            v.layoutParams = lp
        }
        if (position == _firstExpPos || position == _secondExpPos) {
            if (position < _srcPos) {
                (v as DragItemView).setGravity(Gravity.BOTTOM)
            } else if (position > _srcPos) {
                (v as DragItemView).setGravity(Gravity.TOP)
            }
        }
        val oldVis = v.visibility
        var vis = View.VISIBLE
        if (position == _srcPos && _floatView != null) {
            vis = View.INVISIBLE
        }
        if (vis != oldVis) {
            v.visibility = vis
        }
    }

    private fun getChildHeight(position: Int, item: View?, invalidChildHeight: Boolean): Int {
        if (position == _srcPos) {
            return 0
        }
        var child: View
        if (position < headerViewsCount || position >= count - footerViewsCount) {
            child = item!!
        } else {
            child = (item as ViewGroup).getChildAt(0)
        }
        val lp = child.layoutParams
        if (lp != null) {
            if (lp.height > 0) {
                return lp.height
            }
        }
        var childHeight = child.getHeight()
        if (childHeight == 0 || invalidChildHeight) {
            measureItem(child)
            childHeight = child.measuredHeight
        }
        return childHeight
    }

    private fun measureItem(item: View?) {
        var lp = item!!.layoutParams
        if (lp == null) {
            lp = AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT)
            item.layoutParams = lp
        }
        val wspec = ViewGroup.getChildMeasureSpec(_widthMeasureSpec, listPaddingLeft + listPaddingRight, lp.width)
        var hspec: Int
        if (lp.height > 0) {
            hspec = MeasureSpec.makeMeasureSpec(lp.height, MeasureSpec.EXACTLY)
        } else {
            hspec = MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED)
        }
        item.measure(wspec, hspec)
    }

    private fun calcItemHeight(position: Int, item: View?, invalidChildHeight: Boolean): Int =
            calcItemHeight(position, getChildHeight(position, item, invalidChildHeight))

    private fun calcItemHeight(position: Int, childHeight: Int): Int {
        val isSliding = _animate && _firstExpPos != _secondExpPos
        val maxNonSrcBlankHeight = _floatViewHeight - _itemHeightCollapsed
        val slideHeight = (_slideFrac * maxNonSrcBlankHeight).toInt()
        var height: Int
        if (position == _srcPos) {
            if (_srcPos == _firstExpPos) {
                if (isSliding) {
                    height = slideHeight + _itemHeightCollapsed
                } else {
                    height = _floatViewHeight
                }
            } else if (_srcPos == _secondExpPos) {
                height = _floatViewHeight - slideHeight
            } else {
                height = _itemHeightCollapsed
            }
        } else if (position == _firstExpPos) {
            if (isSliding) {
                height = childHeight + slideHeight
            } else {
                height = childHeight + maxNonSrcBlankHeight
            }
        } else if (position == _secondExpPos) {
            height = childHeight + maxNonSrcBlankHeight - slideHeight
        } else {
            height = childHeight
        }
        return height
    }

    private fun clearPositions() {
        _srcPos = -1
        _firstExpPos = -1
        _secondExpPos = -1
        _floatPos = -1
    }

    private fun destroyFloatView() {
        if (_floatView != null) {
            _floatView?.visibility = GONE
            _floatViewManager?.onDestroyFloatView(_floatView)
            _floatView = null
            invalidate()
        }
    }

    private fun updateScrollStarts() {
        val padTop = paddingTop
        val listHeight = height - padTop - paddingBottom
        val heightF = listHeight.toFloat()
        _upScrollStartYF = padTop + _dragUpScrollStartFrac * heightF
        _downScrollStartYF = padTop + (1.0f - _dragDownScrollStartFrac) * heightF
        _upScrollStartY = _upScrollStartYF.toInt()
        _downScrollStartY = _downScrollStartYF.toInt()
        _dragUpScrollHeight = _upScrollStartYF - padTop
        _dragDownScrollHeight = padTop + listHeight - _downScrollStartYF
    }

    fun getFloatAlpha(): Float = _currFloatAlpha

    fun setFloatAlpha(alpha: Float) {
        _currFloatAlpha = alpha
    }

    fun setMaxScrollSpeed(max: Float) {
        _maxScrollSpeed = max
    }

    fun setFloatViewManager(manager: IFloatViewManager?) {
        _floatViewManager = manager
    }

    fun setDragSortListener(l: CombinedDragListener?) {
        setDropListener(l)
        setDragListener(l)
        setRemoveListener(l)
    }

    fun setDragScrollProfile(ssp: DragScrollProfile?) {
        if (ssp != null) {
            _scrollProfile = ssp
        }
    }

    inner class DragScroller: Runnable {

        private var _abort = false
        private var _prevTime = 0L
        private var _currTime = 0L
        private var _dy = 0
        private var _dt = 0.0f
        private var _tStart = 0L
        private var _scrollDir = 0
        private var _scrollSpeed = 0.0f
        private var _scrolling = false

        constructor() {

        }

        fun isScrolling(): Boolean = _scrolling

        fun getScrollDir(): Int = if (_scrolling) _scrollDir else STOP

        fun startScrolling(dir: Int) {
            if (!_scrolling) {
                _abort = false
                _scrolling = true
                _tStart = SystemClock.uptimeMillis()
                _prevTime = _tStart
                _scrollDir = dir
                post(this)
            }
        }

        fun stopScrolling(now: Boolean) {
            if (now) {
                this@DragListView.removeCallbacks(this)
                _scrolling = false
            } else {
                _abort = true
            }
        }

        override fun run() {
            if (_abort) {
                _scrolling = false
                return
            }
            val first = firstVisiblePosition
            val last = lastVisiblePosition
            val padTop = paddingTop
            val listHeight = height - padTop - paddingBottom
            val minY = Math.min(_y, _floatViewMid + _floatViewHeightHalf)
            val maxY = Math.max(_y, _floatViewMid - _floatViewHeightHalf)
            if (_scrollDir == UP) {
                val v = getChildAt(0)
                if (v == null) {
                    _scrolling = false
                    return
                } else {
                    if (first == 0 && v.top == padTop) {
                        _scrolling = false
                        return
                    }
                }
                _scrollSpeed = _scrollProfile!!.getSpeed((_upScrollStartYF - maxY) / _dragUpScrollHeight, _prevTime)
            } else {
                val v = getChildAt(last - first)
                if (v == null) {
                    _scrolling = false
                    return
                } else {
                    if (last == count - 1 && v.bottom <= listHeight + padTop) {
                        _scrolling = false
                        return
                    }
                }
                _scrollSpeed = -_scrollProfile!!.getSpeed((minY - _downScrollStartYF) / _dragDownScrollHeight, _prevTime)
            }
            _currTime = SystemClock.uptimeMillis()
            _dt = (_currTime - _prevTime).toFloat()
            _dy = Math.round(_scrollSpeed * _dt).toInt()
            var movePos: Int
            if (_dy >= 0) {
                _dy = Math.min(listHeight, _dy)
                movePos = first
            } else {
                _dy = Math.max(-listHeight, _dy)
                movePos = last
            }
            val moveItem = getChildAt(movePos - first)
            var top = moveItem.top + _dy
            if (movePos == 0 && top > padTop) {
                top = padTop
            }
            _blockLayoutRequests = true
            setSelectionFromTop(movePos, top - padTop)
            this@DragListView.layoutChildren()
            invalidate()
            _blockLayoutRequests = false
            doDragFloatView(movePos, moveItem, false)
            _prevTime = _currTime
            post(this)
        }
    }

    private fun doDragFloatView(forceInvalidate: Boolean) {
        val movePos = firstVisiblePosition + childCount / 2
        val moveItem = getChildAt(childCount / 2) ?: return
        doDragFloatView(movePos, moveItem, forceInvalidate)
    }

    private fun doDragFloatView(movePos: Int, moveItem: View?, forceInvalidate: Boolean) {
        _blockLayoutRequests = true
        updateFloatView()
        val oldFirstExpPos = _firstExpPos
        val oldSecondExpPos = _secondExpPos
        val updated = updatePositions()
        if (updated) {
            adjustAllItems()
            val scroll = adjustScroll(movePos, moveItem, oldFirstExpPos, oldSecondExpPos)
            setSelectionFromTop(movePos, moveItem!!.top + scroll - paddingTop)
            layoutChildren()
        }
        if (updated || forceInvalidate) {
            invalidate()
        }
        _blockLayoutRequests = false
    }

    private fun adjustScroll(movePos: Int, moveItem: View?, oldFirstExpPos: Int, oldSecondExpPos: Int): Int {
        var adjust = 0
        val childHeight = getChildHeight(movePos)
        var moveHeightBefore = moveItem!!.getHeight()
        var moveHeightAfter = calcItemHeight(movePos, childHeight)
        var moveBlankBefore = moveHeightBefore
        var moveBlankAfter = moveHeightAfter
        if (movePos != _srcPos) {
            moveBlankBefore -= childHeight
            moveBlankAfter -= childHeight
        }
        var maxBlank = _floatViewHeight
        if (_srcPos != _firstExpPos && _srcPos != _secondExpPos) {
            maxBlank -= _itemHeightCollapsed
        }
        if (movePos <= oldFirstExpPos) {
            if (movePos > _firstExpPos) {
                adjust += maxBlank - moveBlankAfter
            }
        } else if (movePos == oldSecondExpPos) {
            if (movePos <= _firstExpPos) {
                adjust += moveBlankBefore - maxBlank
            } else if (movePos == _secondExpPos) {
                adjust += moveHeightBefore - moveHeightAfter
            } else {
                adjust += moveBlankBefore
            }
        } else {
            if (movePos <= _firstExpPos) {
                adjust -= maxBlank
            } else if (movePos == _secondExpPos) {
                adjust -= moveBlankAfter
            }
        }
        return adjust
    }

    private fun getChildHeight(position: Int): Int {
        if (position == _srcPos) {
            return 0
        }
        var v = getChildAt(position - firstVisiblePosition)
        if (v != null) {
            return getChildHeight(position, v, false)
        } else {
            var childHeight = _childHeightCache.get(position)
            if (childHeight != -1) {
                return childHeight
            }
            val type = adapter.getItemViewType(position)
            val typeCount = adapter.viewTypeCount
            if (typeCount != _sampleViewTypes.size) {
                _sampleViewTypes = arrayOfNulls<View>(typeCount)
            }
            if (type >= 0) {
                if (_sampleViewTypes[type] == null) {
                    v = adapter.getView(position, null, this)
                    _sampleViewTypes[type] = v
                } else {
                    v = adapter.getView(position, _sampleViewTypes[type], this)
                }
            } else {
                v = adapter.getView(position, null, this)
            }
            childHeight = getChildHeight(position, v, true)
            _childHeightCache.add(position, childHeight)
            return childHeight
        }
    }

    private fun getItemHeight(position: Int): Int {
        val v = getChildAt(position - firstVisiblePosition)
        if (v != null) {
            return v.height
        } else {
            return calcItemHeight(position, getChildHeight(position))
        }
    }

    private fun getShuffleEdge(position: Int, top: Int): Int {
        val numHeaders = headerViewsCount
        val numFooters = footerViewsCount
        if (position <= numHeaders || (position >= count - numFooters)) {
            return top
        }
        val divHeight = dividerHeight
        var edge: Int
        val maxBlankHeight = _floatViewHeight - _itemHeightCollapsed
        val childHeight = getChildHeight(position)
        val itemHeight = getItemHeight(position)
        var otop = top
        if (_secondExpPos <= _srcPos) {
            if (position == _secondExpPos && _firstExpPos != _secondExpPos) {
                if (position == _srcPos) {
                    otop = top + itemHeight - _floatViewHeight
                } else {
                    val blankHeight = itemHeight - childHeight
                    otop = top + blankHeight - maxBlankHeight
                }
            } else if (position > _secondExpPos && position <= _srcPos) {
                otop = top - maxBlankHeight
            }
        } else {
            if (position > _srcPos && position <= _firstExpPos) {
                otop = top + maxBlankHeight
            } else if (position == _secondExpPos && _firstExpPos != _secondExpPos) {
                val blankHeight = itemHeight - childHeight
                otop = top + blankHeight
            }
        }
        if (position <= _srcPos) {
            edge = otop + (_floatViewHeight - divHeight - getChildHeight(position - 1)) / 2
        } else {
            edge = otop + (childHeight - divHeight - _floatViewHeight) / 2
        }
        return edge
    }

    private fun updatePositions(): Boolean {
        val first = firstVisiblePosition
        var startPos = _firstExpPos
        var startView = getChildAt(startPos - first)
        if (startView == null) {
            startPos = first + childCount / 2
            startView = getChildAt(startPos - first)
        }
        var startTop = startView.top
        var itemHeight = startView.height
        var edge = getShuffleEdge(startPos, startTop)
        var lastEdge = edge
        var divHeight = dividerHeight
        var itemPos = startPos
        var itemTop = startTop
        if (_floatViewMid < edge) {
            while (itemPos >= 0) {
                itemPos--
                itemHeight = getItemHeight(itemPos)
                if (itemPos == 0) {
                    edge = itemTop - divHeight - itemHeight
                    break
                }
                itemTop -= itemHeight + divHeight
                edge = getShuffleEdge(itemPos, itemTop)
                if (_floatViewMid >= edge) {
                    break
                }
                lastEdge = edge
            }
        } else {
            while (itemPos < count) {
                if (itemPos == count - 1) {
                    edge = itemTop + divHeight + itemHeight
                    break
                }
                itemTop += divHeight + itemHeight
                itemHeight = getItemHeight(itemPos + 1)
                edge = getShuffleEdge(itemPos + 1, itemTop)
                if (_floatViewMid < edge) {
                    break
                }
                lastEdge = edge
                itemPos++
            }
        }
        val numHeaders = headerViewsCount
        val numFooters = footerViewsCount
        var updated = false
        val oldFirstExpPos = _firstExpPos
        val oldSecondExpPos = _secondExpPos
        val oldSlideFrac = _slideFrac
        if (_animate) {
            val edgeToEdge = Math.abs(edge - lastEdge)
            var edgeTop: Int
            var edgeBottom: Int
            if (_floatViewMid < edge) {
                edgeBottom = edge
                edgeTop = lastEdge
            } else {
                edgeTop = edge
                edgeBottom = lastEdge
            }
            val slideRgnHeight = (0.5f * _slideRegionFrac * edgeToEdge).toInt()
            val slideRgnHeightF = slideRgnHeight.toFloat()
            val slideEdgeTop = edgeTop + slideRgnHeight
            val slideEdgeBottom = edgeBottom - slideRgnHeight
            if (_floatViewMid < slideEdgeTop) {
                _firstExpPos = itemPos - 1
                _secondExpPos = itemPos
                _slideFrac = 0.5f * (slideEdgeTop - _floatViewMid) / slideRgnHeightF
            } else if (_floatViewMid < slideEdgeBottom) {
                _firstExpPos = itemPos
                _secondExpPos = itemPos
            } else {
                _firstExpPos = itemPos
                _secondExpPos = itemPos + 1
                _slideFrac = 0.5f * (1.0f + ((edgeBottom - _floatViewMid) * 1.0f / slideRgnHeightF))
            }

        } else {
            _firstExpPos = itemPos
            _secondExpPos = itemPos
        }
        if (_firstExpPos < numHeaders) {
            itemPos = numHeaders
            _firstExpPos = itemPos
            _secondExpPos = itemPos
        } else if (_secondExpPos >= count - numFooters) {
            itemPos = count - numFooters - 1
            _firstExpPos = itemPos
            _secondExpPos = itemPos
        }

        if (_firstExpPos != oldFirstExpPos || _secondExpPos != oldSecondExpPos || _slideFrac != oldSlideFrac) {
            updated = true
        }

        if (itemPos != _floatPos) {
            _dragListener?.drag(_floatPos - numHeaders, itemPos - numHeaders)
            _floatPos = itemPos
            updated = true
        }
        return updated
    }

    private fun updateFloatView() {
        if (_floatViewManager != null) {
            _touchLoc.set(_x, _y)
            _floatViewManager?.onDragFloatView(_floatView, _floatLoc, _touchLoc)
        }
        val floatX = _floatLoc.x
        val floatY = _floatLoc.y
        val padLeft = paddingLeft
        if ((_dragFlags and DRAG_POS_X) == 0 && floatX > padLeft) {
            _floatLoc.x = padLeft
        } else if ((_dragFlags and DRAG_NEG_X) == 0 && floatX < padLeft) {
            _floatLoc.x = padLeft
        }
        val numHeaders = headerViewsCount
        val numFooters = footerViewsCount
        val firstPos = firstVisiblePosition
        val lastPos = lastVisiblePosition
        var topLimit = paddingTop
        if (firstPos < numHeaders) {
            topLimit = getChildAt(numHeaders - firstPos - 1).bottom
        }
        if ((_dragFlags and DRAG_NEG_Y) == 0) {
            if (firstPos <= _srcPos) {
                topLimit = Math.max(getChildAt(_srcPos - firstPos).top, topLimit)
            }
        }
        var bottomLimit = height - paddingBottom
        if (lastPos >= count - numFooters - 1) {
            bottomLimit = getChildAt(count - numFooters - 1 - firstPos).bottom
        }
        if ((_dragFlags and DRAG_POS_Y) == 0) {
            if (lastPos >= _srcPos) {
                bottomLimit = Math.min(getChildAt(_srcPos - firstPos).bottom, bottomLimit)
            }
        }
        if (floatY < topLimit) {
            _floatLoc.y = topLimit
        } else if (floatY + _floatViewHeight > bottomLimit) {
            _floatLoc.y = bottomLimit - _floatViewHeight
        }
        _floatViewMid = _floatLoc.y + _floatViewHeightHalf
    }

    fun stopDragWithVelocity(remove: Boolean, velocityX: Float): Boolean {
        _useRemoveVelocity = true
        return stopDrag(remove, velocityX)
    }

    fun stopDrag(remove: Boolean): Boolean {
        _useRemoveVelocity = false
        return stopDrag(remove, 0.0f)
    }

    fun stopDrag(remove: Boolean, velocityX: Float): Boolean {
        if (_floatView != null) {
            _dragScroller?.stopScrolling(true)
            if (remove) {
                removeItem(_srcPos - headerViewsCount, velocityX)
            } else {
                if (_dropAnimator != null) {
                    _dropAnimator?.start()
                } else {
                    dropFloatView()
                }
            }
            return true
        } else {
            return false
        }
    }

    private fun dropFloatView() {
        _dragState = DROPPING
        if (_dropListener != null && _floatPos >= 0 && _floatPos < count) {
            val numHeaders = headerViewsCount
            _dropListener?.drop(_srcPos - numHeaders, _floatPos - numHeaders)
        }
        destroyFloatView()
        adjustOnReorder()
        clearPositions()
        adjustAllItems()
        if (_inTouchEvent) {
            _dragState = STOPPED
        } else {
            _dragState = IDLE
        }
    }

    private fun adjustOnReorder() {
        val firstPos = firstVisiblePosition
        if (_srcPos < firstPos) {
            val v = getChildAt(0)
            var top = 0
            if (v != null) {
                top = v.top
            }
            setSelectionFromTop(firstPos - 1, top - paddingTop)
        }
    }

    fun removeItem(which: Int) {
        _useRemoveVelocity = false
        removeItem(which, 0.0f)
    }

    fun removeItem(which: Int, velocityX: Float) {
        if (_dragState == IDLE || _dragState == DRAGGING) {
            if (_dragState == IDLE) {
                _srcPos = headerViewsCount + which
                _firstExpPos = _srcPos
                _secondExpPos = _srcPos
                _floatPos = _srcPos
                getChildAt(_srcPos - firstVisiblePosition)?.visibility = View.INVISIBLE
            }
            _dragState = REMOVING
            _removeVelocityX = velocityX
            if (_inTouchEvent) {
                when (_cancelMethod) {
                    ON_TOUCH_EVENT -> super.onTouchEvent(_cancelEvent)
                    ON_INTERCEPT_TOUCH_EVENT -> super.onInterceptTouchEvent(_cancelEvent)
                }
            }
            if (_removeAnimator != null) {
                _removeAnimator?.start()
            } else {
                doRemoveItem(which)
            }
        }
    }

    private fun doRemoveItem() {
        doRemoveItem(_srcPos - headerViewsCount)
    }

    private fun doRemoveItem(which: Int) {
        _dragState = REMOVING
        _removeListener?.remove(which)
        destroyFloatView()
        adjustOnReorder()
        clearPositions()
        if (_inTouchEvent) {
            _dragState = STOPPED
        } else {
            _dragState = IDLE
        }
    }

    fun startDrag(position: Int, dragFlags: Int, deltaX: Int, deltaY: Int): Boolean {
        if (!_inTouchEvent || _floatViewManager == null) {
            return false
        }
        val v = _floatViewManager?.onCreateFloatView(position)
        if (v == null) {
            return false
        } else {
            return startDrag(position, v, dragFlags, deltaX, deltaY)
        }
    }

    fun startDrag(position: Int, floatView: View?, dragFlags: Int, deltaX: Int, deltaY: Int): Boolean {
        if (_dragState != IDLE || !_inTouchEvent || _floatView != null || floatView == null || !_dragEnabled) {
            return false
        }
        if (parent != null) {
            parent.requestDisallowInterceptTouchEvent(true)
        }
        val pos = position +headerViewsCount
        _firstExpPos = pos
        _secondExpPos = pos
        _srcPos = pos
        _floatPos = pos
        _dragState = DRAGGING
        _dragFlags = 0
        _dragFlags =_dragFlags or dragFlags
        _floatView = floatView
        measureFloatView()
        _dragDeltaX = deltaX
        _dragDeltaY = deltaY
        _floatLoc.x = _x - _dragDeltaX
        _floatLoc.y = _y - _dragDeltaY
        val srcItem = getChildAt(_srcPos - firstVisiblePosition)
        srcItem?.visibility = View.INVISIBLE
        when (_cancelMethod) {
            ON_TOUCH_EVENT -> super.onTouchEvent(_cancelEvent)
            ON_INTERCEPT_TOUCH_EVENT -> super.onInterceptTouchEvent(_cancelEvent)
        }
        requestLayout()
        _liftAnimator?.start()
        return true
    }

    private fun measureFloatView() {
        if (_floatView != null) {
            measureItem(_floatView)
            _floatViewHeight = _floatView!!.measuredHeight
            _floatViewHeightHalf = _floatViewHeight / 2
        }
    }

    fun isDragEnabled(): Boolean = _dragEnabled

    fun setDragEnabled(enabled: Boolean) {
        _dragEnabled = enabled
    }

    fun listViewIntercepted(): Boolean = _listViewIntercepted

    override fun setAdapter(adapter: ListAdapter?) {
        if (adapter != null) {
            _adapterWrapper = AdapterWrapper(adapter)
            adapter.registerDataSetObserver(_observer)
            if (adapter is DropListener) {
                setDropListener(adapter)
            }
            if (adapter is DragListener) {
                setDragListener(adapter)
            }
            if (adapter is RemoveListener) {
                setRemoveListener(adapter)
            }
        } else {
            _adapterWrapper = null
        }
        super.setAdapter(_adapterWrapper)
    }

    fun setDragListener(l: DragListener?) {
        _dragListener = l
    }

    fun setDropListener(l: DropListener?) {
        _dropListener = l
    }

    fun setRemoveListener(l: RemoveListener?) {
        _removeListener = l
    }

    override fun onTouchEvent(ev: MotionEvent?): Boolean {
        if (_ignoreTouchEvent) {
            _ignoreTouchEvent = false
            return false
        }
        if (!_dragEnabled) {
            return super.onTouchEvent(ev)
        }
        var more = false
        var lastCallWasIntercept = _lastCallWasIntercept
        _lastCallWasIntercept = false
        if (!lastCallWasIntercept) {
            saveTouchCoords(ev)
        }
        if (_dragState == DRAGGING) {
            onDragTouchEvent(ev)
            more = true
        } else {
            if (_dragState == IDLE) {
                if (super.onTouchEvent(ev)) {
                    more = true
                }
            }
            val action = ev!!.action and MotionEvent.ACTION_MASK
            when (action) {
                MotionEvent.ACTION_CANCEL,
                MotionEvent.ACTION_UP -> doActionUpOrCancel()
                else -> if (more) { _cancelMethod = ON_TOUCH_EVENT }
            }
        }
        return more
    }

    private fun saveTouchCoords(ev: MotionEvent?) {
        val action = ev!!.action and MotionEvent.ACTION_MASK
        if (action != MotionEvent.ACTION_DOWN) {
            _lastY = _y
        }
        _x = ev.x.toInt()
        _y = ev.y.toInt()
        if (action == MotionEvent.ACTION_DOWN) {
            _lastY = _y
        }
    }

    protected fun onDragTouchEvent(ev: MotionEvent?): Boolean {
        when (ev!!.action and MotionEvent.ACTION_MASK) {
            MotionEvent.ACTION_CANCEL -> {
                if (_dragState == DRAGGING) {
                    cancelDrag()
                }
                doActionUpOrCancel()
            }
            MotionEvent.ACTION_UP -> {
                if (_dragState == DRAGGING) {
                    stopDrag(false)
                }
                doActionUpOrCancel()
            }
            MotionEvent.ACTION_MOVE -> {
                continueDrag(ev.x.toInt(), ev.y.toInt())
            }
        }
        return true
    }

    private fun doActionUpOrCancel() {
        _cancelMethod = NO_CANCEL
        _inTouchEvent = false
        if (_dragState == STOPPED) {
            _dragState = IDLE
        }
        _currFloatAlpha = _floatAlpha
        _listViewIntercepted = false
        _childHeightCache.clear()
    }

    private fun continueDrag(x: Int, y: Int) {
        _floatLoc.x = x - _dragDeltaX
        _floatLoc.y = y - _dragDeltaY
        doDragFloatView(true)
        val minY = Math.min(y, _floatViewMid + _floatViewHeightHalf)
        val maxY = Math.max(y, _floatViewMid - _floatViewHeightHalf)
        val currentScrollDir = _dragScroller!!.getScrollDir()
        if (minY > _lastY && minY > _downScrollStartY && currentScrollDir != DOWN) {
            if (currentScrollDir != STOP) {
                _dragScroller?.stopScrolling(true)
            }
            _dragScroller?.startScrolling(DOWN)
        } else if (maxY < _lastY && maxY < _upScrollStartY && currentScrollDir != UP) {
            if (currentScrollDir != STOP) {
                _dragScroller?.stopScrolling(true)
            }
            _dragScroller?.startScrolling(UP)
        } else if (maxY >= _upScrollStartY && minY <= _downScrollStartY && _dragScroller!!.isScrolling()) {
            _dragScroller?.stopScrolling(true)
        }
    }

    override fun onDraw(canvas: Canvas?) {
        super.onDraw(canvas)
    }

    override fun onInterceptTouchEvent(ev: MotionEvent?): Boolean {
        if (!_dragEnabled) {
            return super.onInterceptTouchEvent(ev)
        }
        saveTouchCoords(ev)
        _lastCallWasIntercept = true
        val action = ev!!.action and MotionEvent.ACTION_MASK
        if (action == MotionEvent.ACTION_DOWN) {
            if (_dragState != IDLE) {
                _ignoreTouchEvent = true
                return true
            }
            _inTouchEvent = true
        }
        var intercept = false
        if (_floatView != null) {
            intercept = true
        } else {
            if (super.onInterceptTouchEvent(ev)) {
                _listViewIntercepted = true
                intercept = true
            }
            when (action) {
                MotionEvent.ACTION_CANCEL,
                MotionEvent.ACTION_UP -> doActionUpOrCancel()
                else -> {
                    if (intercept) {
                        _cancelMethod = ON_TOUCH_EVENT
                    } else {
                        _cancelMethod = ON_INTERCEPT_TOUCH_EVENT
                    }
                }
            }
        }
        if (action == MotionEvent.ACTION_UP || action == MotionEvent.ACTION_CANCEL) {
            _inTouchEvent = false
        }
        return intercept
    }

    override fun onSizeChanged(w: Int, h: Int, oldw: Int, oldh: Int) {
        super.onSizeChanged(w, h, oldw, oldh)
        updateScrollStarts()
    }

    override fun requestLayout() {
        if (!_blockLayoutRequests) {
            super.requestLayout()
        }
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec)
        if (_floatView != null) {
            if (_floatView!!.isLayoutRequested) {
                measureFloatView()
            }
            _floatViewOnMeasured = true
        }
        _widthMeasureSpec = widthMeasureSpec
    }


    override fun layoutChildren() {
        super.layoutChildren()
        if (_floatView != null) {
            if (_floatView!!.isLayoutRequested && !_floatViewOnMeasured) {
                measureFloatView()
            }
            _floatView?.layout(0, 0, _floatView!!.measuredWidth, _floatView!!.measuredHeight)
            _floatViewOnMeasured = false
        }
    }

    override fun dispatchDraw(canvas: Canvas?) {
        super.dispatchDraw(canvas)
        if (_dragState != IDLE) {
            if (_firstExpPos != _srcPos) {
                drawDivider(_firstExpPos, canvas)
            }
            if (_secondExpPos != _firstExpPos && _secondExpPos != _srcPos) {
                drawDivider(_secondExpPos, canvas)
            }
        }
        if (_floatView != null) {
            val w = _floatView!!.width
            val h = _floatView!!.height
            var x = _floatLoc.x
            val width = this.width
            if (x < 0) {
                x = -x
            }
            var alphaMod: Float
            if (x < width) {
                alphaMod = (width - x) * 1.0f / width
                alphaMod *= alphaMod
            } else {
                alphaMod = 0.0f
            }
            val alpha = (255f * _currFloatAlpha * alphaMod).toInt()
            canvas?.save()
            canvas?.translate(_floatLoc.x.toFloat(), _floatLoc.y.toFloat())
            canvas?.clipRect(0, 0, w, h)
            canvas?.saveLayerAlpha(0.0f, 0.0f, w.toFloat(), h.toFloat(), alpha, Canvas.ALL_SAVE_FLAG)
            _floatView?.draw(canvas)
            canvas?.restore()
            canvas?.restore()
        }
    }

    private fun drawDivider(expPosition: Int, canvas: Canvas?) {
        if (divider != null && dividerHeight != 0) {
            val expItem = getChildAt(expPosition - firstVisiblePosition) as ViewGroup?
            if (expItem != null) {
                val l = paddingLeft
                val r = width - paddingRight
                var t: Int
                var b: Int
                val childHeight = expItem.getChildAt(0).height
                if (expPosition > _srcPos) {
                    t = expItem.top + childHeight
                    b = t + dividerHeight
                } else {
                    b = expItem.bottom - childHeight
                    t = b - dividerHeight
                }
                canvas?.save()
                canvas?.clipRect(l, t, r, b)
                divider.setBounds(l, t, r, b)
                divider.draw(canvas)
                canvas?.restore()
            }
        }
    }

    fun moveItem(from: Int, to: Int) {
        if (_dropListener != null) {
            val count = getInputAdapter()!!.count
            if (from >= 0 && from < count && to >= 0 && to < count) {
                _dropListener?.drop(from, to)
            }
        }
    }

    fun moveCheckState(from: Int, to: Int) {
        val cip = checkedItemPositions
        var rangeStart = from
        var rangeEnd = to
        if (to < from) {
            rangeStart = to
            rangeEnd = from
        }
        rangeEnd += 1
        val runStart = IntArray(cip.size())
        val runEnd = IntArray(cip.size())
        val runCount = buildRunList(cip, rangeStart, rangeEnd, runStart, runEnd)
        if (runCount == 1 && (runStart[0] == runEnd[0])) {
            return
        }
        if (from < to) {
            for (i in 0..runCount - 1) {
                setItemChecked(rotate(runStart[i], -1, rangeStart, rangeEnd), true)
                setItemChecked(rotate(runEnd[i], -1, rangeStart, rangeEnd), false)
            }
        } else {
            for (i in 0..runCount - 1) {
                setItemChecked(runStart[i], false)
                setItemChecked(runEnd[i], true)
            }
        }
    }

    fun removeCheckState(position: Int) {
        val cip = checkedItemPositions
        if (cip.size() == 0) {
            return
        }
        val runStart = IntArray(cip.size())
        val runEnd = IntArray(cip.size())
        val rangeStart = position
        val rangeEnd = cip.keyAt(cip.size() - 1) + 1
        val runCount = buildRunList(cip, rangeStart, rangeEnd, runStart, runEnd)
        for (i in 0..runCount - 1) {
            if (!(runStart[i] == position || (runEnd[i] < runStart[i] && runEnd[i] > position))) {
                setItemChecked(rotate(runStart[i], -1, rangeStart, rangeEnd), true)
            }
            setItemChecked(rotate(runEnd[i], -1, rangeStart, rangeEnd), false)
        }
    }



    fun getInputAdapter(): ListAdapter? {
        if (_adapterWrapper == null) {
            return null
        } else {
            return _adapterWrapper?.getAdapter()
        }
    }

    inner class AdapterWrapper: BaseAdapter {

        private var _adapter: ListAdapter? = null

        constructor(adapter: ListAdapter?): super() {
            _adapter = adapter
            _adapter?.registerDataSetObserver(object: DataSetObserver() {
                override fun onChanged() {
                    notifyDataSetChanged()
                }
                override fun onInvalidated() {
                    notifyDataSetInvalidated()
                }
            })
        }

        fun getAdapter(): ListAdapter? = _adapter

        override fun getItemId(position: Int): Long = _adapter!!.getItemId(position)

        override fun getItem(position: Int): Any? = _adapter?.getItem(position)

        override fun getCount(): Int = _adapter!!.count

        override fun areAllItemsEnabled(): Boolean = _adapter!!.areAllItemsEnabled()

        override fun isEnabled(position: Int): Boolean = _adapter!!.isEnabled(position)

        override fun getItemViewType(position: Int): Int = _adapter!!.getItemViewType(position)

        override fun getViewTypeCount(): Int = _adapter!!.viewTypeCount

        override fun hasStableIds(): Boolean = _adapter!!.hasStableIds()

        override fun isEmpty(): Boolean = _adapter!!.isEmpty

        override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
            var v: DragItemView
            var child: View
            if (convertView != null) {
                v = convertView as DragItemView
                val oldChild = v.getChildAt(0)
                child = _adapter!!.getView(position, oldChild, this@DragListView)
                if (child != oldChild) {
                    if (oldChild != null) {
                        v.removeViewAt(0)
                    }
                    v.addView(child)
                }
            } else {
                child = _adapter!!.getView(position, null, this@DragListView)
                v = DragItemView(context)
                v.layoutParams = AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT)
                v.addView(child)
            }
            adjustItem(position + headerViewsCount, v, true)
            return v
        }
    }

    inner open class SmoothAnimator: Runnable {

        protected var startTime = 0L
        private var _durationF = 0.0f
        private var _alpha = 0.0f
        private var _a = 0.0f
        private var _b = 0.0f
        private var _c = 0.0f
        private var _d = 0.0f
        private var _canceled = false

        constructor(smoothness: Float, duration: Int) {
            _alpha = smoothness
            _durationF = duration.toFloat()
            _a = 1.0f / (2.0f * _alpha * (1.0f - _alpha))
            _d = _a
            _b = _alpha / (2.0f * (_alpha - 1.0f))
            _d = 1.0f / (1.0f - _alpha)
        }

        fun transform(frac: Float): Float {
            if (frac < _alpha) {
                return _a * frac * frac
            } else if (frac < 1.0f - _alpha) {
                return _b + _c * frac
            } else {
                return 1.0f - _d * (frac - 1.0f) * (frac - 1.0f)
            }
        }

        fun start() {
            startTime = SystemClock.uptimeMillis()
            _canceled = false
            onStart()
            post(this)
        }

        fun cancel() {
            _canceled = true
        }

        open fun onStart() { }

        open fun onUpdate(frac: Float, smoothFrac: Float) { }

        open fun onStop() { }

        override fun run() {
            if (_canceled) {
                return
            }
            val fraction = (SystemClock.uptimeMillis() - startTime) * 1.0f / _durationF
            if (fraction >= 1.0f) {
                onUpdate(1.0f, 1.0f)
                onStop()
            } else {
                onUpdate(fraction, transform(fraction))
                post(this)
            }
        }
    }


    inner class RemoveAnimator: SmoothAnimator {

        private var _floatLocX = 0.0f
        private var _firstStartBlank = 0.0f
        private var _secondStartBlank = 0.0f
        private var _firstChildHeight = -1
        private var _secondChildHeight = -1
        private var _firstPos = 0
        private var _secondPos = 0

        constructor(smoothness: Float, duration: Int): super(smoothness, duration)

        override fun onStart() {
            _firstChildHeight = -1
            _secondChildHeight = -1
            _firstPos = _firstExpPos
            _secondPos = _secondExpPos
            _dragState = REMOVING
            _floatLocX = _floatLoc.x.toFloat()
            if (_useRemoveVelocity) {
                var minVelocity = 2.0f * width
                if (_removeVelocityX.toInt() == 0) {
                    _removeVelocityX = (if (_floatLocX < 0) -1 else 1) * minVelocity
                } else {
                    minVelocity *= 2
                    if (_removeVelocityX < 0 && _removeVelocityX > -minVelocity) {
                        _removeVelocityX = -minVelocity
                    } else if (_removeVelocityX > 0 && _removeVelocityX < minVelocity) {
                        _removeVelocityX = minVelocity
                    }
                }
            } else {
                destroyFloatView()
            }
        }

        override fun onUpdate(frac: Float, smoothFrac: Float) {
            val f = 1.0f - smoothFrac
            val firstVis = firstVisiblePosition
            var item = getChildAt(_firstPos - firstVis)
            var lp: ViewGroup.LayoutParams
            var blank: Int
            if (_useRemoveVelocity) {
                val dt = (SystemClock.uptimeMillis() - startTime) * 1.0f / 1000
                if (dt.toInt() == 0) {
                    return
                }
                val dx = _removeVelocityX * dt
                val w = width
                _removeVelocityX += (if (_removeVelocityX > 0) 1 else -1) * dt * w
                _floatLocX += dx
                _floatLoc.x = _floatLocX.toInt()
                if (_floatLocX < w && _floatLocX > -w) {
                    startTime = SystemClock.uptimeMillis()
                    doDragFloatView(true)
                    return
                }
            }
            if (item != null) {
                if (_firstChildHeight == -1) {
                    _firstChildHeight = getChildHeight(_firstPos, item, false)
                    _firstStartBlank = (item.height - _firstChildHeight).toFloat()
                }
                blank = Math.max((f * _firstStartBlank).toInt(), 1)
                lp = item.layoutParams
                lp.height = _firstChildHeight + blank
                item.layoutParams = lp
            }
            if (_secondPos != _firstPos) {
                item = getChildAt(_secondPos - firstVis)
                if (item != null) {
                    if (_secondChildHeight == -1) {
                        _secondChildHeight = getChildHeight(_secondPos, item, false)
                        _secondStartBlank = (item.height - _secondChildHeight).toFloat()
                    }
                    blank = Math.max((f * _secondStartBlank).toInt(), 1)
                    lp = item.layoutParams
                    lp.height = _secondChildHeight + blank
                    item.layoutParams = lp
                }
            }
        }

        override fun onStop() {
            doRemoveItem()
        }
    }

    inner class LiftAnimator: SmoothAnimator {

        private var _initDragDeltaY = 0.0f
        private var _finalDragDeltaY = 0.0f

        constructor(smoothness: Float, duration: Int): super(smoothness, duration)

        override fun onStart() {
            _initDragDeltaY = _dragDeltaY.toFloat()
            _finalDragDeltaY = _floatViewHeightHalf.toFloat()
        }

        override fun onUpdate(frac: Float, smoothFrac: Float) {
            if (_dragState != DRAGGING) {
                cancel()
            } else {
                _dragDeltaY = (smoothFrac * _finalDragDeltaY + (1f - smoothFrac) * _initDragDeltaY).toInt()
                _floatLoc.y = _y - _dragDeltaY
                doDragFloatView(true)
            }
        }
    }


    inner class DropAnimator: SmoothAnimator {

        private var _dropPos = 0
        private var _srcPos = 0
        private var _initDeltaY = 0.0f
        private var _initDeltaX = 0.0f

        constructor(smoothness: Float, duration: Int): super(smoothness, duration)

        override fun onStart() {
            _dropPos = _floatPos
            _srcPos = this@DragListView._srcPos
            _dragState = DROPPING
            _initDeltaY = (_floatLoc.y - getTargetY()).toFloat()
            _initDeltaX = (_floatLoc.x - paddingLeft).toFloat()
        }

        override fun onUpdate(frac: Float, smoothFrac: Float) {
            val targetY = getTargetY().toFloat()
            val targetX = paddingLeft.toFloat()
            val deltaY = (_floatLoc.y - targetY).toFloat()
            val deltaX = (_floatLoc.x - targetX).toFloat()
            val f = 1.0f - smoothFrac
            if (f < Math.abs(deltaY / _initDeltaY) || f < Math.abs(deltaX / _initDeltaX)) {
                _floatLoc.y = (targetY + (_initDeltaY * f)).toInt()
                _floatLoc.x = (paddingLeft +(_initDeltaX * f)).toInt()
                doDragFloatView(true)
            }
        }

        override fun onStop() {
            dropFloatView()
        }

        private fun getTargetY(): Int {
            val first = firstVisiblePosition
            val otherAdjust = (_itemHeightCollapsed + dividerHeight) / 2
            val v = getChildAt(_dropPos - first)
            var targetY = -1
            if (v != null) {
                if (_dropPos == _srcPos) {
                    targetY = v.top
                } else if (_dropPos < _srcPos) {
                    targetY = v.top - otherAdjust
                } else {
                    targetY = v.bottom + otherAdjust - _floatViewHeight
                }
            } else {
                cancel()
            }
            return targetY
        }
    }

}