package com.hujiang.devart.component.recycler

import android.content.Context
import android.graphics.PointF
import android.os.Parcel
import android.os.Parcelable
import android.support.v4.view.ViewCompat
import android.support.v4.view.accessibility.AccessibilityEventCompat
import android.util.AttributeSet
import android.view.View
import android.view.ViewGroup
import android.view.accessibility.AccessibilityEvent

/**
 * Created by rarnu on 4/20/16.
 */
class LinearLayoutManager: RecyclerView.LayoutManager, ItemTouchHelper.ViewDropHandler {


    companion object {
        val HORIZONTAL = OrientationHelper.HORIZONTAL
        val VERTICAL = OrientationHelper.VERTICAL
        val INVALID_OFFSET = Integer.MIN_VALUE
        val MAX_SCROLL_FACTOR = 0.33f
    }

    var _orientation = 0
    var _layoutState: LayoutState? = null
    var _orientationHelper: OrientationHelper? = null
    var _lastStackFromEnd = false
    var _reverseLayout = false
    var _shouldReverseLayout = false
    var _stackFromEnd = false
    var _smoothScrollbarEnabled = true
    var _pendingScrollPosition = RecyclerView.NO_POSITION
    var _pendingScrollPositionOffset = INVALID_OFFSET
    var _recycleChildrenOnDetach = false
    var _pendingSavedState: SavedState? = null
    var _anchorInfo = AnchorInfo()

    constructor(context: Context): this(context, VERTICAL, false)
    constructor(context: Context, orientation: Int, reverseLayout: Boolean) {
        setOrientation(orientation)
        setReverseLayout(reverseLayout)
    }
    constructor(context: Context, attrs: AttributeSet?, defStyleAttr: Int, defStyleRes: Int) {
        val properties = getProperties(context, attrs, defStyleAttr, defStyleRes)
        setOrientation(properties!!.orientation)
        setReverseLayout(properties.reverseLayout)
        setStackFromEnd(properties.stackFromEnd)
    }

    fun setStackFromEnd(stackFromEnd: Boolean) {
        assertNotInLayoutOrScroll(null)
        if (_stackFromEnd == stackFromEnd) {
            return
        }
        _stackFromEnd = stackFromEnd
        requestLayout()
    }

    fun setReverseLayout(reverseLayout: Boolean) {
        assertNotInLayoutOrScroll(null)
        if (reverseLayout == _reverseLayout) {
            return
        }
        _reverseLayout = reverseLayout
        requestLayout()
    }

    fun setOrientation(orientation: Int) {
        if (orientation != HORIZONTAL && orientation != VERTICAL) {
            throw IllegalArgumentException("invalid orientation:${orientation}")
        }
        assertNotInLayoutOrScroll(null)
        if (orientation == _orientation) {
            return
        }
        _orientation = orientation
        _orientationHelper = null
        requestLayout()
    }

    class LayoutChunkResult {
        var _consumed = 0
        var _finished = false
        var _ignoreConsumed = false
        var _focusable = false

        fun resetInternal() {
            _consumed = 0
            _finished = false
            _ignoreConsumed = false
            _focusable = false
        }
    }

    class LayoutState {
        companion object {
            val LAYOUT_START = -1
            val LAYOUT_END = 1
            val INVALID_LAYOUT = Integer.MIN_VALUE
            val ITEM_DIRECTION_HEAD = -1
            val ITEM_DIRECTION_TAIL = 1
            val SCOLLING_OFFSET_NaN = Integer.MIN_VALUE
        }

        var _recycle = true;
        var _offset = 0
        var _available = 0
        var _currentPosition = 0
        var _itemDirection = 0
        var _layoutDirection = 0
        var _scrollingOffset = 0
        var _extra = 0
        var _isPreLayout = false
        var _lastScrollDelta = 0
        var _scrapList: MutableList<RecyclerView.ViewHolder?>? = null

        fun hasMore(state: RecyclerView.State?): Boolean = _currentPosition >= 0 && _currentPosition < state!!.getItemCount()

        fun next(recycler: RecyclerView.Recycler?): View? {
            if (_scrapList != null) {
                return nextViewFromScrapList()
            }
            val view = recycler?.getViewForPosition(_currentPosition)
            _currentPosition += _itemDirection
            return view
        }

        fun nextViewFromScrapList(): View? {
            val size = _scrapList!!.size
            for (i in 0..size - 1) {
                val view = _scrapList?.get(i)?._itemView
                val lp = view?.getLayoutParams() as RecyclerView.LayoutParams
                if (lp.isItemRemoved()) {
                    continue
                }
                if (_currentPosition == lp.getViewLayoutPosition()) {
                    assignPositionFromScrapList(view)
                    return view
                }
            }
            return null;
        }

        fun assignPositionFromScrapList() = assignPositionFromScrapList(null)

        fun assignPositionFromScrapList(ignore: View?) {
            val closest = nextViewInLimitedList(ignore)
            if (closest == null) {
                _currentPosition = RecyclerView.NO_POSITION
            } else {
                _currentPosition = (closest.getLayoutParams() as RecyclerView.LayoutParams).getViewLayoutPosition()
            }
        }

        fun nextViewInLimitedList(ignore: View?): View? {
            val size = _scrapList!!.size
            var closest: View? = null
            var closestDistance = Integer.MAX_VALUE
            for (i in 0..size - 1) {
                val view = _scrapList?.get(i)?._itemView
                val lp = view?.getLayoutParams() as RecyclerView.LayoutParams
                if (view == ignore || lp.isItemRemoved()) {
                    continue
                }
                val distance = (lp.getViewLayoutPosition() - _currentPosition) * _itemDirection
                if (distance < 0) {
                    continue
                }
                if (distance < closestDistance) {
                    closest = view
                    closestDistance = distance
                    if (distance == 0) {
                        break
                    }
                }
            }
            return closest
        }
    }

    class SavedState: Parcelable {
        var _anchorPosition = 0
        var _anchorOffset = 0
        var _anchorLayoutFromEnd = false

        constructor()
        constructor(src: Parcel?) {
            _anchorPosition = src!!.readInt()
            _anchorOffset = src.readInt()
            _anchorLayoutFromEnd = src.readInt() == 1
        }

        constructor(other: SavedState?) {
            _anchorPosition = other!!._anchorPosition
            _anchorOffset = other._anchorOffset
            _anchorLayoutFromEnd = other._anchorLayoutFromEnd
        }

        fun hasValidAnchor(): Boolean = _anchorPosition >= 0

        fun invalidateAnchor() {
            _anchorPosition = RecyclerView.NO_POSITION
        }

        override fun describeContents(): Int = 0

        override fun writeToParcel(dest: Parcel?, flags: Int) {
            dest?.writeInt(_anchorPosition)
            dest?.writeInt(_anchorOffset)
            dest?.writeInt(if (_anchorLayoutFromEnd) 1 else 0)
        }

        companion object {
            val CREATOR= object: Parcelable.Creator<SavedState> {
                override fun createFromParcel(source: Parcel?): SavedState? = SavedState(source)
                override fun newArray(size: Int): Array<SavedState?>? = arrayOfNulls<SavedState>(size)
            }
        }
    }


    inner class AnchorInfo {
        var _position = 0
        var _coordinate = 0
        var _layoutFromEnd = false

        fun reset() {
            _position = RecyclerView.NO_POSITION
            _coordinate = INVALID_OFFSET
            _layoutFromEnd = false
        }

        fun assignCoordinateFromPadding() {
            _coordinate = if (_layoutFromEnd) _orientationHelper!!.getEndAfterPadding() else _orientationHelper!!.getStartAfterPadding()
        }

        override fun toString(): String = "AnchorInfo{" + "position=${_position}, coordinate=${_coordinate}, layoutFromEnd=${_layoutFromEnd}}"

        fun isViewValidAsAnchor(child: View?, state: RecyclerView.State?): Boolean {
            val lp = child?.getLayoutParams() as RecyclerView.LayoutParams
            return !lp.isItemRemoved() && lp.getViewLayoutPosition() >= 0 && lp.getViewLayoutPosition() < state!!.getItemCount()
        }

        fun assignFromViewAndKeepVisibleRect(child: View?) {
            val spaceChange = _orientationHelper!!.getTotalSpaceChange()
            if (spaceChange >= 0) {
                assignFromView(child)
                return;
            }
            _position = getPosition(child)
            if (_layoutFromEnd) {
                val prevLayoutEnd = _orientationHelper!!.getEndAfterPadding() - spaceChange
                val childEnd = _orientationHelper!!.getDecoratedEnd(child)
                val previousEndMargin = prevLayoutEnd - childEnd
                _coordinate = _orientationHelper!!.getEndAfterPadding() - previousEndMargin
                if (previousEndMargin > 0) {
                    val childSize = _orientationHelper!!.getDecoratedMeasurement(child)
                    val estimatedChildStart = _coordinate - childSize
                    val layoutStart = _orientationHelper!!.getStartAfterPadding()
                    val previousStartMargin = _orientationHelper!!.getDecoratedStart(child) - layoutStart
                    val startReference = layoutStart + Math.min(previousStartMargin, 0)
                    val startMargin = estimatedChildStart - startReference
                    if (startMargin < 0) {
                        _coordinate += Math.min(previousEndMargin, -startMargin)
                    }
                }
            } else {
                val childStart = _orientationHelper!!.getDecoratedStart(child)
                val startMargin = childStart - _orientationHelper!!.getStartAfterPadding()
                _coordinate = childStart
                if (startMargin > 0) {
                    val estimatedEnd = childStart + _orientationHelper!!.getDecoratedMeasurement(child)
                    val previousLayoutEnd = _orientationHelper!!.getEndAfterPadding() - spaceChange
                    val previousEndMargin = previousLayoutEnd - _orientationHelper!!.getDecoratedEnd(child)
                    val endReference = _orientationHelper!!.getEndAfterPadding() - Math.min(0, previousEndMargin)
                    val endMargin = endReference - estimatedEnd
                    if (endMargin < 0) {
                        _coordinate -= Math.min(startMargin, -endMargin)
                    }
                }
            }
        }

        fun assignFromView(child: View?) {
            if (_layoutFromEnd) {
                _coordinate = _orientationHelper!!.getDecoratedEnd(child) + _orientationHelper!!.getTotalSpaceChange()
            } else {
                _coordinate = _orientationHelper!!.getDecoratedStart(child)
            }
            _position = getPosition(child)
        }
    }

    override fun generateDefaultLayoutParams(): RecyclerView.LayoutParams? = RecyclerView.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT)

    override fun prepareForDrop(view: View?, target: View?, x: Int, y: Int) {
        assertNotInLayoutOrScroll("Cannot drop a view during a scroll or layout calculation")
        ensureLayoutState()
        resolveShouldLayoutReverse()
        val myPos = getPosition(view)
        val targetPos = getPosition(target)
        val dropDirection = if (myPos < targetPos) LayoutState.ITEM_DIRECTION_TAIL else LayoutState.ITEM_DIRECTION_HEAD
        if (_shouldReverseLayout) {
            if (dropDirection == LayoutState.ITEM_DIRECTION_TAIL) {
                scrollToPositionWithOffset(targetPos, _orientationHelper!!.getEndAfterPadding() - (_orientationHelper!!.getDecoratedStart(target) + _orientationHelper!!.getDecoratedMeasurement(view)))
            } else {
                scrollToPositionWithOffset(targetPos, _orientationHelper!!.getEndAfterPadding() - _orientationHelper!!.getDecoratedEnd(target))
            }
        } else {
            if (dropDirection == LayoutState.ITEM_DIRECTION_HEAD) {
                scrollToPositionWithOffset(targetPos, _orientationHelper!!.getDecoratedStart(target))
            } else {
                scrollToPositionWithOffset(targetPos, _orientationHelper!!.getDecoratedEnd(target) - _orientationHelper!!.getDecoratedMeasurement(view))
            }
        }
    }

    fun scrollToPositionWithOffset(position: Int, offset: Int) {
        _pendingScrollPosition = position
        _pendingScrollPositionOffset = offset
        _pendingSavedState?.invalidateAnchor()
        requestLayout()
    }

    fun resolveShouldLayoutReverse() {
        if (_orientation == VERTICAL || !isLayoutRTL()) {
            _shouldReverseLayout = _reverseLayout
        } else {
            _shouldReverseLayout = !_reverseLayout
        }
    }

    fun isLayoutRTL(): Boolean = getLayoutDirection() == ViewCompat.LAYOUT_DIRECTION_RTL

    fun ensureLayoutState() {
        if (_layoutState == null) {
            _layoutState = createLayoutState()
        }
        if (_orientationHelper == null) {
            _orientationHelper = OrientationHelper.createOrientationHelper(this, _orientation)
        }
    }

    fun createLayoutState(): LayoutState = LayoutState()

    override fun onDetachedFromWindow(view: RecyclerView?, recycler: RecyclerView.Recycler?) {
        super.onDetachedFromWindow(view, recycler);
        if (_recycleChildrenOnDetach) {
            removeAndRecycleAllViews(recycler)
            recycler?.clear()
        }
    }

    override fun onInitializeAccessibilityEvent(event: AccessibilityEvent?) {
        super.onInitializeAccessibilityEvent(event)
        if (getChildCount() > 0) {
            val record = AccessibilityEventCompat.asRecord(event)
            record.setFromIndex(findFirstVisibleItemPosition())
            record.setToIndex(findLastVisibleItemPosition())
        }
    }

    fun findOneVisibleChild(fromIndex: Int, toIndex: Int, completelyVisible: Boolean, acceptPartiallyVisible: Boolean): View? {
        ensureLayoutState()
        val start = _orientationHelper!!.getStartAfterPadding()
        val end = _orientationHelper!!.getEndAfterPadding()
        val next = if (toIndex > fromIndex) 1 else -1
        var partiallyVisible: View? = null
        var i = fromIndex
        while (i != toIndex) {
            val child = getChildAt(i)
            val childStart = _orientationHelper!!.getDecoratedStart(child)
            val childEnd = _orientationHelper!!.getDecoratedEnd(child)
            if (childStart < end && childEnd > start) {
                if (completelyVisible) {
                    if (childStart >= start && childEnd <= end) {
                        return child
                    } else if (acceptPartiallyVisible && partiallyVisible == null) {
                        partiallyVisible = child
                    }
                } else {
                    return child
                }
            }
            i += next
        }
        return partiallyVisible
    }

    fun findLastVisibleItemPosition(): Int {
        val child = findOneVisibleChild(getChildCount() - 1, -1, false, true)
        return if (child == null) RecyclerView.NO_POSITION else getPosition(child)
    }

    fun findFirstVisibleItemPosition(): Int {
        val child = findOneVisibleChild(0, getChildCount(), false, true)
        return if (child == null) RecyclerView.NO_POSITION else getPosition(child)
    }

    override fun onSaveInstanceState(): Parcelable? {
        if (_pendingSavedState != null) {
            return SavedState(_pendingSavedState)
        }
        val state = SavedState()
        if (getChildCount() > 0) {
            ensureLayoutState();
            val didLayoutFromEnd = _lastStackFromEnd xor _shouldReverseLayout
            state._anchorLayoutFromEnd = didLayoutFromEnd
            if (didLayoutFromEnd) {
                val refChild = getChildClosestToEnd()
                state._anchorOffset = _orientationHelper!!.getEndAfterPadding() - _orientationHelper!!.getDecoratedEnd(refChild)
                state._anchorPosition = getPosition(refChild)
            } else {
                val refChild = getChildClosestToStart()
                state._anchorPosition = getPosition(refChild)
                state._anchorOffset = _orientationHelper!!.getDecoratedStart(refChild) - _orientationHelper!!.getStartAfterPadding()
            }
        } else {
            state.invalidateAnchor()
        }
        return state
    }

    fun getChildClosestToEnd(): View? = getChildAt(if (_shouldReverseLayout) 0 else getChildCount() - 1)

    fun getChildClosestToStart(): View? = getChildAt(if (_shouldReverseLayout) getChildCount() - 1 else 0)

    override fun onRestoreInstanceState(state: Parcelable?) {
        if (state is SavedState) {
            _pendingSavedState = state
            requestLayout()
        }
    }

    override fun canScrollHorizontally(): Boolean = _orientation == HORIZONTAL

    override fun canScrollVertically(): Boolean = _orientation == VERTICAL

    override fun findViewByPosition(position: Int): View? {
        val childCount = getChildCount()
        if (childCount == 0) {
            return null
        }
        val firstChild = getPosition(getChildAt(0))
        val viewPosition = position - firstChild
        if (viewPosition >= 0 && viewPosition < childCount) {
            val child = getChildAt(viewPosition)
            if (getPosition(child) == position) {
                return child
            }
        }
        return super.findViewByPosition(position)
    }

    override fun smoothScrollToPosition(recyclerView: RecyclerView?, state: RecyclerView.State?, position: Int) {
        val linearSmoothScroller = object : LinearSmoothScroller(recyclerView!!.getContext()) {
            override fun computeScrollVectorForPosition(targetPosition: Int): PointF? {
                return this@LinearLayoutManager.computeScrollVectorForPosition(targetPosition)
            }
        }
        linearSmoothScroller.setTargetPosition(position)
        startSmoothScroll(linearSmoothScroller)
    }

    fun computeScrollVectorForPosition(targetPosition: Int): PointF? {
        if (getChildCount() == 0) {
            return null
        }
        val firstChildPos = getPosition(getChildAt(0))
        val direction = (if (targetPosition < firstChildPos != _shouldReverseLayout) -1 else 1).toFloat()
        if (_orientation == HORIZONTAL) {
            return PointF(direction, 0.0f)
        } else {
            return PointF(0.0f, direction)
        }
    }

    private fun updateAnchorInfoForLayout(recycler: RecyclerView.Recycler?, state: RecyclerView.State?, anchorInfo: AnchorInfo?) {
        if (updateAnchorFromPendingData(state, anchorInfo)) {
            return
        }
        if (updateAnchorFromChildren(recycler, state, anchorInfo)) {
            return
        }
        anchorInfo?.assignCoordinateFromPadding()
        anchorInfo?._position = if (_stackFromEnd) state!!.getItemCount() - 1 else 0
    }

    fun updateAnchorFromPendingData(state: RecyclerView.State?, anchorInfo: AnchorInfo?): Boolean {
        if (state!!.isPreLayout() || _pendingScrollPosition == RecyclerView.NO_POSITION) {
            return false
        }
        if (_pendingScrollPosition < 0 || _pendingScrollPosition >= state.getItemCount()) {
            _pendingScrollPosition = RecyclerView.NO_POSITION
            _pendingScrollPositionOffset = INVALID_OFFSET
            return false
        }
        anchorInfo?._position = _pendingScrollPosition
        if (_pendingSavedState != null && _pendingSavedState!!.hasValidAnchor()) {
            anchorInfo?._layoutFromEnd = _pendingSavedState!!._anchorLayoutFromEnd
            if (anchorInfo!!._layoutFromEnd) {
                anchorInfo._coordinate = _orientationHelper!!.getEndAfterPadding() - _pendingSavedState!!._anchorOffset
            } else {
                anchorInfo._coordinate = _orientationHelper!!.getStartAfterPadding() + _pendingSavedState!!._anchorOffset
            }
            return true
        }
        if (_pendingScrollPositionOffset == INVALID_OFFSET) {
            val child = findViewByPosition(_pendingScrollPosition)
            if (child != null) {
                val childSize = _orientationHelper!!.getDecoratedMeasurement(child)
                if (childSize > _orientationHelper!!.getTotalSpace()) {
                    anchorInfo?.assignCoordinateFromPadding()
                    return true
                }
                val startGap = _orientationHelper!!.getDecoratedStart(child) - _orientationHelper!!.getStartAfterPadding()
                if (startGap < 0) {
                    anchorInfo?._coordinate = _orientationHelper!!.getStartAfterPadding()
                    anchorInfo?._layoutFromEnd = false
                    return true
                }
                val endGap = _orientationHelper!!.getEndAfterPadding() - _orientationHelper!!.getDecoratedEnd(child)
                if (endGap < 0) {
                    anchorInfo?._coordinate = _orientationHelper!!.getEndAfterPadding()
                    anchorInfo?._layoutFromEnd = true
                    return true
                }
                anchorInfo?._coordinate = if (anchorInfo!!._layoutFromEnd) (_orientationHelper!!.getDecoratedEnd(child) + _orientationHelper!!.getTotalSpaceChange()) else _orientationHelper!!.getDecoratedStart(child)
            } else {
                if (getChildCount() > 0) {
                    val pos = getPosition(getChildAt(0))
                    anchorInfo?._layoutFromEnd = _pendingScrollPosition < pos == _shouldReverseLayout
                }
                anchorInfo?.assignCoordinateFromPadding()
            }
            return true;
        }
        anchorInfo?._layoutFromEnd = _shouldReverseLayout
        if (_shouldReverseLayout) {
            anchorInfo?._coordinate = _orientationHelper!!.getEndAfterPadding() - _pendingScrollPositionOffset
        } else {
            anchorInfo?._coordinate = _orientationHelper!!.getStartAfterPadding() + _pendingScrollPositionOffset
        }
        return true
    }

    fun updateAnchorFromChildren(recycler: RecyclerView.Recycler?, state: RecyclerView.State?, anchorInfo: AnchorInfo?): Boolean {
        if (getChildCount() == 0) {
            return false
        }
        val focused = getFocusedChild()
        if (focused != null && anchorInfo!!.isViewValidAsAnchor(focused, state)) {
            anchorInfo.assignFromViewAndKeepVisibleRect(focused)
            return true
        }
        if (_lastStackFromEnd != _stackFromEnd) {
            return false
        }
        val referenceChild = if (anchorInfo!!._layoutFromEnd) findReferenceChildClosestToEnd(recycler, state) else findReferenceChildClosestToStart(recycler, state)
        if (referenceChild != null) {
            anchorInfo.assignFromView(referenceChild)
            if (!state!!.isPreLayout() && supportsPredictiveItemAnimations()) {
                val notVisible = _orientationHelper!!.getDecoratedStart(referenceChild) >= _orientationHelper!!.getEndAfterPadding() || _orientationHelper!!.getDecoratedEnd(referenceChild) < _orientationHelper!!.getStartAfterPadding()
                if (notVisible) {
                    anchorInfo._coordinate = if (anchorInfo!!._layoutFromEnd) _orientationHelper!!.getEndAfterPadding() else _orientationHelper!!.getStartAfterPadding()
                }
            }
            return true
        }
        return false
    }

    private fun findReferenceChildClosestToEnd(recycler: RecyclerView.Recycler?, state: RecyclerView.State?): View? = if (_shouldReverseLayout) findFirstReferenceChild(recycler, state) else findLastReferenceChild(recycler, state)

    private fun findReferenceChildClosestToStart(recycler: RecyclerView.Recycler?, state: RecyclerView.State?): View? = if (_shouldReverseLayout) findLastReferenceChild(recycler, state) else findFirstReferenceChild(recycler, state)

    private fun findFirstReferenceChild(recycler: RecyclerView.Recycler?, state: RecyclerView.State?): View? = findReferenceChild(recycler, state, 0, getChildCount(), state!!.getItemCount())

    private fun findLastReferenceChild(recycler: RecyclerView.Recycler?, state: RecyclerView.State?): View? = findReferenceChild(recycler, state, getChildCount() - 1, -1, state!!.getItemCount())

    fun findReferenceChild(recycler: RecyclerView.Recycler?, state: RecyclerView.State?, start: Int, end: Int, itemCount: Int): View? {
        ensureLayoutState()
        var invalidMatch: View? = null
        var outOfBoundsMatch: View? = null
        val boundsStart = _orientationHelper!!.getStartAfterPadding()
        val boundsEnd = _orientationHelper!!.getEndAfterPadding()
        val diff = if (end > start) 1 else -1
        var i = start
        while (i != end) {
            val view = getChildAt(i)
            val position = getPosition(view)
            if (position >= 0 && position < itemCount) {
                if ((view?.getLayoutParams() as RecyclerView.LayoutParams).isItemRemoved()) {
                    if (invalidMatch == null) {
                        invalidMatch = view
                    }
                } else if (_orientationHelper!!.getDecoratedStart(view) >= boundsEnd || _orientationHelper!!.getDecoratedEnd(view) < boundsStart) {
                    if (outOfBoundsMatch == null) {
                        outOfBoundsMatch = view
                    }
                } else {
                    return view
                }
            }
            i += diff
        }
        return if (outOfBoundsMatch != null) outOfBoundsMatch else invalidMatch
    }


    protected fun getExtraLayoutSpace(state: RecyclerView.State?): Int {
        if (state!!.hasTargetScrollPosition()) {
            return _orientationHelper!!.getTotalSpace()
        } else {
            return 0
        }
    }

    open fun onAnchorReady(recycler: RecyclerView.Recycler?, state: RecyclerView.State?, anchorInfo: AnchorInfo?) { }

    private fun updateLayoutStateToFillStart(anchorInfo: AnchorInfo?) {
        updateLayoutStateToFillStart(anchorInfo!!._position, anchorInfo._coordinate)
    }

    private fun updateLayoutStateToFillStart(itemPosition: Int, offset: Int) {
        _layoutState?._available = offset - _orientationHelper!!.getStartAfterPadding()
        _layoutState?._currentPosition = itemPosition
        _layoutState?._itemDirection = if (_shouldReverseLayout) LayoutState.ITEM_DIRECTION_TAIL else LayoutState.ITEM_DIRECTION_HEAD
        _layoutState?._layoutDirection = LayoutState.LAYOUT_START
        _layoutState?._offset = offset
        _layoutState?._scrollingOffset = LayoutState.SCOLLING_OFFSET_NaN

    }

    fun fill(recycler: RecyclerView.Recycler?, layoutState: LayoutState?, state: RecyclerView.State?, stopOnFocusable: Boolean): Int {
        val start = layoutState!!._available
        if (layoutState._scrollingOffset != LayoutState.SCOLLING_OFFSET_NaN) {
            if (layoutState._available < 0) {
                layoutState._scrollingOffset += layoutState._available
            }
            recycleByLayoutState(recycler, layoutState)
        }
        var remainingSpace = layoutState._available + layoutState._extra
        val layoutChunkResult = LayoutChunkResult()
        while (remainingSpace > 0 && layoutState.hasMore(state)) {
            layoutChunkResult.resetInternal()
            layoutChunk(recycler, state, layoutState, layoutChunkResult)
            if (layoutChunkResult._finished) {
                break
            }
            layoutState._offset += layoutChunkResult._consumed * layoutState._layoutDirection
            if (!layoutChunkResult._ignoreConsumed || _layoutState!!._scrapList != null || !state!!.isPreLayout()) {
                layoutState._available -= layoutChunkResult._consumed
                remainingSpace -= layoutChunkResult._consumed
            }
            if (layoutState._scrollingOffset != LayoutState.SCOLLING_OFFSET_NaN) {
                layoutState._scrollingOffset += layoutChunkResult._consumed
                if (layoutState._available < 0) {
                    layoutState._scrollingOffset += layoutState._available
                }
                recycleByLayoutState(recycler, layoutState)
            }
            if (stopOnFocusable && layoutChunkResult._focusable) {
                break
            }
        }
        return start - layoutState._available
    }

    fun layoutChunk(recycler: RecyclerView.Recycler?, state: RecyclerView.State?, layoutState: LayoutState?, result: LayoutChunkResult?) {
        val view = layoutState?.next(recycler)
        if (view == null) {
            result?._finished = true
            return
        }
        val params = view.getLayoutParams() as RecyclerView.LayoutParams
        if (layoutState?._scrapList == null) {
            if (_shouldReverseLayout == (layoutState!!._layoutDirection == LayoutState.LAYOUT_START)) {
                addView(view)
            } else {
                addView(view, 0)
            }
        } else {
            if (_shouldReverseLayout == (layoutState!!._layoutDirection == LayoutState.LAYOUT_START)) {
                addDisappearingView(view)
            } else {
                addDisappearingView(view, 0)
            }
        }
        measureChildWithMargins(view, 0, 0)
        result?._consumed = _orientationHelper!!.getDecoratedMeasurement(view)

        var left: Int
        var top: Int
        var right: Int
        var bottom: Int
        if (_orientation == VERTICAL) {
            if (isLayoutRTL()) {
                right = getWidth() - getPaddingRight()
                left = right - _orientationHelper!!.getDecoratedMeasurementInOther(view)
            } else {
                left = getPaddingLeft()
                right = left + _orientationHelper!!.getDecoratedMeasurementInOther(view)
            }
            if (layoutState._layoutDirection == LayoutState.LAYOUT_START) {
                bottom = layoutState._offset
                top = layoutState._offset - result!!._consumed
            } else {
                top = layoutState._offset
                bottom = layoutState._offset + result!!._consumed
            }
        } else {
            top = getPaddingTop()
            bottom = top + _orientationHelper!!.getDecoratedMeasurementInOther(view)
            if (layoutState._layoutDirection == LayoutState.LAYOUT_START) {
                right = layoutState._offset
                left = layoutState._offset - result!!._consumed
            } else {
                left = layoutState._offset
                right = layoutState._offset + result!!._consumed
            }
        }
        layoutDecorated(view, left + params.leftMargin, top + params.topMargin, right - params.rightMargin, bottom - params.bottomMargin)
        if (params.isItemRemoved() || params.isItemChanged()) {
            result._ignoreConsumed = true
        }
        result._focusable = view.isFocusable()
    }


    private fun recycleByLayoutState(recycler: RecyclerView.Recycler?, layoutState: LayoutState?) {
        if (!layoutState!!._recycle) {
            return
        }
        if (layoutState._layoutDirection == LayoutState.LAYOUT_START) {
            recycleViewsFromEnd(recycler, layoutState._scrollingOffset)
        } else {
            recycleViewsFromStart(recycler, layoutState._scrollingOffset)
        }
    }

    private fun recycleChildren(recycler: RecyclerView.Recycler?, startIndex: Int, endIndex: Int) {
        if (startIndex == endIndex) {
            return
        }
        if (endIndex > startIndex) {
            for (i in endIndex - 1 downTo startIndex) {
                removeAndRecycleViewAt(i, recycler)
            }
        } else {
            for (i in startIndex downTo endIndex + 1) {
                removeAndRecycleViewAt(i, recycler)
            }
        }
    }

    private fun recycleViewsFromStart(recycler: RecyclerView.Recycler?, dt: Int) {
        if (dt < 0) {
            return
        }
        val limit = dt
        val childCount = getChildCount()
        if (_shouldReverseLayout) {
            for (i in childCount - 1 downTo 0) {
                val child = getChildAt(i)
                if (_orientationHelper!!.getDecoratedEnd(child) > limit) {
                    recycleChildren(recycler, childCount - 1, i)
                    return
                }
            }
        } else {
            for (i in 0..childCount - 1) {
                val child = getChildAt(i)
                if (_orientationHelper!!.getDecoratedEnd(child) > limit) {
                    recycleChildren(recycler, 0, i)
                    return
                }
            }
        }
    }


    private fun recycleViewsFromEnd(recycler: RecyclerView.Recycler?, dt: Int) {
        val childCount = getChildCount()
        if (dt < 0) {
            return
        }
        val limit = _orientationHelper!!.getEnd() - dt
        if (_shouldReverseLayout) {
            for (i in 0..childCount - 1) {
                val child = getChildAt(i)
                if (_orientationHelper!!.getDecoratedStart(child) < limit) {
                    recycleChildren(recycler, 0, i)
                    return
                }
            }
        } else {
            for (i in childCount - 1 downTo 0) {
                val child = getChildAt(i)
                if (_orientationHelper!!.getDecoratedStart(child) < limit) {
                    recycleChildren(recycler, childCount - 1, i)
                    return
                }
            }
        }

    }

    private fun updateLayoutStateToFillEnd(anchorInfo: AnchorInfo?) {
        updateLayoutStateToFillEnd(anchorInfo!!._position, anchorInfo._coordinate)
    }

    private fun updateLayoutStateToFillEnd(itemPosition: Int, offset: Int) {
        _layoutState?._available = _orientationHelper!!.getEndAfterPadding() - offset
        _layoutState?._itemDirection = if (_shouldReverseLayout) LayoutState.ITEM_DIRECTION_HEAD else LayoutState.ITEM_DIRECTION_TAIL
        _layoutState?._currentPosition = itemPosition
        _layoutState?._layoutDirection = LayoutState.LAYOUT_END
        _layoutState?._offset = offset
        _layoutState?._scrollingOffset = LayoutState.SCOLLING_OFFSET_NaN
    }

    fun scrollBy(dy: Int, recycler: RecyclerView.Recycler?, state: RecyclerView.State?): Int {
        if (getChildCount() == 0 || dy == 0) {
            return 0
        }
        _layoutState?._recycle = true
        ensureLayoutState()
        val layoutDirection = if (dy > 0) LayoutState.LAYOUT_END else LayoutState.LAYOUT_START
        val absDy = Math.abs(dy);
        updateLayoutState(layoutDirection, absDy, true, state)
        val freeScroll = _layoutState!!._scrollingOffset
        val consumed = freeScroll + fill(recycler, _layoutState, state, false)
        if (consumed < 0) {
            return 0
        }
        val scrolled = if (absDy > consumed) layoutDirection * consumed else dy
        _orientationHelper?.offsetChildren(-scrolled)
        _layoutState?._lastScrollDelta = scrolled
        return scrolled
    }

    private fun updateLayoutState(layoutDirection: Int, requiredSpace: Int, canUseExistingSpace: Boolean, state: RecyclerView.State?) {
        _layoutState?._extra = getExtraLayoutSpace(state)
        _layoutState?._layoutDirection = layoutDirection
        var fastScrollSpace: Int
        if (layoutDirection == LayoutState.LAYOUT_END) {
            _layoutState!!._extra += _orientationHelper!!.getEndPadding()
            val child = getChildClosestToEnd()
            _layoutState?._itemDirection = if (_shouldReverseLayout) LayoutState.ITEM_DIRECTION_HEAD else LayoutState.ITEM_DIRECTION_TAIL
            _layoutState?._currentPosition = getPosition(child) + _layoutState!!._itemDirection
            _layoutState?._offset = _orientationHelper!!.getDecoratedEnd(child)
            fastScrollSpace = _orientationHelper!!.getDecoratedEnd(child)- _orientationHelper!!.getEndAfterPadding()
        } else {
            val child = getChildClosestToStart()
            _layoutState!!._extra += _orientationHelper!!.getStartAfterPadding()
            _layoutState?._itemDirection = if (_shouldReverseLayout) LayoutState.ITEM_DIRECTION_TAIL else LayoutState.ITEM_DIRECTION_HEAD
            _layoutState?._currentPosition = getPosition(child) + _layoutState!!._itemDirection
            _layoutState?._offset = _orientationHelper!!.getDecoratedStart(child)
            fastScrollSpace = -_orientationHelper!!.getDecoratedStart(child)+ _orientationHelper!!.getStartAfterPadding()
        }
        _layoutState?._available = requiredSpace
        if (canUseExistingSpace) {
            _layoutState!!._available -= fastScrollSpace
        }
        _layoutState?._scrollingOffset = fastScrollSpace
    }

    private fun fixLayoutEndGap(endOffset: Int, recycler: RecyclerView.Recycler?, state: RecyclerView.State?, canOffsetChildren: Boolean): Int {
        var gap = _orientationHelper!!.getEndAfterPadding() - endOffset
        var fixOffset = 0
        if (gap > 0) {
            fixOffset = -scrollBy(-gap, recycler, state)
        } else {
            return 0
        }
        var nendOffset = endOffset
        nendOffset += fixOffset
        if (canOffsetChildren) {
            gap = _orientationHelper!!.getEndAfterPadding() - nendOffset
            if (gap > 0) {
                _orientationHelper?.offsetChildren(gap)
                return gap + fixOffset
            }
        }
        return fixOffset
    }

    private fun fixLayoutStartGap(startOffset: Int, recycler: RecyclerView.Recycler?, state: RecyclerView.State?, canOffsetChildren: Boolean): Int {
        var gap = startOffset - _orientationHelper!!.getStartAfterPadding()
        var fixOffset = 0
        if (gap > 0) {
            fixOffset = -scrollBy(gap, recycler, state)
        } else {
            return 0
        }
        var nstartOffset = startOffset
        nstartOffset += fixOffset;
        if (canOffsetChildren) {
            gap = nstartOffset - _orientationHelper!!.getStartAfterPadding()
            if (gap > 0) {
                _orientationHelper?.offsetChildren(-gap)
                return fixOffset - gap
            }
        }
        return fixOffset
    }

    override fun onLayoutChildren(recycler: RecyclerView.Recycler?, state: RecyclerView.State?) {
        if (_pendingSavedState != null || _pendingScrollPosition != RecyclerView.NO_POSITION) {
            if (state!!.getItemCount() == 0) {
                removeAndRecycleAllViews(recycler)
                return
            }
        }
        if (_pendingSavedState != null && _pendingSavedState!!.hasValidAnchor()) {
            _pendingScrollPosition = _pendingSavedState!!._anchorPosition
        }
        ensureLayoutState()
        _layoutState?._recycle = false
        resolveShouldLayoutReverse()
        _anchorInfo.reset()
        _anchorInfo._layoutFromEnd = _shouldReverseLayout xor _stackFromEnd
        updateAnchorInfoForLayout(recycler, state, _anchorInfo)
        var extraForStart: Int
        var extraForEnd: Int
        val extra = getExtraLayoutSpace(state)
        if (_layoutState!!._lastScrollDelta >= 0) {
            extraForEnd = extra
            extraForStart = 0
        } else {
            extraForStart = extra
            extraForEnd = 0
        }
        extraForStart += _orientationHelper!!.getStartAfterPadding()
        extraForEnd += _orientationHelper!!.getEndPadding()
        if (state!!.isPreLayout() && _pendingScrollPosition != RecyclerView.NO_POSITION && _pendingScrollPositionOffset != INVALID_OFFSET) {
            val existing = findViewByPosition(_pendingScrollPosition)
            if (existing != null) {
                var current: Int
                var upcomingOffset: Int
                if (_shouldReverseLayout) {
                    current = _orientationHelper!!.getEndAfterPadding() - _orientationHelper!!.getDecoratedEnd(existing)
                    upcomingOffset = current - _pendingScrollPositionOffset
                } else {
                    current = _orientationHelper!!.getDecoratedStart(existing) - _orientationHelper!!.getStartAfterPadding()
                    upcomingOffset = _pendingScrollPositionOffset - current
                }
                if (upcomingOffset > 0) {
                    extraForStart += upcomingOffset
                } else {
                    extraForEnd -= upcomingOffset
                }
            }
        }
        var startOffset: Int
        var endOffset: Int
        onAnchorReady(recycler, state, _anchorInfo)
        detachAndScrapAttachedViews(recycler)
        _layoutState?._isPreLayout = state.isPreLayout()
        if (_anchorInfo._layoutFromEnd) {
            updateLayoutStateToFillStart(_anchorInfo)
            _layoutState?._extra = extraForStart
            fill(recycler, _layoutState, state, false)
            startOffset = _layoutState!!._offset
            val firstElement = _layoutState!!._currentPosition
            if (_layoutState!!._available > 0) {
                extraForEnd += _layoutState!!._available
            }
            updateLayoutStateToFillEnd(_anchorInfo)
            _layoutState?._extra = extraForEnd
            _layoutState!!._currentPosition += _layoutState!!._itemDirection
            fill(recycler, _layoutState, state, false)
            endOffset = _layoutState!!._offset

            if (_layoutState!!._available > 0) {
                extraForStart = _layoutState!!._available
                updateLayoutStateToFillStart(firstElement, startOffset)
                _layoutState?._extra = extraForStart
                fill(recycler, _layoutState, state, false)
                startOffset = _layoutState!!._offset
            }
        } else {
            updateLayoutStateToFillEnd(_anchorInfo)
            _layoutState?._extra = extraForEnd
            fill(recycler, _layoutState, state, false)
            endOffset = _layoutState!!._offset
            val lastElement = _layoutState!!._currentPosition
            if (_layoutState!!._available > 0) {
                extraForStart += _layoutState!!._available
            }
            updateLayoutStateToFillStart(_anchorInfo)
            _layoutState?._extra = extraForStart
            _layoutState!!._currentPosition += _layoutState!!._itemDirection
            fill(recycler, _layoutState, state, false)
            startOffset = _layoutState!!._offset
            if (_layoutState!!._available > 0) {
                extraForEnd = _layoutState!!._available
                updateLayoutStateToFillEnd(lastElement, endOffset)
                _layoutState?._extra = extraForEnd
                fill(recycler, _layoutState, state, false)
                endOffset = _layoutState!!._offset
            }
        }
        if (getChildCount() > 0) {
            if (_shouldReverseLayout xor _stackFromEnd) {
                var fixOffset = fixLayoutEndGap(endOffset, recycler, state, true)
                startOffset += fixOffset
                endOffset += fixOffset
                fixOffset = fixLayoutStartGap(startOffset, recycler, state, false)
                startOffset += fixOffset
                endOffset += fixOffset
            } else {
                var fixOffset = fixLayoutStartGap(startOffset, recycler, state, true)
                startOffset += fixOffset
                endOffset += fixOffset
                fixOffset = fixLayoutEndGap(endOffset, recycler, state, false)
                startOffset += fixOffset
                endOffset += fixOffset
            }
        }
        layoutForPredictiveAnimations(recycler, state, startOffset, endOffset)
        if (!state.isPreLayout()) {
            _pendingScrollPosition = RecyclerView.NO_POSITION
            _pendingScrollPositionOffset = INVALID_OFFSET
            _orientationHelper?.onLayoutComplete()
        }
        _lastStackFromEnd = _stackFromEnd
        _pendingSavedState = null
    }

    private fun layoutForPredictiveAnimations(recycler: RecyclerView.Recycler?, state: RecyclerView.State?, startOffset: Int, endOffset: Int) {
        if (!state!!.willRunPredictiveAnimations() ||  getChildCount() == 0 || state.isPreLayout() || !supportsPredictiveItemAnimations()) {
            return
        }
        var scrapExtraStart = 0
        var scrapExtraEnd = 0
        val scrapList = recycler?.getScrapList()
        val scrapSize = scrapList!!.size
        val firstChildPos = getPosition(getChildAt(0));
        for (i in 0..scrapSize - 1) {
            val scrap = scrapList.get(i)
            if (scrap!!.isRemoved()) {
                continue
            }
            val position = scrap.getLayoutPosition()
            val direction = if (position < firstChildPos != _shouldReverseLayout) LayoutState.LAYOUT_START else LayoutState.LAYOUT_END
            if (direction == LayoutState.LAYOUT_START) {
                scrapExtraStart += _orientationHelper!!.getDecoratedMeasurement(scrap._itemView)
            } else {
                scrapExtraEnd += _orientationHelper!!.getDecoratedMeasurement(scrap._itemView)
            }
        }
        _layoutState?._scrapList = scrapList
        if (scrapExtraStart > 0) {
            val anchor = getChildClosestToStart()
            updateLayoutStateToFillStart(getPosition(anchor), startOffset)
            _layoutState?._extra = scrapExtraStart
            _layoutState?._available = 0
            _layoutState?.assignPositionFromScrapList()
            fill(recycler, _layoutState, state, false)
        }

        if (scrapExtraEnd > 0) {
            val anchor = getChildClosestToEnd()
            updateLayoutStateToFillEnd(getPosition(anchor), endOffset)
            _layoutState?._extra = scrapExtraEnd
            _layoutState?._available = 0
            _layoutState?.assignPositionFromScrapList()
            fill(recycler, _layoutState, state, false)
        }
        _layoutState?._scrapList = null
    }

    override fun scrollToPosition(position: Int) {
        _pendingScrollPosition = position
        _pendingScrollPositionOffset = INVALID_OFFSET
        _pendingSavedState?.invalidateAnchor()
        requestLayout()
    }

    override fun scrollHorizontallyBy(dx: Int, recycler: RecyclerView.Recycler?, state: RecyclerView.State?): Int {
        if (_orientation == VERTICAL) {
            return 0
        }
        return scrollBy(dx, recycler, state)
    }

    override fun scrollVerticallyBy(dy: Int, recycler: RecyclerView.Recycler?, state: RecyclerView.State?): Int {
        if (_orientation == HORIZONTAL) {
            return 0
        }
        return scrollBy(dy, recycler, state)
    }

    private fun convertFocusDirectionToLayoutDirection(focusDirection: Int): Int {
        when (focusDirection) {
            View.FOCUS_BACKWARD -> return LayoutState.LAYOUT_START
            View.FOCUS_FORWARD -> return LayoutState.LAYOUT_END
            View.FOCUS_UP -> return if (_orientation == VERTICAL) LayoutState.LAYOUT_START else LayoutState.INVALID_LAYOUT
            View.FOCUS_DOWN -> return if (_orientation == VERTICAL) LayoutState.LAYOUT_END else LayoutState.INVALID_LAYOUT
            View.FOCUS_LEFT -> return if (_orientation == HORIZONTAL) LayoutState.LAYOUT_START else LayoutState.INVALID_LAYOUT
            View.FOCUS_RIGHT -> return if (_orientation == HORIZONTAL) LayoutState.LAYOUT_END else LayoutState.INVALID_LAYOUT
            else -> return LayoutState.INVALID_LAYOUT
        }
    }

    override fun onFocusSearchFailed(focused: View?, direction: Int, recycler: RecyclerView.Recycler?, state: RecyclerView.State?): View? {
        resolveShouldLayoutReverse()
        if (getChildCount() == 0) {
            return null
        }
        val layoutDir = convertFocusDirectionToLayoutDirection(direction)
        if (layoutDir == LayoutState.INVALID_LAYOUT) {
            return null
        }
        ensureLayoutState()
        var referenceChild: View?
        if (layoutDir == LayoutState.LAYOUT_START) {
            referenceChild = findReferenceChildClosestToStart(recycler, state)
        } else {
            referenceChild = findReferenceChildClosestToEnd(recycler, state)
        }
        if (referenceChild == null) {
            return null
        }
        ensureLayoutState()
        val maxScroll = (MAX_SCROLL_FACTOR * _orientationHelper!!.getTotalSpace()).toInt()
        updateLayoutState(layoutDir, maxScroll, false, state)
        _layoutState?._scrollingOffset = LayoutState.SCOLLING_OFFSET_NaN
        _layoutState?._recycle = false
        fill(recycler, _layoutState, state, true)
        var nextFocus: View?
        if (layoutDir == LayoutState.LAYOUT_START) {
            nextFocus = getChildClosestToStart()
        } else {
            nextFocus = getChildClosestToEnd()
        }
        if (nextFocus == referenceChild || !nextFocus!!.isFocusable()) {
            return null
        }
        return nextFocus
    }

    private fun computeScrollOffset(state: RecyclerView.State?): Int {
        if (getChildCount() == 0) {
            return 0
        }
        ensureLayoutState()
        return ScrollbarHelper.computeScrollOffset(state, _orientationHelper, findFirstVisibleChildClosestToStart(!_smoothScrollbarEnabled, true), findFirstVisibleChildClosestToEnd(!_smoothScrollbarEnabled, true), this, _smoothScrollbarEnabled, _shouldReverseLayout)
    }

    private fun computeScrollExtent(state: RecyclerView.State?): Int {
        if (getChildCount() == 0) {
            return 0
        }
        ensureLayoutState()
        return ScrollbarHelper.computeScrollExtent(state, _orientationHelper, findFirstVisibleChildClosestToStart(!_smoothScrollbarEnabled, true), findFirstVisibleChildClosestToEnd(!_smoothScrollbarEnabled, true), this,  _smoothScrollbarEnabled)
    }

    private fun computeScrollRange(state: RecyclerView.State?): Int {
        if (getChildCount() == 0) {
            return 0
        }
        ensureLayoutState()
        return ScrollbarHelper.computeScrollRange(state, _orientationHelper, findFirstVisibleChildClosestToStart(!_smoothScrollbarEnabled, true), findFirstVisibleChildClosestToEnd(!_smoothScrollbarEnabled, true), this, _smoothScrollbarEnabled)
    }

    private fun findFirstVisibleChildClosestToStart(completelyVisible: Boolean, acceptPartiallyVisible: Boolean): View? {
        if (_shouldReverseLayout) {
            return findOneVisibleChild(getChildCount() - 1, -1, completelyVisible, acceptPartiallyVisible)
        } else {
            return findOneVisibleChild(0, getChildCount(), completelyVisible, acceptPartiallyVisible)
        }
    }

    private fun findFirstVisibleChildClosestToEnd(completelyVisible: Boolean, acceptPartiallyVisible: Boolean): View? {
        if (_shouldReverseLayout) {
            return findOneVisibleChild(0, getChildCount(), completelyVisible, acceptPartiallyVisible)
        } else {
            return findOneVisibleChild(getChildCount() - 1, -1, completelyVisible, acceptPartiallyVisible)
        }
    }

    override fun computeHorizontalScrollOffset(state: RecyclerView.State?): Int {
        return computeScrollOffset(state)
    }

    override fun computeVerticalScrollOffset(state: RecyclerView.State?): Int {
        return computeScrollOffset(state)
    }

    override fun computeHorizontalScrollExtent(state: RecyclerView.State?): Int {
        return computeScrollExtent(state)
    }

    override fun computeVerticalScrollExtent(state: RecyclerView.State?): Int {
        return computeScrollExtent(state)
    }

    override fun computeHorizontalScrollRange(state: RecyclerView.State?): Int {
        return computeScrollRange(state)
    }

    override fun computeVerticalScrollRange(state: RecyclerView.State?): Int {
        return computeScrollRange(state)
    }

    override fun assertNotInLayoutOrScroll(message: String?) {
        if (_pendingSavedState == null) {
            super.assertNotInLayoutOrScroll(message)
        }
    }

    override fun supportsPredictiveItemAnimations(): Boolean = _pendingSavedState == null && _lastStackFromEnd == _stackFromEnd


    fun validateChildOrder() {
        if (getChildCount() < 1) {
            return
        }
        var lastPos = getPosition(getChildAt(0))
        var lastScreenLoc = _orientationHelper!!.getDecoratedStart(getChildAt(0))
        if (_shouldReverseLayout) {
            for (i in 1..getChildCount() - 1) {
                val child = getChildAt(i)
                val pos = getPosition(child)
                val screenLoc = _orientationHelper!!.getDecoratedStart(child)
                if (pos < lastPos) {
                    throw RuntimeException("detected invalid position. loc invalid? " + (screenLoc < lastScreenLoc))
                }
                if (screenLoc > lastScreenLoc) {
                    throw RuntimeException("detected invalid location")
                }
            }
        } else {
            for (i in 1..getChildCount() - 1) {
                val child = getChildAt(i)
                val pos = getPosition(child)
                val screenLoc = _orientationHelper!!.getDecoratedStart(child)
                if (pos < lastPos) {
                    throw RuntimeException("detected invalid position. loc invalid? " + (screenLoc < lastScreenLoc))
                }
                if (screenLoc < lastScreenLoc) {
                    throw RuntimeException("detected invalid location")
                }
            }
        }
    }

    fun getRecycleChildrenOnDetach(): Boolean = _recycleChildrenOnDetach

    fun setRecycleChildrenOnDetach(recycleChildrenOnDetach: Boolean) {
        _recycleChildrenOnDetach = recycleChildrenOnDetach
    }

    fun getStackFromEnd(): Boolean = _stackFromEnd

    fun getOrientation(): Int = _orientation

    fun getReverseLayout(): Boolean = _reverseLayout

    fun setSmoothScrollbarEnabled(enabled: Boolean) {
        _smoothScrollbarEnabled = enabled
    }

    fun isSmoothScrollbarEnabled(): Boolean = _smoothScrollbarEnabled

    fun findFirstCompletelyVisibleItemPosition(): Int {
        val child = findOneVisibleChild(0, getChildCount(), true, false)
        return if (child == null) RecyclerView.NO_POSITION else getPosition(child)
    }

    fun findLastCompletelyVisibleItemPosition(): Int {
        val child = findOneVisibleChild(getChildCount() - 1, -1, true, false)
        return if (child == null) RecyclerView.NO_POSITION else getPosition(child)
    }
}
