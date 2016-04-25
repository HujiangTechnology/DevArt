package com.hujiang.devart.component.recycler

import android.content.Context
import android.graphics.PointF
import android.graphics.Rect
import android.support.v4.os.TraceCompat
import android.support.v4.util.ArrayMap
import android.util.AttributeSet
import android.util.SparseArray
import android.view.animation.Interpolator
import android.database.Observable
import android.graphics.Canvas
import android.os.*
import android.support.v4.view.*
import android.support.v4.view.accessibility.AccessibilityEventCompat
import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat
import android.support.v4.widget.EdgeEffectCompat
import android.support.v4.widget.ScrollerCompat
import android.util.SparseIntArray
import android.util.TypedValue
import android.view.*
import android.view.accessibility.AccessibilityEvent
import android.view.accessibility.AccessibilityManager
import com.hujiang.devart.R
import java.lang.reflect.Constructor
import java.util.*

/**
 * Created by rarnu on 4/20/16.
 */
open class RecyclerView: ViewGroup, ScrollingView, NestedScrollingChild {

    companion object {
        val FORCE_INVALIDATE_DISPLAY_LIST = Build.VERSION.SDK_INT == 18 || Build.VERSION.SDK_INT == 19 || Build.VERSION.SDK_INT == 20
        val DISPATCH_TEMP_DETACH = false
        val HORIZONTAL = 0
        val VERTICAL = 1
        val NO_POSITION = -1;
        val NO_ID = -1L
        val INVALID_TYPE = -1
        val TOUCH_SLOP_DEFAULT = 0
        val TOUCH_SLOP_PAGING = 1
        val MAX_SCROLL_DURATION = 2000
        val TRACE_SCROLL_TAG = "RV Scroll"
        val TRACE_ON_LAYOUT_TAG = "RV OnLayout"
        val TRACE_ON_DATA_SET_CHANGE_LAYOUT_TAG = "RV FullInvalidate"
        val TRACE_HANDLE_ADAPTER_UPDATES_TAG = "RV PartialInvalidate"
        val TRACE_BIND_VIEW_TAG = "RV OnBindView"
        val TRACE_CREATE_VIEW_TAG = "RV CreateView"
        val LAYOUT_MANAGER_CONSTRUCTOR_SIGNATURE = arrayOf(Context::class.java, AttributeSet::class.java, Integer.TYPE, Integer.TYPE)
        val DEFAULT_CACHE_SIZE = 2
        val DEFAULT_MAX_SCRAP = 5
        val SCROLL_STATE_IDLE = 0
        val SCROLL_STATE_DRAGGING = 1
        val SCROLL_STATE_SETTLING = 2
        val INVALID_POINTER = -1
        val sQuinticInterpolator = object: Interpolator {
            override fun getInterpolation(t: Float): Float {
                var nt = t - 1.0f
                return nt * nt * nt * nt * nt + 1.0f
            }
        }

        fun getChildViewHolderInt(child: View?): ViewHolder? {
            if (child == null) {
                return null
            }
            return (child.getLayoutParams() as LayoutParams)._viewHolder
        }
    }

    var _adapter: Adapter<*>? = null
    var _layout: LayoutManager? = null
    var _adapterHelper: AdapterHelper? = null
    var _childHelper: ChildHelper? = null
    var _clipToPadding = false
    val _state = State()
    var _itemsAddedOrRemoved = false
    var _itemsChanged = false;
    var _itemAnimatorListener = ItemAnimatorRestoreListener()
    var _postedAnimatorRunner = false
    var _accessibilityDelegate: RecyclerViewAccessibilityDelegate? = null
    var _childDrawingOrderCallback: ChildDrawingOrderCallback? = null
    var _itemAnimator: ItemAnimator? = DefaultItemAnimator()
    var _itemAnimatorRunner = Runnable {
        _itemAnimator?.runPendingAnimations()
        _postedAnimatorRunner = false
    }
    val _recycler = Recycler()
    val _itemDecorations = arrayListOf<ItemDecoration?>()
    var _isAttached = false
    var _hasFixedSize = false
    var _firstLayoutComplete = false
    var _eatRequestLayout = false
    var _layoutRequestEaten = false
    var _layoutFrozen = false
    var _ignoreMotionEventTillDown = false
    val _observer = RecyclerViewDataObserver()
    val _viewFlinger = ViewFlinger()
    var _scrollListener: OnScrollListener? = null
    var _scrollListeners: MutableList<OnScrollListener?>? = null
    var _dataSetHasChangedAfterLayout = false
    val _updateChildViewsRunnable = Runnable {
            if (!_firstLayoutComplete) {
                return@Runnable
            }
            if (_dataSetHasChangedAfterLayout) {
                TraceCompat.beginSection(TRACE_ON_DATA_SET_CHANGE_LAYOUT_TAG)
                dispatchLayout()
                TraceCompat.endSection()
            } else if (_adapterHelper!!.hasPendingUpdates()) {
                TraceCompat.beginSection(TRACE_HANDLE_ADAPTER_UPDATES_TAG)
                eatRequestLayout()
                _adapterHelper?.preProcess()
                if (!_layoutRequestEaten) {
                    rebindUpdatedViewHolders()
                }
                resumeRequestLayout(true)
                TraceCompat.endSection()
            }
        }
    var _layoutOrScrollCounter = 0
    val _minMaxLayoutPositions = IntArray(2)
    var _pendingSavedState: SavedState? = null
    val _tempRect = Rect()
    var _recyclerListener: RecyclerListener? = null
    val _onItemTouchListeners = arrayListOf<OnItemTouchListener?>()
    var _activeOnItemTouchListener: OnItemTouchListener? = null

    var _eatenAccessibilityChangeFlags = 0
    var _adapterUpdateDuringMeasure = false
    var _postUpdatesOnAnimation = false
    var _accessibilityManager: AccessibilityManager? = null
    var _onChildAttachStateListeners: MutableList<OnChildAttachStateChangeListener?>? = null
    var _leftGlow: EdgeEffectCompat? = null
    var _topGlow: EdgeEffectCompat? = null
    var _rightGlow: EdgeEffectCompat? = null
    var _bottomGlow: EdgeEffectCompat? = null
    var _scrollState = SCROLL_STATE_IDLE;
    var _scrollPointerId = INVALID_POINTER;
    var _velocityTracker: VelocityTracker? = null
    var _initialTouchX = 0
    var _initialTouchY = 0
    var _lastTouchX = 0
    var _lastTouchY = 0
    var _touchSlop = 0
    var _minFlingVelocity = 0
    var _maxFlingVelocity = 0
    var _scrollFactor = Float.MIN_VALUE;
    var _scrollingChildHelper: NestedScrollingChildHelper? = null
    val _scrollOffset = IntArray(2)
    val _scrollConsumed = IntArray(2)
    val _nestedOffsets = IntArray(2)

    constructor(context: Context): this(context, null)
    constructor(context: Context, attrs: AttributeSet?): this(context, attrs, 0)
    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        setScrollContainer(true)
        setFocusableInTouchMode(true)
        val version = Build.VERSION.SDK_INT
        _postUpdatesOnAnimation = version >= 16
        val vc = ViewConfiguration.get(context)
        _touchSlop = vc.getScaledTouchSlop();
        _minFlingVelocity = vc.getScaledMinimumFlingVelocity()
        _maxFlingVelocity = vc.getScaledMaximumFlingVelocity()
        setWillNotDraw(ViewCompat.getOverScrollMode(this) == ViewCompat.OVER_SCROLL_NEVER)
        _itemAnimator?.setListener(_itemAnimatorListener)
        initAdapterManager()
        initChildrenHelper()
        if (ViewCompat.getImportantForAccessibility(this) == ViewCompat.IMPORTANT_FOR_ACCESSIBILITY_AUTO) {
            ViewCompat.setImportantForAccessibility(this, ViewCompat.IMPORTANT_FOR_ACCESSIBILITY_YES)
        }
        _accessibilityManager = getContext().getSystemService(Context.ACCESSIBILITY_SERVICE) as AccessibilityManager
        setAccessibilityDelegateCompat(RecyclerViewAccessibilityDelegate(this))
        if (attrs != null) {
            var defStyleRes = 0
            val a = context.obtainStyledAttributes(attrs, R.styleable.RecyclerView, defStyle, defStyleRes)
            val layoutManagerName = a.getString(R.styleable.RecyclerView_layoutManager)
            a.recycle()
            createLayoutManager(context, layoutManagerName, attrs, defStyle, defStyleRes)
        }
        _scrollingChildHelper = NestedScrollingChildHelper(this)
        setNestedScrollingEnabled(true)
    }

    fun getCompatAccessibilityDelegate(): RecyclerViewAccessibilityDelegate? = _accessibilityDelegate

    fun setHasFixedSize(hasFixedSize: Boolean) {
        _hasFixedSize = hasFixedSize
    }

    fun hasFixedSize(): Boolean = _hasFixedSize

    override fun setClipToPadding(clipToPadding: Boolean) {
        if (clipToPadding != _clipToPadding) {
            invalidateGlows()
        }
        _clipToPadding = clipToPadding
        super.setClipToPadding(clipToPadding)
        if (_firstLayoutComplete) {
            requestLayout()
        }
    }

    fun setScrollingTouchSlop(slopConstant: Int) {
        val vc = ViewConfiguration.get(getContext())
        when (slopConstant) {
            TOUCH_SLOP_PAGING -> _touchSlop = ViewConfigurationCompat.getScaledPagingTouchSlop(vc)
            else -> _touchSlop = vc.getScaledTouchSlop()
        }
    }

    fun setLayoutFrozen(frozen: Boolean) {
        if (frozen != _layoutFrozen) {
            assertNotInLayoutOrScroll("Do not setLayoutFrozen in layout or scroll")
            if (!frozen) {
                _layoutFrozen = frozen;
                if (_layoutRequestEaten && _layout != null && _adapter != null) {
                    requestLayout()
                }
                _layoutRequestEaten = false
            } else {
                val now = SystemClock.uptimeMillis()
                val cancelEvent = MotionEvent.obtain(now, now, MotionEvent.ACTION_CANCEL, 0.0f, 0.0f, 0)
                onTouchEvent(cancelEvent)
                _layoutFrozen = frozen
                _ignoreMotionEventTillDown = true
                stopScroll()
            }
        }
    }

    fun setAdapterInternal(adapter: Adapter<*>?, compatibleWithPrevious: Boolean, removeAndRecycleViews: Boolean) {
        if (_adapter != null) {
            _adapter?.unregisterAdapterDataObserver(_observer)
            _adapter?.onDetachedFromRecyclerView(this)
        }
        if (!compatibleWithPrevious || removeAndRecycleViews) {
            _itemAnimator?.endAnimations()
            if (_layout != null) {
                _layout?.removeAndRecycleAllViews(_recycler)
                _layout?.removeAndRecycleScrapInt(_recycler)
            }
            _recycler.clear()
        }
        _adapterHelper?.reset()
        val oldAdapter = _adapter
        _adapter = adapter
        if (adapter != null) {
            adapter.registerAdapterDataObserver(_observer)
            adapter.onAttachedToRecyclerView(this)
        }
        _layout?.onAdapterChanged(oldAdapter, _adapter)
        _recycler.onAdapterChanged(oldAdapter, _adapter, compatibleWithPrevious)
        _state._structureChanged = true
        markKnownViewsInvalid()
    }

    fun dispatchOnItemTouch(e: MotionEvent?): Boolean {
        val action = e!!.getAction()
        if (_activeOnItemTouchListener != null) {
            if (action == MotionEvent.ACTION_DOWN) {
                _activeOnItemTouchListener = null
            } else {
                _activeOnItemTouchListener?.onTouchEvent(this, e)
                if (action == MotionEvent.ACTION_CANCEL || action == MotionEvent.ACTION_UP) {
                    _activeOnItemTouchListener = null
                }
                return true
            }
        }
        if (action != MotionEvent.ACTION_DOWN) {
            val listenerCount = _onItemTouchListeners.size
            for (i in 0..listenerCount - 1) {
                val listener = _onItemTouchListeners.get(i)
                if (listener!!.onInterceptTouchEvent(this, e)) {
                    _activeOnItemTouchListener = listener
                    return true
                }
            }
        }
        return false
    }

    fun resetTouch() {
        _velocityTracker?.clear()
        stopNestedScroll()
        releaseGlows()
    }

    fun cancelTouch() {
        resetTouch()
        setScrollState(SCROLL_STATE_IDLE)
    }

    override fun onTouchEvent(e: MotionEvent?): Boolean {
        if (_layoutFrozen || _ignoreMotionEventTillDown) {
            return false
        }
        if (dispatchOnItemTouch(e)) {
            cancelTouch()
            return true
        }
        if (_layout == null) {
            return false
        }

        val canScrollHorizontally = _layout!!.canScrollHorizontally()
        val canScrollVertically = _layout!!.canScrollVertically()
        if (_velocityTracker == null) {
            _velocityTracker = VelocityTracker.obtain()
        }
        var eventAddedToVelocityTracker = false
        val vtev = MotionEvent.obtain(e)
        val action = MotionEventCompat.getActionMasked(e)
        val actionIndex = MotionEventCompat.getActionIndex(e)

        if (action == MotionEvent.ACTION_DOWN) {
            _nestedOffsets[0] = 0
            _nestedOffsets[1] = 0
        }
        vtev.offsetLocation(_nestedOffsets[0].toFloat(), _nestedOffsets[1].toFloat())
        when (action) {
            MotionEvent.ACTION_DOWN -> {
                _scrollPointerId = MotionEventCompat.getPointerId(e, 0)
                _initialTouchX = (e!!.getX() + 0.5f).toInt()
                _lastTouchX = _initialTouchX
                _initialTouchY = (e.getY() + 0.5f).toInt()
                _lastTouchY = _initialTouchY
                var nestedScrollAxis = ViewCompat.SCROLL_AXIS_NONE
                if (canScrollHorizontally) {
                    nestedScrollAxis = nestedScrollAxis or ViewCompat.SCROLL_AXIS_HORIZONTAL
                }
                if (canScrollVertically) {
                    nestedScrollAxis = nestedScrollAxis or ViewCompat.SCROLL_AXIS_VERTICAL
                }
                startNestedScroll(nestedScrollAxis)
            }
            MotionEventCompat.ACTION_POINTER_DOWN -> {
                _scrollPointerId = MotionEventCompat.getPointerId(e, actionIndex)
                _initialTouchX = (MotionEventCompat.getX(e, actionIndex) + 0.5f).toInt()
                _lastTouchX = _initialTouchX
                _initialTouchY = (MotionEventCompat.getY(e, actionIndex) + 0.5f).toInt()
                _lastTouchY = _initialTouchY
            }
            MotionEvent.ACTION_MOVE -> {
                val index = MotionEventCompat.findPointerIndex(e, _scrollPointerId)
                if (index < 0) {
                    return false
                }

                val x = (MotionEventCompat.getX(e, index) + 0.5f).toInt()
                val y = (MotionEventCompat.getY(e, index) + 0.5f).toInt()
                var dx = _lastTouchX - x
                var dy = _lastTouchY - y
                if (dispatchNestedPreScroll(dx, dy, _scrollConsumed, _scrollOffset)) {
                    dx -= _scrollConsumed[0]
                    dy -= _scrollConsumed[1]
                    vtev.offsetLocation(_scrollOffset[0].toFloat(), _scrollOffset[1].toFloat())
                    _nestedOffsets[0] += _scrollOffset[0]
                    _nestedOffsets[1] += _scrollOffset[1]
                }

                if (_scrollState != SCROLL_STATE_DRAGGING) {
                    var startScroll = false
                    if (canScrollHorizontally && Math.abs(dx) > _touchSlop) {
                        if (dx > 0) {
                            dx -= _touchSlop
                        } else {
                            dx += _touchSlop
                        }
                        startScroll = true
                    }
                    if (canScrollVertically && Math.abs(dy) > _touchSlop) {
                        if (dy > 0) {
                            dy -= _touchSlop
                        } else {
                            dy += _touchSlop
                        }
                        startScroll = true
                    }
                    if (startScroll) {
                        val parent = getParent()
                        if (parent != null) {
                            parent.requestDisallowInterceptTouchEvent(true)
                        }
                        setScrollState(SCROLL_STATE_DRAGGING)
                    }
                }

                if (_scrollState == SCROLL_STATE_DRAGGING) {
                    _lastTouchX = x - _scrollOffset[0]
                    _lastTouchY = y - _scrollOffset[1]

                    if (scrollByInternal(if (canScrollHorizontally) dx else 0, if( canScrollVertically) dy else 0, vtev)) {
                        getParent().requestDisallowInterceptTouchEvent(true)
                    }
                }
            }

            MotionEventCompat.ACTION_POINTER_UP -> onPointerUp(e)

            MotionEvent.ACTION_UP -> {
                _velocityTracker?.addMovement(vtev)
                eventAddedToVelocityTracker = true
                _velocityTracker?.computeCurrentVelocity(1000, _maxFlingVelocity.toFloat())
                val xvel = if (canScrollHorizontally) -VelocityTrackerCompat.getXVelocity(_velocityTracker, _scrollPointerId) else 0.0f
                val yvel = if (canScrollVertically) -VelocityTrackerCompat.getYVelocity(_velocityTracker, _scrollPointerId) else 0.0f
                if (!((xvel != 0.0f || yvel != 0.0f) && fling(xvel.toInt(), yvel.toInt()))) {
                    setScrollState(SCROLL_STATE_IDLE)
                }
                resetTouch();
            }

            MotionEvent.ACTION_CANCEL -> cancelTouch()
        }
        if (!eventAddedToVelocityTracker) {
            _velocityTracker?.addMovement(vtev)
        }
        vtev.recycle()
        return true;
    }

    fun onPointerUp(e: MotionEvent?) {
        val actionIndex = MotionEventCompat.getActionIndex(e)
        if (MotionEventCompat.getPointerId(e, actionIndex) == _scrollPointerId) {
            val newIndex = if (actionIndex == 0) 1 else 0
            _scrollPointerId = MotionEventCompat.getPointerId(e, newIndex);
            _initialTouchX = (MotionEventCompat.getX(e, newIndex) + 0.5f).toInt()
            _lastTouchX = _initialTouchX
            _initialTouchY = (MotionEventCompat.getY(e, newIndex) + 0.5f).toInt()
            _lastTouchY = _initialTouchY
        }
    }

    fun fling(velocityX: Int, velocityY: Int): Boolean {
        if (_layout == null) {
            return false
        }
        if (_layoutFrozen) {
            return false
        }
        val canScrollHorizontal = _layout!!.canScrollHorizontally()
        val canScrollVertical = _layout!!.canScrollVertically()
        var nvelocityX = velocityX
        var nvelocityY = velocityY

        if (!canScrollHorizontal || Math.abs(nvelocityX) < _minFlingVelocity) {
            nvelocityX = 0
        }
        if (!canScrollVertical || Math.abs(nvelocityY) < _minFlingVelocity) {
            nvelocityY = 0
        }
        if (nvelocityX == 0 && nvelocityY == 0) {
            return false
        }
        if (!dispatchNestedPreFling(nvelocityX.toFloat(), nvelocityY.toFloat())) {
            val canScroll = canScrollHorizontal || canScrollVertical
            dispatchNestedFling(nvelocityX.toFloat(), nvelocityY.toFloat(), canScroll)
            if (canScroll) {
                nvelocityX = Math.max(-_maxFlingVelocity, Math.min(nvelocityX, _maxFlingVelocity))
                nvelocityY = Math.max(-_maxFlingVelocity, Math.min(nvelocityY, _maxFlingVelocity))
                _viewFlinger.fling(nvelocityX, nvelocityY)
                return true;
            }
        }
        return false;
    }

    fun scrollByInternal(x: Int, y: Int, ev: MotionEvent?): Boolean {
        var unconsumedX = 0
        var unconsumedY = 0
        var consumedX = 0
        var consumedY = 0
        consumePendingUpdateOperations()
        if (_adapter != null) {
            eatRequestLayout()
            onEnterLayoutOrScroll()
            TraceCompat.beginSection(TRACE_SCROLL_TAG)
            if (x != 0) {
                consumedX = _layout!!.scrollHorizontallyBy(x, _recycler, _state)
                unconsumedX = x - consumedX
            }
            if (y != 0) {
                consumedY = _layout!!.scrollVerticallyBy(y, _recycler, _state)
                unconsumedY = y - consumedY
            }
            TraceCompat.endSection()
            if (supportsChangeAnimations()) {
                val count = _childHelper!!.getChildCount()
                for (i in 0..count - 1) {
                    val view = _childHelper?.getChildAt(i)
                    val holder = getChildViewHolder(view)
                    if (holder != null && holder._shadowingHolder != null) {
                        val shadowingHolder = holder._shadowingHolder
                        val shadowingView = if (shadowingHolder != null) shadowingHolder._itemView else null
                        if (shadowingView != null) {
                            val left = view!!.getLeft()
                            val top = view.getTop()
                            if (left != shadowingView.getLeft() || top != shadowingView.getTop()) {
                                shadowingView.layout(left, top, left + shadowingView.getWidth(), top + shadowingView.getHeight())
                            }
                        }
                    }
                }
            }
            onExitLayoutOrScroll()
            resumeRequestLayout(false)
        }
        if (!_itemDecorations.isEmpty()) {
            invalidate()
        }
        if (dispatchNestedScroll(consumedX, consumedY, unconsumedX, unconsumedY, _scrollOffset)) {
            _lastTouchX -= _scrollOffset[0]
            _lastTouchY -= _scrollOffset[1]
            if (ev != null) {
                ev.offsetLocation(_scrollOffset[0].toFloat(), _scrollOffset[1].toFloat())
            }
            _nestedOffsets[0] += _scrollOffset[0]
            _nestedOffsets[1] += _scrollOffset[1]
        } else if (ViewCompat.getOverScrollMode(this) != ViewCompat.OVER_SCROLL_NEVER) {
            if (ev != null) {
                pullGlows(ev.getX(), unconsumedX.toFloat(), ev.getY(), unconsumedY.toFloat())
            }
            considerReleasingGlowsOnScroll(x, y)
        }
        if (consumedX != 0 || consumedY != 0) {
            dispatchOnScrolled(consumedX, consumedY)
        }
        if (!awakenScrollBars()) {
            invalidate()
        }
        return consumedX != 0 || consumedY != 0
    }

    fun swapAdapter(adapter: Adapter<*>?, removeAndRecycleExistingViews: Boolean) {
        setLayoutFrozen(false)
        setAdapterInternal(adapter, true, removeAndRecycleExistingViews)
        setDataSetChangedAfterLayout()
        requestLayout()
    }

    fun setAdapter(adapter: Adapter<*>?) {
        setLayoutFrozen(false)
        setAdapterInternal(adapter, false, true)
        requestLayout()
    }

    fun dispatchChildDetached(child: View?) {
        val viewHolder = getChildViewHolderInt(child)
        onChildDetachedFromWindow(child)
        if (_adapter != null && viewHolder != null) {
            _adapter?.onViewDetachedFromWindow(viewHolder as Nothing?)
        }
        if (_onChildAttachStateListeners != null) {
            val cnt = _onChildAttachStateListeners!!.size
            for (i in cnt - 1 downTo 0) {
                _onChildAttachStateListeners?.get(i)?.onChildViewDetachedFromWindow(child)
            }
        }
    }

    fun dispatchChildAttached(child: View?) {
        val viewHolder = getChildViewHolderInt(child)
        onChildAttachedToWindow(child)
        if (_adapter != null && viewHolder != null) {
            _adapter?.onViewAttachedToWindow(viewHolder as Nothing?)
        }
        if (_onChildAttachStateListeners != null) {
            val cnt = _onChildAttachStateListeners!!.size
            for (i in cnt - 1 downTo 0) {
                _onChildAttachStateListeners?.get(i)?.onChildViewAttachedToWindow(child)
            }
        }
    }

    open fun onChildAttachedToWindow(child: View?) { }

    open fun onChildDetachedFromWindow(child: View?) { }

    fun initChildrenHelper() {
        _childHelper = ChildHelper(object: ChildHelper.Callback {

            override fun getChildCount(): Int = this@RecyclerView.getChildCount()

            override fun addView(child: View?, index: Int) {
                this@RecyclerView.addView(child, index)
                dispatchChildAttached(child)
            }

            override fun indexOfChild(view: View?): Int = this@RecyclerView.indexOfChild(view)

            override fun removeViewAt(index: Int) {
                val child = this@RecyclerView.getChildAt(index)
                if (child != null) {
                    dispatchChildDetached(child)
                }
                this@RecyclerView.removeViewAt(index)
            }

            override fun getChildAt(offset: Int): View? = this@RecyclerView.getChildAt(offset)

            override fun removeAllViews() {
                val count = getChildCount()
                for (i in 0..count - 1) {
                    dispatchChildDetached(getChildAt(i))
                }
                this@RecyclerView.removeAllViews()
            }

            override fun getChildViewHolder(view: View?): ViewHolder? = getChildViewHolderInt(view)

            override fun attachViewToParent(child: View?, index: Int, layoutParams: ViewGroup.LayoutParams?) {
                val vh = getChildViewHolderInt(child)
                if (vh != null) {
                    if (!vh.isTmpDetached() && !vh.shouldIgnore()) {
                        throw IllegalArgumentException("Called attach on a child which is not detached: ${vh}")
                    }
                    vh.clearTmpDetachFlag();
                }
                this@RecyclerView.attachViewToParent(child, index, layoutParams)
            }

            override fun detachViewFromParent(offset: Int) {
                val view = getChildAt(offset);
                if (view != null) {
                    val vh = getChildViewHolderInt(view)
                    if (vh != null) {
                        if (vh.isTmpDetached() && !vh.shouldIgnore()) {
                            throw IllegalArgumentException("called detach on an already detached child ${vh}")
                        }
                        vh.addFlags(ViewHolder.FLAG_TMP_DETACHED)
                    }
                }
                this@RecyclerView.detachViewFromParent(offset)
            }

            override fun onEnteredHiddenState(child: View?) {
                val vh = getChildViewHolderInt(child)
                if (vh != null) {
                    vh.onEnteredHiddenState()
                }
            }

            override fun onLeftHiddenState(child: View?) {
                val vh = getChildViewHolderInt(child);
                if (vh != null) {
                    vh.onLeftHiddenState()
                }
            }
        })
    }


    fun findViewHolderForPosition(position: Int, checkNewPosition: Boolean): ViewHolder? {
        val childCount = _childHelper!!.getUnfilteredChildCount()
        for (i in 0..childCount - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getUnfilteredChildAt(i))
            if (holder != null && !holder.isRemoved()) {
                if (checkNewPosition) {
                    if (holder._position == position) {
                        return holder
                    }
                } else if (holder.getLayoutPosition() == position) {
                    return holder
                }
            }
        }
        return null
    }

    fun offsetPositionRecordsForMove(from: Int, to: Int) {
        val childCount = _childHelper!!.getUnfilteredChildCount()
        var start: Int
        var end: Int
        var inBetweenOffset: Int
        if (from < to) {
            start = from
            end = to
            inBetweenOffset = -1
        } else {
            start = to
            end = from
            inBetweenOffset = 1
        }
        for (i in 0..childCount - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getUnfilteredChildAt(i))
            if (holder == null || holder._position < start || holder._position > end) {
                continue
            }
            if (holder._position == from) {
                holder.offsetPosition(to - from, false)
            } else {
                holder.offsetPosition(inBetweenOffset, false)
            }
            _state._structureChanged = true
        }
        _recycler.offsetPositionRecordsForMove(from, to)
        requestLayout()
    }

    fun offsetPositionRecordsForInsert(positionStart: Int, itemCount: Int) {
        val childCount = _childHelper!!.getUnfilteredChildCount()
        for (i in 0..childCount - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getUnfilteredChildAt(i))
            if (holder != null && !holder.shouldIgnore() && holder._position >= positionStart) {
                holder.offsetPosition(itemCount, false)
                _state._structureChanged = true
            }
        }
        _recycler.offsetPositionRecordsForInsert(positionStart, itemCount)
        requestLayout()
    }

    fun offsetPositionRecordsForRemove(positionStart: Int, itemCount: Int, applyToPreLayout: Boolean) {
        val positionEnd = positionStart + itemCount
        val childCount = _childHelper!!.getUnfilteredChildCount()
        for (i in 0..childCount - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getUnfilteredChildAt(i))
            if (holder != null && !holder.shouldIgnore()) {
                if (holder._position >= positionEnd) {
                    holder.offsetPosition(-itemCount, applyToPreLayout)
                    _state._structureChanged = true
                } else if (holder._position >= positionStart) {
                    holder.flagRemovedAndOffsetPosition(positionStart - 1, -itemCount, applyToPreLayout)
                    _state._structureChanged = true
                }
            }
        }
        _recycler.offsetPositionRecordsForRemove(positionStart, itemCount, applyToPreLayout)
        requestLayout()
    }

    fun viewRangeUpdate(positionStart: Int, itemCount: Int, payload: Any?) {
        val childCount = _childHelper!!.getUnfilteredChildCount()
        val positionEnd = positionStart + itemCount
        for (i in 0..childCount - 1) {
            val child = _childHelper?.getUnfilteredChildAt(i)
            val holder = getChildViewHolderInt(child)
            if (holder == null || holder.shouldIgnore()) {
                continue
            }
            if (holder._position >= positionStart && holder._position < positionEnd) {
                holder.addFlags(ViewHolder.FLAG_UPDATE)
                holder.addChangePayload(payload)
                if (supportsChangeAnimations()) {
                    holder.addFlags(ViewHolder.FLAG_CHANGED)
                }
                (child?.getLayoutParams() as LayoutParams)._insetsDirty = true
            }
        }
        _recycler.viewRangeUpdate(positionStart, itemCount)
    }

    fun initAdapterManager() {
        _adapterHelper = AdapterHelper(object: AdapterHelper.Callback {
            override fun markViewHoldersUpdated(positionStart: Int, itemCount: Int, payload: Any?) {
                viewRangeUpdate(positionStart, itemCount, payload)
                _itemsChanged = true
            }

            override fun findViewHolder(position: Int): ViewHolder? {
                val vh = findViewHolderForPosition(position, true)
                if (vh == null) {
                    return null
                }
                if (_childHelper!!.isHidden(vh._itemView)) {
                    return null
                }
                return vh
            }

            override fun offsetPositionsForMove(from: Int, to: Int) {
                offsetPositionRecordsForMove(from, to)
                _itemsAddedOrRemoved = true
            }

            override fun offsetPositionsForRemovingInvisible(positionStart: Int, itemCount: Int) {
                offsetPositionRecordsForRemove(positionStart, itemCount, true);
                _itemsAddedOrRemoved = true
                _state._deletedInvisibleItemCountSincePreviousLayout += itemCount
            }

            override fun offsetPositionsForRemovingLaidOutOrNewView(positionStart: Int, itemCount: Int) {
                offsetPositionRecordsForRemove(positionStart, itemCount, false)
                _itemsAddedOrRemoved = true
            }

            override fun onDispatchFirstPass(updateOp: AdapterHelper.UpdateOp?) {
                dispatchUpdate(updateOp)
            }

            override fun offsetPositionsForAdd(positionStart: Int, itemCount: Int) {
                offsetPositionRecordsForInsert(positionStart, itemCount)
                _itemsAddedOrRemoved = true
            }

            override fun onDispatchSecondPass(updateOp: AdapterHelper.UpdateOp?) {
                dispatchUpdate(updateOp)
            }

            fun dispatchUpdate(op: AdapterHelper.UpdateOp?) {
                when (op!!._cmd) {
                    AdapterHelper.UpdateOp.ADD -> _layout?.onItemsAdded(this@RecyclerView, op._positionStart, op._itemCount)
                    AdapterHelper.UpdateOp.REMOVE -> _layout?.onItemsRemoved(this@RecyclerView, op._positionStart, op._itemCount)
                    AdapterHelper.UpdateOp.UPDATE -> _layout?.onItemsUpdated(this@RecyclerView, op._positionStart, op._itemCount, op._payload)
                    AdapterHelper.UpdateOp.MOVE -> _layout?.onItemsMoved(this@RecyclerView, op._positionStart, op._itemCount, 1)
                }
            }
        })
    }

    fun setAccessibilityDelegateCompat(accessibilityDelegate: RecyclerViewAccessibilityDelegate?) {
        _accessibilityDelegate = accessibilityDelegate
        ViewCompat.setAccessibilityDelegate(this, _accessibilityDelegate)
    }

    fun getFullClassName(context: Context, className: String?): String? {
        if (className!![0] == '.') {
            return context.getPackageName() + className
        }
        if (className.contains(".")) {
            return className
        }
        return RecyclerView.javaClass.getPackage().getName() + '.' + className
    }

    fun createLayoutManager(context: Context, className: String?, attrs: AttributeSet?, defStyleAttr: Int, defStyleRes: Int) {
        var nclassName = className
        if (nclassName != null) {
            nclassName = nclassName.trim()
            if (nclassName.length != 0) {
                nclassName = getFullClassName(context, className)
                try {
                    var classLoader: ClassLoader? = null
                    if (isInEditMode()) {
                        classLoader = javaClass.getClassLoader()
                    } else {
                        classLoader = context.getClassLoader()
                    }
                    val layoutManagerClass = classLoader.loadClass(nclassName).asSubclass(LayoutManager::class.java)
                    var constructor: Constructor<out LayoutManager>? = null
                    var constructorArgs: Array<Any?>? = null
                    try {
                        constructor = layoutManagerClass.getConstructor(LAYOUT_MANAGER_CONSTRUCTOR_SIGNATURE as java.lang.Class<*>)
                        constructorArgs = arrayOf(context, attrs, defStyleAttr, defStyleRes)
                    } catch (e: Exception) {
                        try {
                            constructor = layoutManagerClass.getConstructor()
                        } catch (ee: Exception) {
                        }
                    }
                    constructor?.setAccessible(true)
                    setLayoutManager(constructor?.newInstance(constructorArgs))
                } catch (e: Exception) {
                }
            }
        }
    }

    fun setLayoutManager(layout: LayoutManager?) {
        if (layout == _layout) {
            return
        }
        if (_layout != null) {
            if (_isAttached) {
                _layout?.dispatchDetachedFromWindow(this, _recycler)
            }
            _layout?.setRecyclerView(null)
        }
        _recycler.clear()
        _childHelper?.removeAllViewsUnfiltered()
        _layout = layout
        if (layout != null) {
            if (layout._recyclerView != null) {
                throw IllegalArgumentException("LayoutManager ${layout} is already attached to a RecyclerView: ${layout._recyclerView}")
            }
            _layout?.setRecyclerView(this)
            if (_isAttached) {
                _layout?.dispatchAttachedToWindow(this)
            }
        }
        requestLayout()
    }

    fun rebindUpdatedViewHolders() {
        val childCount = _childHelper!!.getChildCount()
        for (i in 0..childCount - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getChildAt(i))
            if (holder == null || holder.shouldIgnore()) {
                continue
            }
            if (holder.isRemoved() || holder.isInvalid()) {
                requestLayout()
            } else if (holder.needsUpdate()) {
                val type = _adapter!!.getItemViewType(holder._position)
                if (holder.getItemViewType() == type) {
                    if (!holder.isChanged() || !supportsChangeAnimations()) {
                        _adapter?.bindViewHolder(holder as Nothing?, holder._position)
                    } else {
                        requestLayout()
                    }
                } else {
                    requestLayout()
                    break
                }
            }
        }
    }

    fun pullGlows(x: Float, overscrollX: Float, y: Float, overscrollY: Float) {
        var invalidate = false
        if (overscrollX < 0) {
            ensureLeftGlow()
            if (_leftGlow!!.onPull(-overscrollX / getWidth(), 1f - y  / getHeight())) {
                invalidate = true
            }
        } else if (overscrollX > 0) {
            ensureRightGlow()
            if (_rightGlow!!.onPull(overscrollX / getWidth(), y / getHeight())) {
                invalidate = true
            }
        }
        if (overscrollY < 0) {
            ensureTopGlow()
            if (_topGlow!!.onPull(-overscrollY / getHeight(), x / getWidth())) {
                invalidate = true
            }
        } else if (overscrollY > 0) {
            ensureBottomGlow()
            if (_bottomGlow!!.onPull(overscrollY / getHeight(), 1f - x / getWidth())) {
                invalidate = true
            }
        }
        if (invalidate || overscrollX != 0.0f || overscrollY != 0.0f) {
            ViewCompat.postInvalidateOnAnimation(this)
        }
    }

    fun releaseGlows() {
        var needsInvalidate = false
        if (_leftGlow != null) {
            needsInvalidate = _leftGlow!!.onRelease()
        }
        if (_topGlow != null) {
            needsInvalidate = needsInvalidate or _topGlow!!.onRelease()
        }
        if (_rightGlow != null) {
            needsInvalidate = needsInvalidate or _rightGlow!!.onRelease()
        }
        if (_bottomGlow != null) {
            needsInvalidate = needsInvalidate or _bottomGlow!!.onRelease()
        }
        if (needsInvalidate) {
            ViewCompat.postInvalidateOnAnimation(this)
        }
    }

    fun considerReleasingGlowsOnScroll(dx: Int, dy: Int) {
        var needsInvalidate = false
        if (_leftGlow != null && !_leftGlow!!.isFinished() && dx > 0) {
            needsInvalidate = _leftGlow!!.onRelease()
        }
        if (_rightGlow != null && !_rightGlow!!.isFinished() && dx < 0) {
            needsInvalidate = needsInvalidate or _rightGlow!!.onRelease()
        }
        if (_topGlow != null && !_topGlow!!.isFinished() && dy > 0) {
            needsInvalidate = needsInvalidate or _topGlow!!.onRelease()
        }
        if (_bottomGlow != null && !_bottomGlow!!.isFinished() && dy < 0) {
            needsInvalidate = needsInvalidate or _bottomGlow!!.onRelease()
        }
        if (needsInvalidate) {
            ViewCompat.postInvalidateOnAnimation(this)
        }
    }

    fun absorbGlows(velocityX: Int, velocityY: Int) {
        if (velocityX < 0) {
            ensureLeftGlow()
            _leftGlow?.onAbsorb(-velocityX)
        } else if (velocityX > 0) {
            ensureRightGlow()
            _rightGlow?.onAbsorb(velocityX)
        }
        if (velocityY < 0) {
            ensureTopGlow()
            _topGlow?.onAbsorb(-velocityY)
        } else if (velocityY > 0) {
            ensureBottomGlow()
            _bottomGlow?.onAbsorb(velocityY)
        }
        if (velocityX != 0 || velocityY != 0) {
            ViewCompat.postInvalidateOnAnimation(this);
        }
    }

    fun ensureLeftGlow() {
        if (_leftGlow != null) {
            return
        }
        _leftGlow = EdgeEffectCompat(getContext())
        if (_clipToPadding) {
            _leftGlow?.setSize(getMeasuredHeight() - getPaddingTop() - getPaddingBottom(), getMeasuredWidth() - getPaddingLeft() - getPaddingRight())
        } else {
            _leftGlow?.setSize(getMeasuredHeight(), getMeasuredWidth())
        }
    }

    fun ensureRightGlow() {
        if (_rightGlow != null) {
            return
        }
        _rightGlow = EdgeEffectCompat(getContext())
        if (_clipToPadding) {
            _rightGlow?.setSize(getMeasuredHeight() - getPaddingTop() - getPaddingBottom(), getMeasuredWidth() - getPaddingLeft() - getPaddingRight())
        } else {
            _rightGlow?.setSize(getMeasuredHeight(), getMeasuredWidth())
        }
    }

    fun ensureTopGlow() {
        if (_topGlow != null) {
            return
        }
        _topGlow = EdgeEffectCompat(getContext())
        if (_clipToPadding) {
            _topGlow?.setSize(getMeasuredWidth() - getPaddingLeft() - getPaddingRight(), getMeasuredHeight() - getPaddingTop() - getPaddingBottom())
        } else {
            _topGlow?.setSize(getMeasuredWidth(), getMeasuredHeight())
        }

    }

    fun ensureBottomGlow() {
        if (_bottomGlow != null) {
            return
        }
        _bottomGlow = EdgeEffectCompat(getContext())
        if (_clipToPadding) {
            _bottomGlow?.setSize(getMeasuredWidth() - getPaddingLeft() - getPaddingRight(), getMeasuredHeight() - getPaddingTop() - getPaddingBottom())
        } else {
            _bottomGlow?.setSize(getMeasuredWidth(), getMeasuredHeight())
        }
    }

    fun invalidateGlows() {
        _leftGlow = null
        _rightGlow = null
        _topGlow = null
        _bottomGlow = null
    }

    fun offsetChildrenHorizontal(dx: Int) {
        val childCount = _childHelper!!.getChildCount()
        for (i in 0..childCount - 1) {
            _childHelper?.getChildAt(i)?.offsetLeftAndRight(dx)
        }
    }

    fun setDataSetChangedAfterLayout() {
        if (_dataSetHasChangedAfterLayout) {
            return
        }
        _dataSetHasChangedAfterLayout = true
        val childCount = _childHelper!!.getUnfilteredChildCount()
        for (i in 0..childCount - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getUnfilteredChildAt(i))
            if (holder != null && !holder.shouldIgnore()) {
                holder.addFlags(ViewHolder.FLAG_ADAPTER_POSITION_UNKNOWN)
            }
        }
        _recycler.setAdapterPositionsAsUnknown()
    }

    fun supportsChangeAnimations(): Boolean = _itemAnimator != null && _itemAnimator!!.getSupportsChangeAnimations()

    fun removeAnimatingView(view: View?): Boolean {
        eatRequestLayout()
        val removed = _childHelper!!.removeViewIfHidden(view)
        if (removed) {
            val viewHolder = getChildViewHolderInt(view)
            _recycler.unscrapView(viewHolder)
            _recycler.recycleViewHolderInternal(viewHolder)
        }
        resumeRequestLayout(false)
        return removed
    }

    fun hasPendingAdapterUpdates(): Boolean = !_firstLayoutComplete || _dataSetHasChangedAfterLayout || _adapterHelper!!.hasPendingUpdates()

    open fun getLayoutManager(): LayoutManager? = _layout

    fun eatRequestLayout() {
        if (!_eatRequestLayout) {
            _eatRequestLayout = true
            if (!_layoutFrozen) {
                _layoutRequestEaten = false
            }
        }
    }

    fun smoothScrollBy(dx: Int, dy: Int) {
        if (_layout == null) {
            return
        }
        if (_layoutFrozen) {
            return
        }
        var ndx = dx
        var ndy = dy
        if (!_layout!!.canScrollHorizontally()) {
            ndx = 0
        }
        if (!_layout!!.canScrollVertically()) {
            ndy = 0
        }
        if (ndx != 0 || ndy != 0) {
            _viewFlinger.smoothScrollBy(ndx, ndy)
        }
    }

    fun getAdapter(): Adapter<*>? = _adapter

    fun resumeRequestLayout(performLayoutChildren: Boolean) {
        if (_eatRequestLayout) {
            if (performLayoutChildren && _layoutRequestEaten && !_layoutFrozen && _layout != null && _adapter != null) {
                dispatchLayout()
            }
            _eatRequestLayout = false
            if (!_layoutFrozen) {
                _layoutRequestEaten = false
            }
        }
    }

    fun assertInLayoutOrScroll(message: String?) {
        if (!isComputingLayout()) {
            if (message == null) {
                throw IllegalStateException("Cannot call this method unless RecyclerView is computing a layout or scrolling")
            }
            throw IllegalStateException(message)
        }
    }

    fun assertNotInLayoutOrScroll(message: String?) {
        if (isComputingLayout()) {
            if (message == null) {
                throw IllegalStateException("Cannot call this method while RecyclerView is computing a layout or scrolling")
            }
            throw IllegalStateException(message)
        }
    }

    fun getItemDecorInsetsForChild(child: View?): Rect? {
        val lp = child?.getLayoutParams() as LayoutParams
        if (!lp._insetsDirty) {
            return lp._decorInsets
        }
        val insets = lp._decorInsets
        insets.set(0, 0, 0, 0)
        val decorCount = _itemDecorations.size
        for (i in 0..decorCount - 1) {
            _tempRect.set(0, 0, 0, 0)
            _itemDecorations.get(i)?.getItemOffsets(_tempRect, child, this, _state)
            insets.left += _tempRect.left
            insets.top += _tempRect.top
            insets.right += _tempRect.right
            insets.bottom += _tempRect.bottom
        }
        lp._insetsDirty = false
        return insets
    }

    fun isComputingLayout(): Boolean = _layoutOrScrollCounter > 0

    fun getChildLayoutPosition(child: View?): Int {
        val holder = getChildViewHolderInt(child)
        return if (holder != null) holder.getLayoutPosition() else NO_POSITION
    }

    fun onEnterLayoutOrScroll() {
        _layoutOrScrollCounter++
    }

    fun markItemDecorInsetsDirty() {
        val childCount = _childHelper!!.getUnfilteredChildCount()
        for (i in 0..childCount - 1) {
            val child = _childHelper?.getUnfilteredChildAt(i)
            (child?.getLayoutParams() as LayoutParams)._insetsDirty = true
        }
        _recycler.markItemDecorInsetsDirty()
    }

    fun markKnownViewsInvalid() {
        val childCount = _childHelper!!.getUnfilteredChildCount()
        for (i in 0..childCount - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getUnfilteredChildAt(i))
            if (holder != null && !holder.shouldIgnore()) {
                holder.addFlags(ViewHolder.FLAG_UPDATE or ViewHolder.FLAG_INVALID)
            }
        }
        markItemDecorInsetsDirty()
        _recycler.markKnownViewsInvalid()
    }

    fun processAdapterUpdatesAndSetAnimationFlags() {
        if (_dataSetHasChangedAfterLayout) {
            _adapterHelper?.reset()
            markKnownViewsInvalid()
            _layout?.onItemsChanged(this)
        }
        if (_itemAnimator != null && _layout!!.supportsPredictiveItemAnimations()) {
            _adapterHelper?.preProcess()
        } else {
            _adapterHelper?.consumeUpdatesInOnePass()
        }
        val animationTypeSupported = (_itemsAddedOrRemoved && !_itemsChanged) || (_itemsAddedOrRemoved || (_itemsChanged && supportsChangeAnimations()))
        _state._runSimpleAnimations = _firstLayoutComplete && _itemAnimator != null && (_dataSetHasChangedAfterLayout || animationTypeSupported || _layout!!._requestedSimpleAnimations) && (!_dataSetHasChangedAfterLayout || _adapter!!.hasStableIds())
        _state._runPredictiveAnimations = _state._runSimpleAnimations && animationTypeSupported && !_dataSetHasChangedAfterLayout && predictiveItemAnimationsEnabled()
    }

    fun predictiveItemAnimationsEnabled(): Boolean = _itemAnimator != null && _layout!!.supportsPredictiveItemAnimations()

    fun findMinMaxChildLayoutPositions(into: IntArray?) {
        val count = _childHelper!!.getChildCount()
        if (count == 0) {
            into!![0] = 0
            into[1] = 0
            return
        }
        var minPositionPreLayout = Integer.MAX_VALUE
        var maxPositionPreLayout = Integer.MIN_VALUE
        for (i in 0..count - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getChildAt(i))
            if (holder!!.shouldIgnore()) {
                continue
            }
            val pos = holder.getLayoutPosition()
            if (pos < minPositionPreLayout) {
                minPositionPreLayout = pos
            }
            if (pos > maxPositionPreLayout) {
                maxPositionPreLayout = pos
            }
        }
        into!![0] = minPositionPreLayout
        into[1] = maxPositionPreLayout
    }

    fun isAnimating(): Boolean = _itemAnimator != null && _itemAnimator!!.isRunning()

    fun saveOldPositions() {
        val childCount = _childHelper!!.getUnfilteredChildCount()
        for (i in 0..childCount - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getUnfilteredChildAt(i))
            if (!holder!!.shouldIgnore()) {
                holder.saveOldPosition()
            }
        }
    }

    fun scrollToPosition(position: Int) {
        if (_layoutFrozen) {
            return
        }
        stopScroll()
        if (_layout == null) {
            return
        }
        _layout?.scrollToPosition(position)
        awakenScrollBars()
    }

    fun jumpToPositionForSmoothScroller(position: Int) {
        if (_layout == null) {
            return
        }
        _layout?.scrollToPosition(position)
        awakenScrollBars()
    }

    fun defaultOnMeasure(widthSpec: Int, heightSpec: Int) {
        val widthMode = MeasureSpec.getMode(widthSpec)
        val heightMode = MeasureSpec.getMode(heightSpec)
        val widthSize = MeasureSpec.getSize(widthSpec)
        val heightSize = MeasureSpec.getSize(heightSpec)
        var width = 0
        var height = 0
        when (widthMode) {
            MeasureSpec.EXACTLY, MeasureSpec.AT_MOST -> width = widthSize
            else -> width = ViewCompat.getMinimumWidth(this)
        }
        when (heightMode) {
            MeasureSpec.EXACTLY, MeasureSpec.AT_MOST -> height = heightSize
            else -> height = ViewCompat.getMinimumHeight(this)
        }
        setMeasuredDimension(width, height)
    }

    fun clearOldPositions() {
        val childCount = _childHelper!!.getUnfilteredChildCount()
        for (i in 0..childCount - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getUnfilteredChildAt(i))
            if (!holder!!.shouldIgnore()) {
                holder.clearOldPosition()
            }
        }
        _recycler.clearOldPositions()
    }

    fun getChangedHolderKey(holder: ViewHolder?): Long = if (_adapter!!.hasStableIds()) holder!!.getItemId() else holder!!._position.toLong()

    fun animateAppearance(itemHolder: ViewHolder?, beforeBounds: Rect?, afterLeft: Int, afterTop: Int) {
        val newItemView = itemHolder?._itemView
        if (beforeBounds != null && (beforeBounds.left != afterLeft || beforeBounds.top != afterTop)) {
            itemHolder?.setIsRecyclable(false)
            if (_itemAnimator!!.animateMove(itemHolder, beforeBounds.left, beforeBounds.top, afterLeft, afterTop)) {
                postAnimationRunner()
            }
        } else {
            itemHolder?.setIsRecyclable(false)
            if (_itemAnimator!!.animateAdd(itemHolder)) {
                postAnimationRunner()
            }
        }
    }

    fun animateDisappearance(disappearingItem: ItemHolderInfo?) {
        val disappearingItemView = disappearingItem?.holder?._itemView
        addAnimatingView(disappearingItem?.holder)
        val oldLeft = disappearingItem!!.left
        val oldTop = disappearingItem.top
        val newLeft = disappearingItemView!!.getLeft()
        val newTop = disappearingItemView.getTop()
        if (!disappearingItem.holder!!.isRemoved() && (oldLeft != newLeft || oldTop != newTop)) {
            disappearingItem.holder?.setIsRecyclable(false)
            disappearingItemView.layout(newLeft, newTop, newLeft + disappearingItemView.getWidth(), newTop + disappearingItemView.getHeight())
            if (_itemAnimator!!.animateMove(disappearingItem.holder, oldLeft, oldTop, newLeft, newTop)) {
                postAnimationRunner()
            }
        } else {
            disappearingItem.holder?.setIsRecyclable(false)
            if (_itemAnimator!!.animateRemove(disappearingItem.holder)) {
                postAnimationRunner()
            }
        }
    }

    fun postAnimationRunner() {
        if (!_postedAnimatorRunner && _isAttached) {
            ViewCompat.postOnAnimation(this, _itemAnimatorRunner)
            _postedAnimatorRunner = true
        }
    }

    fun addAnimatingView(viewHolder: ViewHolder?) {
        val view = viewHolder?._itemView
        val alreadyParented = view?.getParent() == this
        _recycler.unscrapView(getChildViewHolder(view))
        if (viewHolder!!.isTmpDetached()) {
            _childHelper?.attachViewToParent(view, -1, view?.getLayoutParams(), true)
        } else if(!alreadyParented) {
            _childHelper?.addView(view, true)
        } else {
            _childHelper?.hide(view)
        }
    }

    fun animateChange(oldHolder: ViewHolder?, newHolder: ViewHolder?) {
        oldHolder?.setIsRecyclable(false)
        addAnimatingView(oldHolder)
        oldHolder?._shadowedHolder = newHolder
        _recycler.unscrapView(oldHolder)
        val fromLeft = oldHolder!!._itemView!!.getLeft()
        val fromTop = oldHolder._itemView!!.getTop()
        var toLeft: Int
        var toTop: Int
        if (newHolder == null || newHolder.shouldIgnore()) {
            toLeft = fromLeft
            toTop = fromTop
        } else {
            toLeft = newHolder._itemView!!.getLeft()
            toTop = newHolder._itemView!!.getTop()
            newHolder.setIsRecyclable(false)
            newHolder._shadowingHolder = oldHolder
        }
        if(_itemAnimator!!.animateChange(oldHolder, newHolder, fromLeft, fromTop, toLeft, toTop)) {
            postAnimationRunner()
        }
    }

    fun processDisappearingList(appearingViews: ArrayMap<View, Rect>?) {
        val disappearingList = _state._disappearingViewsInLayoutPass
        for (i in disappearingList.size - 1  downTo 0) {
            val view = disappearingList.get(i)
            val vh = getChildViewHolderInt(view)
            val info = _state._preLayoutHolderMap.remove(vh)
            if (!_state.isPreLayout()) {
                _state._postLayoutHolderMap.remove(vh)
            }
            if (appearingViews?.remove(view) != null) {
                _layout?.removeAndRecycleView(view, _recycler)
                continue
            }
            if (info != null) {
                animateDisappearance(info)
            } else {
                animateDisappearance(ItemHolderInfo(vh, view!!.getLeft(), view.getTop(), view.getRight(), view.getBottom()))
            }
        }
        disappearingList.clear()
    }

    fun dispatchLayout() {
        if (_adapter == null) {
            return
        }
        if (_layout == null) {
            return
        }
        _state._disappearingViewsInLayoutPass.clear()
        eatRequestLayout()
        onEnterLayoutOrScroll()
        processAdapterUpdatesAndSetAnimationFlags()
        _state._oldChangedHolders = if (_state._runSimpleAnimations && _itemsChanged && supportsChangeAnimations()) ArrayMap<Long, ViewHolder?>() else null
        _itemsAddedOrRemoved = false
        _itemsChanged = false
        var appearingViewInitialBounds: ArrayMap<View, Rect>? = null
        _state._inPreLayout = _state._runPredictiveAnimations
        _state._itemCount = _adapter!!.getItemCount()
        findMinMaxChildLayoutPositions(_minMaxLayoutPositions)
        if (_state._runSimpleAnimations) {
            _state._preLayoutHolderMap.clear()
            _state._postLayoutHolderMap.clear()
            val count = _childHelper!!.getChildCount()
            for (i in 0..count - 1) {
                val holder = getChildViewHolderInt(_childHelper?.getChildAt(i))
                if (holder!!.shouldIgnore() || (holder.isInvalid() && !_adapter!!.hasStableIds())) {
                    continue
                }
                val view = holder._itemView
                _state._preLayoutHolderMap.put(holder, ItemHolderInfo(holder, view!!.getLeft(), view.getTop(), view.getRight(), view.getBottom()))
            }
        }
        if (_state._runPredictiveAnimations) {
            saveOldPositions()
            if (_state._oldChangedHolders != null) {
                val count = _childHelper!!.getChildCount()
                for (i in 0..count - 1) {
                    val holder = getChildViewHolderInt(_childHelper?.getChildAt(i))
                    if (holder!!.isChanged() && !holder.isRemoved() && !holder.shouldIgnore()) {
                        val key = getChangedHolderKey(holder)
                        _state._oldChangedHolders?.put(key, holder)
                        _state._preLayoutHolderMap.remove(holder)
                    }
                }
            }
            val didStructureChange = _state._structureChanged
            _state._structureChanged = false
            _layout?.onLayoutChildren(_recycler, _state)
            _state._structureChanged = didStructureChange

            appearingViewInitialBounds = ArrayMap<View, Rect>()
            for (i in 0.._childHelper!!.getChildCount() - 1) {
                var found = false;
                val child = _childHelper?.getChildAt(i)
                if (getChildViewHolderInt(child)!!.shouldIgnore()) {
                    continue
                }
                for (j in 0.._state._preLayoutHolderMap.size -1) {
                    val holder = _state._preLayoutHolderMap.keyAt(j)
                    if (holder?._itemView == child) {
                        found = true
                        break
                    }
                }
                if (!found) {
                    appearingViewInitialBounds.put(child, Rect(child!!.getLeft(), child.getTop(), child.getRight(), child.getBottom()))
                }
            }
            clearOldPositions()
            _adapterHelper?.consumePostponedUpdates()
        } else {
            clearOldPositions()
            _adapterHelper?.consumeUpdatesInOnePass()
            if (_state._oldChangedHolders != null) {
                val count = _childHelper!!.getChildCount()
                for (i in 0..count - 1) {
                    val holder = getChildViewHolderInt(_childHelper?.getChildAt(i))
                    if (holder!!.isChanged() && !holder.isRemoved() && !holder.shouldIgnore()) {
                        val key = getChangedHolderKey(holder)
                        _state._oldChangedHolders?.put(key, holder)
                        _state._preLayoutHolderMap.remove(holder)
                    }
                }
            }
        }
        _state._itemCount = _adapter!!.getItemCount()
        _state._deletedInvisibleItemCountSincePreviousLayout = 0
        _state._inPreLayout = false
        _layout?.onLayoutChildren(_recycler, _state)
        _state._structureChanged = false
        _pendingSavedState = null
        _state._runSimpleAnimations = _state._runSimpleAnimations && _itemAnimator != null

        if (_state._runSimpleAnimations) {
            val newChangedHolders = if (_state._oldChangedHolders != null) ArrayMap<Long, ViewHolder?>() else null
            var count = _childHelper!!.getChildCount()
            for (i in 0..count - 1) {
                val holder = getChildViewHolderInt(_childHelper?.getChildAt(i))
                if (holder!!.shouldIgnore()) {
                    continue
                }
                val view = holder._itemView
                val key = getChangedHolderKey(holder);
                if (newChangedHolders != null && _state._oldChangedHolders?.get(key) != null) {
                    newChangedHolders.put(key, holder)
                } else {
                    _state._postLayoutHolderMap.put(holder, ItemHolderInfo(holder, view!!.getLeft(), view.getTop(), view.getRight(), view.getBottom()))
                }
            }
            processDisappearingList(appearingViewInitialBounds)
            var preLayoutCount = _state._preLayoutHolderMap.size
            for (i in preLayoutCount - 1 downTo 0) {
                val itemHolder = _state._preLayoutHolderMap.keyAt(i)
                if (!_state._postLayoutHolderMap.containsKey(itemHolder)) {
                    val disappearingItem = _state._preLayoutHolderMap.valueAt(i)
                    _state._preLayoutHolderMap.removeAt(i)
                    val disappearingItemView = disappearingItem?.holder?._itemView
                    _recycler.unscrapView(disappearingItem?.holder)
                    animateDisappearance(disappearingItem)
                }
            }
            var postLayoutCount = _state._postLayoutHolderMap.size
            if (postLayoutCount > 0) {
                for (i in postLayoutCount - 1 downTo 0) {
                    val itemHolder = _state._postLayoutHolderMap.keyAt(i)
                    val info = _state._postLayoutHolderMap.valueAt(i)
                    if ((_state._preLayoutHolderMap.isEmpty() || !_state._preLayoutHolderMap.containsKey(itemHolder))) {
                        _state._postLayoutHolderMap.removeAt(i)
                        val initialBounds = if (appearingViewInitialBounds != null) appearingViewInitialBounds.get(itemHolder?._itemView) else null
                        animateAppearance(itemHolder, initialBounds, info!!.left, info.top)
                    }
                }
            }
            count = _state._postLayoutHolderMap.size
            for (i in 0..count - 1) {
                val postHolder = _state._postLayoutHolderMap.keyAt(i)
                val postInfo = _state._postLayoutHolderMap.valueAt(i)
                val preInfo = _state._preLayoutHolderMap.get(postHolder)
                if (preInfo != null && postInfo != null) {
                    if (preInfo.left != postInfo.left || preInfo.top != postInfo.top) {
                        postHolder?.setIsRecyclable(false)
                        if (_itemAnimator!!.animateMove(postHolder, preInfo.left, preInfo.top, postInfo.left, postInfo.top)) {
                            postAnimationRunner()
                        }
                    }
                }
            }
            count = if (_state._oldChangedHolders != null) _state._oldChangedHolders!!.size else 0
            for (i in count - 1 downTo 0) {
                val key = _state._oldChangedHolders?.keyAt(i)
                val oldHolder = _state._oldChangedHolders?.get(key)
                val oldView = oldHolder?._itemView
                if (oldHolder!!.shouldIgnore()) {
                    continue
                }
                if (_recycler._changedScrap != null && _recycler._changedScrap!!.contains(oldHolder)) {
                    animateChange(oldHolder, newChangedHolders?.get(key))
                }
            }
        }
        resumeRequestLayout(false);
        _layout?.removeAndRecycleScrapInt(_recycler)
        _state._previousLayoutItemCount = _state._itemCount
        _dataSetHasChangedAfterLayout = false
        _state._runSimpleAnimations = false
        _state._runPredictiveAnimations = false
        onExitLayoutOrScroll()
        _layout?._requestedSimpleAnimations = false
        _recycler._changedScrap?.clear()
        _state._oldChangedHolders?.clear()
        if (didChildRangeChange(_minMaxLayoutPositions[0], _minMaxLayoutPositions[1])) {
            dispatchOnScrolled(0, 0)
        }
    }

    fun onExitLayoutOrScroll() {
        _layoutOrScrollCounter--
        if (_layoutOrScrollCounter < 1) {
            _layoutOrScrollCounter = 0
            dispatchContentChangedIfNecessary()
        }
    }

    fun offsetChildrenVertical(dy: Int) {
        val childCount = _childHelper!!.getChildCount()
        for (i in 0..childCount - 1) {
            _childHelper?.getChildAt(i)?.offsetTopAndBottom(dy)
        }
    }

    fun isAccessibilityEnabled(): Boolean = _accessibilityManager != null && _accessibilityManager!!.isEnabled()

    fun dispatchContentChangedIfNecessary() {
        val flags = _eatenAccessibilityChangeFlags
        _eatenAccessibilityChangeFlags = 0
        if (flags != 0 && isAccessibilityEnabled()) {
            val event = AccessibilityEvent.obtain()
            event.setEventType(AccessibilityEventCompat.TYPE_WINDOW_CONTENT_CHANGED)
            AccessibilityEventCompat.setContentChangeTypes(event, flags)
            sendAccessibilityEventUnchecked(event)
        }
    }

    fun didChildRangeChange(minPositionPreLayout: Int, maxPositionPreLayout: Int): Boolean {
        val count = _childHelper!!.getChildCount()
        if (count == 0) {
            return minPositionPreLayout != 0 || maxPositionPreLayout != 0
        }
        for (i in 0..count - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getChildAt(i))
            if (holder!!.shouldIgnore()) {
                continue
            }
            val pos = holder.getLayoutPosition()
            if (pos < minPositionPreLayout || pos > maxPositionPreLayout) {
                return true
            }
        }
        return false
    }

    fun getScrollState(): Int = _scrollState

    fun setScrollState(state: Int) {
        if (state == _scrollState) {
            return
        }
        _scrollState = state
        if (state != SCROLL_STATE_SETTLING) {
            stopScrollersInternal()
        }
        dispatchOnScrollStateChanged(state)
    }

    fun stopScroll() {
        setScrollState(SCROLL_STATE_IDLE)
        stopScrollersInternal()
    }

    fun stopScrollersInternal() {
        _viewFlinger.stop()
        _layout?.stopSmoothScroller()
    }

    open fun onScrollStateChanged(state: Int) { }

    fun dispatchOnScrollStateChanged(state: Int) {
        _layout?.onScrollStateChanged(state)
        onScrollStateChanged(state)
        _scrollListener?.onScrollStateChanged(this, state)
        if (_scrollListeners != null) {
            for (i in _scrollListeners!!.size - 1 downTo 0) {
                _scrollListeners?.get(i)?.onScrollStateChanged(this, state)
            }
        }
    }

    fun consumePendingUpdateOperations() {
        _updateChildViewsRunnable.run()
    }

    open fun onScrolled(dx: Int, dy: Int) { }

    fun dispatchOnScrolled(hresult: Int, vresult: Int) {
        val scrollX = getScrollX()
        val scrollY = getScrollY()
        onScrollChanged(scrollX, scrollY, scrollX, scrollY)
        onScrolled(hresult, vresult)
        _scrollListener?.onScrolled(this, hresult, vresult)
        if (_scrollListeners != null) {
            for (i in _scrollListeners!!.size - 1 downTo 0) {
                _scrollListeners!!.get(i)?.onScrolled(this, hresult, vresult)
            }
        }
    }

    fun getChildViewHolder(child: View?): ViewHolder? {
        val parent = child?.getParent()
        if (parent != null && parent != this) {
            throw IllegalArgumentException("View ${child} is not a direct child of ${this}")
        }
        return getChildViewHolderInt(child)
    }

    fun getAdapterPositionFor(viewHolder: ViewHolder?): Int {
        if (viewHolder!!.hasAnyOfTheFlags(ViewHolder.FLAG_INVALID or ViewHolder.FLAG_REMOVED or ViewHolder.FLAG_ADAPTER_POSITION_UNKNOWN) || !viewHolder.isBound()) {
            return RecyclerView.NO_POSITION
        }
        return _adapterHelper!!.applyPendingUpdatesToPosition(viewHolder._position)
    }

    interface ChildDrawingOrderCallback {
        fun onGetChildDrawingOrder(childCount: Int, i: Int): Int
    }

    abstract class ItemAnimator {

        var _listener: ItemAnimatorListener? = null
        var _finishedListeners = arrayListOf<ItemAnimatorFinishedListener?>()
        var _addDuration = 120L
        var _removeDuration = 120L
        var _moveDuration = 250L
        var _changeDuration = 250L
        var _supportsChangeAnimations = true

        fun getMoveDuration(): Long = _moveDuration
        fun setMoveDuration(moveDuration: Long) {
            _moveDuration = moveDuration
        }
        fun getAddDuration(): Long = _addDuration
        fun setAddDuration(addDuration: Long) {
            _addDuration = addDuration
        }
        fun getRemoveDuration(): Long = _removeDuration
        fun setRemoveDuration(removeDuration: Long) {
            _removeDuration = removeDuration
        }
        fun getChangeDuration(): Long = _changeDuration
        fun setChangeDuration(changeDuration: Long) {
            _changeDuration = changeDuration
        }
        fun getSupportsChangeAnimations(): Boolean = _supportsChangeAnimations
        fun setSupportsChangeAnimations(supportsChangeAnimations: Boolean) {
            _supportsChangeAnimations = supportsChangeAnimations
        }
        fun setListener(listener: ItemAnimatorListener?) {
            _listener = listener
        }
        abstract fun runPendingAnimations()
        abstract fun animateRemove(holder: ViewHolder?): Boolean
        abstract fun animateAdd(holder: ViewHolder?): Boolean
        abstract fun animateMove(holder: ViewHolder?, fromX: Int, fromY: Int, toX: Int, toY: Int): Boolean
        abstract fun animateChange(oldHolder: ViewHolder?, newHolder: ViewHolder?, fromLeft: Int, fromTop: Int, toLeft: Int, toTop: Int): Boolean
        fun dispatchRemoveFinished(item: ViewHolder?) {
            onRemoveFinished(item)
            _listener?.onRemoveFinished(item)
        }
        fun dispatchMoveFinished(item: ViewHolder?) {
            onMoveFinished(item)
            _listener?.onMoveFinished(item)
        }
        fun dispatchAddFinished(item: ViewHolder?) {
            onAddFinished(item)
            _listener?.onAddFinished(item)
        }
        fun dispatchChangeFinished(item: ViewHolder?, oldItem: Boolean) {
            onChangeFinished(item, oldItem)
            _listener?.onChangeFinished(item)
        }
        fun dispatchRemoveStarting(item: ViewHolder?) {
            onRemoveStarting(item)
        }
        fun dispatchMoveStarting(item: ViewHolder?) {
            onMoveStarting(item)
        }
        fun dispatchAddStarting(item: ViewHolder?) {
            onAddStarting(item)
        }
        fun dispatchChangeStarting(item: ViewHolder?, oldItem: Boolean) {
            onChangeStarting(item, oldItem)
        }
        abstract fun endAnimation(item: ViewHolder?)
        abstract fun endAnimations()
        abstract fun isRunning(): Boolean
        fun isRunning(listener: ItemAnimatorFinishedListener?): Boolean {
            val running = isRunning()
            if (listener != null) {
                if (!running) {
                    listener.onAnimationsFinished()
                } else {
                    _finishedListeners.add(listener)
                }
            }
            return running
        }
        interface ItemAnimatorListener {
            fun onRemoveFinished(item: ViewHolder?)
            fun onAddFinished(item: ViewHolder?)
            fun onMoveFinished(item: ViewHolder?)
            fun onChangeFinished(item: ViewHolder?)
        }
        fun dispatchAnimationsFinished() {
            val count = _finishedListeners.size
            for (i in 0..count - 1) {
                _finishedListeners[i]?.onAnimationsFinished()
            }
            _finishedListeners.clear()
        }
        interface ItemAnimatorFinishedListener {
            fun onAnimationsFinished()
        }
        open fun onRemoveStarting(item: ViewHolder?) { }
        open fun onRemoveFinished(item: ViewHolder?) { }
        open fun onAddStarting(item: ViewHolder?) { }
        open fun onAddFinished(item: ViewHolder?) {}
        open fun onMoveStarting(item: ViewHolder?) { }
        open fun onMoveFinished(item: ViewHolder?) { }
        open fun onChangeStarting(item: ViewHolder?, oldItem: Boolean) { }
        open fun onChangeFinished(item: ViewHolder?, oldItem: Boolean) { }
    }

    inner class ItemAnimatorRestoreListener: ItemAnimator.ItemAnimatorListener {

        override fun onRemoveFinished(item: ViewHolder?) {
            item?.setIsRecyclable(true)
            if (!removeAnimatingView(item?._itemView) && item!!.isTmpDetached()) {
                removeDetachedView(item._itemView, false)
            }
        }

        override fun onAddFinished(item: ViewHolder?) {
            item?.setIsRecyclable(true)
            if (!item!!.shouldBeKeptAsChild()) {
                removeAnimatingView(item._itemView)
            }
        }

        override fun onMoveFinished(item: ViewHolder?) {
            item?.setIsRecyclable(true);
            if (!item!!.shouldBeKeptAsChild()) {
                removeAnimatingView(item._itemView)
            }
        }

        override fun onChangeFinished(item: ViewHolder?) {
            item?.setIsRecyclable(true)
            if (item?._shadowedHolder != null && item?._shadowingHolder == null) {
                item?._shadowedHolder = null
                item?.setFlags(ViewHolder.FLAG_CHANGED.inv(), item._flags)
            }
            item?._shadowingHolder = null
            if (!item!!.shouldBeKeptAsChild()) {
                removeAnimatingView(item._itemView)
            }
        }
    }

    abstract class ViewCacheExtension {
        abstract fun getViewForPositionAndType(recycler: Recycler?, position: Int, type: Int): View?
    }

    class RecycledViewPool {
        var _scrap = SparseArray<ArrayList<ViewHolder?>?>()
        var _maxScrap = SparseIntArray()
        var _attachCount = 0

        fun clear() = _scrap.clear()

        fun setMaxRecycledViews(viewType: Int, max: Int) {
            _maxScrap.put(viewType, max)
            val scrapHeap = _scrap.get(viewType)
            if (scrapHeap != null) {
                while (scrapHeap.size > max) {
                    scrapHeap.removeAt(scrapHeap.size - 1)
                }
            }
        }

        fun getRecycledView(viewType: Int): ViewHolder? {
            val scrapHeap = _scrap.get(viewType)
            if (scrapHeap != null && !scrapHeap.isEmpty()) {
                val index = scrapHeap.size - 1
                val scrap = scrapHeap[index]
                scrapHeap.removeAt(index)
                return scrap
            }
            return null
        }

        fun size(): Int {
            var count = 0
            for (i in 0.._scrap.size() - 1) {
                val viewHolders = _scrap.valueAt(i)
                if (viewHolders != null) {
                    count += viewHolders.size
                }
            }
            return count
        }

        fun putRecycledView(scrap: ViewHolder?) {
            val viewType = scrap!!.getItemViewType()
            val scrapHeap = getScrapHeapForType(viewType)
            if (_maxScrap.get(viewType) <= scrapHeap!!.size) {
                return
            }
            scrap.resetInternal()
            scrapHeap.add(scrap)
        }

        fun attach(adapter: Adapter<*>?) {
            _attachCount++
        }

        fun detach() {
            _attachCount--
        }

        fun onAdapterChanged(oldAdapter: Adapter<*>?, newAdapter: Adapter<*>?, compatibleWithPrevious: Boolean) {
            if (oldAdapter != null) {
                detach()
            }
            if (!compatibleWithPrevious && _attachCount == 0) {
                clear()
            }
            if (newAdapter != null) {
                attach(newAdapter)
            }
        }

        fun getScrapHeapForType(viewType: Int): MutableList<ViewHolder?>? {
            var scrap = _scrap.get(viewType)
            if (scrap == null) {
                scrap = arrayListOf<ViewHolder?>()
                _scrap.put(viewType, scrap)
                if (_maxScrap.indexOfKey(viewType) < 0) {
                    _maxScrap.put(viewType, DEFAULT_MAX_SCRAP)
                }
            }
            return scrap
        }
    }

    inner class Recycler {
        val _attachedScrap = arrayListOf<ViewHolder?>()
        var _changedScrap: MutableList<ViewHolder?>? = null
        val _cachedViews = arrayListOf<ViewHolder?>()
        val _unmodifiableAttachedScrap = Collections.unmodifiableList(_attachedScrap)
        var _viewCacheMax = DEFAULT_CACHE_SIZE
        var _recyclerPool: RecycledViewPool? = null
        var _viewCacheExtension: ViewCacheExtension? = null

        fun clear() {
            _attachedScrap.clear()
            recycleAndClearCachedViews()
        }

        fun setViewCacheSize(viewCount: Int) {
            _viewCacheMax = viewCount
            var i = _cachedViews.size - 1
            while (i >= 0 && _cachedViews.size > viewCount) {
                recycleCachedViewAt(i)
                --i
            }
        }

        fun getScrapList(): MutableList<ViewHolder?>? = _unmodifiableAttachedScrap

        fun validateViewHolderForOffsetPosition(holder: ViewHolder?): Boolean {
            if (holder!!.isRemoved()) {
                return true
            }
            if (holder._position < 0 || holder._position >= _adapter!!.getItemCount()) {
                throw IndexOutOfBoundsException("Inconsistency detected. Invalid view holder " + "adapter position ${holder}")
            }
            if (!_state.isPreLayout()) {
                val type = _adapter!!.getItemViewType(holder._position)
                if (type != holder.getItemViewType()) {
                    return false
                }
            }
            if (_adapter!!.hasStableIds()) {
                return holder.getItemId() == _adapter!!.getItemId(holder._position)
            }
            return true
        }

        fun bindViewToPosition(view: View?, position: Int) {
            val holder = getChildViewHolderInt(view) ?: throw IllegalArgumentException("The view does not have a ViewHolder. You cannot pass arbitrary views to this method, they should be created by the Adapter")
            val offsetPosition = _adapterHelper!!.findPositionOffset(position)
            if (offsetPosition < 0 || offsetPosition >= _adapter!!.getItemCount()) {
                throw IndexOutOfBoundsException("Inconsistency detected. Invalid item position ${position}(offset:${offsetPosition})." + "state:${_state.getItemCount()}")
            }
            holder._ownerRecyclerView = this@RecyclerView
            _adapter?.bindViewHolder(holder as Nothing?, offsetPosition)
            attachAccessibilityDelegate(view)
            if (_state.isPreLayout()) {
                holder._preLayoutPosition = position
            }
            val lp = holder._itemView?.getLayoutParams()
            var rvLayoutParams: LayoutParams
            if (lp == null) {
                rvLayoutParams = generateDefaultLayoutParams() as LayoutParams
                holder._itemView?.setLayoutParams(rvLayoutParams)
            } else if (!checkLayoutParams(lp)) {
                rvLayoutParams = generateLayoutParams(lp) as LayoutParams
                holder._itemView?.setLayoutParams(rvLayoutParams)
            } else {
                rvLayoutParams = lp as LayoutParams
            }
            rvLayoutParams._insetsDirty = true
            rvLayoutParams._viewHolder = holder
            rvLayoutParams._pendingInvalidate = holder._itemView?.getParent() == null
        }

        fun convertPreLayoutPositionToPostLayout(position: Int): Int {
            if (position < 0 || position >= _state.getItemCount()) {
                throw IndexOutOfBoundsException("invalid position " + position + ". State " + "item count is ${_state.getItemCount()}")
            }
            if (!_state.isPreLayout()) {
                return position
            }
            return _adapterHelper!!.findPositionOffset(position)
        }

        fun getViewForPosition(position: Int): View? {
            return getViewForPosition(position, false)
        }

        fun getViewForPosition(position: Int, dryRun: Boolean): View? {
            if (position < 0 || position >= _state.getItemCount()) {
                throw IndexOutOfBoundsException("Invalid item position ${position}(${position}). Item count:${_state.getItemCount()}")
            }
            var fromScrap = false
            var holder: ViewHolder? = null
            if (_state.isPreLayout()) {
                holder = getChangedScrapViewForPosition(position)
                fromScrap = holder != null
            }
            if (holder == null) {
                holder = getScrapViewForPosition(position, RecyclerView.INVALID_TYPE, dryRun)
                if (holder != null) {
                    if (!validateViewHolderForOffsetPosition(holder)) {
                        if (!dryRun) {
                            holder.addFlags(ViewHolder.FLAG_INVALID)
                            if (holder.isScrap()) {
                                removeDetachedView(holder._itemView, false)
                                holder.unScrap()
                            } else if (holder.wasReturnedFromScrap()) {
                                holder.clearReturnedFromScrapFlag()
                            }
                            recycleViewHolderInternal(holder)
                        }
                        holder = null
                    } else {
                        fromScrap = true
                    }
                }
            }
            if (holder == null) {
                val offsetPosition = _adapterHelper!!.findPositionOffset(position)
                if (offsetPosition < 0 || offsetPosition >= _adapter!!.getItemCount()) {
                    throw IndexOutOfBoundsException("Inconsistency detected. Invalid item position ${position}(offset:${offsetPosition})." + "state:${_state.getItemCount()}")
                }
                val type = _adapter!!.getItemViewType(offsetPosition)
                if (_adapter!!.hasStableIds()) {
                    holder = getScrapViewForId(_adapter!!.getItemId(offsetPosition), type, dryRun)
                    if (holder != null) {
                        holder._position = offsetPosition
                        fromScrap = true
                    }
                }
                if (holder == null && _viewCacheExtension != null) {
                    val view = _viewCacheExtension?.getViewForPositionAndType(this, position, type)
                    if (view != null) {
                        holder = getChildViewHolder(view)
                        if (holder == null) {
                            throw IllegalArgumentException("getViewForPositionAndType returned a view which does not have a ViewHolder")
                        } else if (holder.shouldIgnore()) {
                            throw IllegalArgumentException("getViewForPositionAndType returned a view that is ignored. You must call stopIgnoring before returning this view.")
                        }
                    }
                }
                if (holder == null) {
                    holder = getRecycledViewPool()?.getRecycledView(type)
                    if (holder != null) {
                        holder.resetInternal()
                        if (RecyclerView.FORCE_INVALIDATE_DISPLAY_LIST) {
                            invalidateDisplayListInt(holder)
                        }
                    }
                }
                if (holder == null) {
                    holder = _adapter?.createViewHolder(this@RecyclerView, type)
                }
            }
            var bound = false
            if (_state.isPreLayout() && holder!!.isBound()) {
                holder._preLayoutPosition = position
            } else if (!holder!!.isBound() || holder.needsUpdate() || holder.isInvalid()) {
                val offsetPosition = _adapterHelper!!.findPositionOffset(position)
                holder._ownerRecyclerView = this@RecyclerView
                _adapter?.bindViewHolder(holder as Nothing?, offsetPosition)
                attachAccessibilityDelegate(holder._itemView)
                bound = true
                if (_state.isPreLayout()) {
                    holder._preLayoutPosition = position
                }
            }
            val lp = holder._itemView?.getLayoutParams()
            var rvLayoutParams: LayoutParams
            if (lp == null) {
                rvLayoutParams = generateDefaultLayoutParams() as LayoutParams
                holder._itemView?.setLayoutParams(rvLayoutParams)
            } else if (!checkLayoutParams(lp)) {
                rvLayoutParams = generateLayoutParams(lp) as LayoutParams
                holder._itemView?.setLayoutParams(rvLayoutParams)
            } else {
                rvLayoutParams = lp as LayoutParams
            }
            rvLayoutParams._viewHolder = holder
            rvLayoutParams._pendingInvalidate = fromScrap && bound
            return holder._itemView
        }

        private fun attachAccessibilityDelegate(itemView: View?) {
            if (isAccessibilityEnabled()) {
                if (ViewCompat.getImportantForAccessibility(itemView) == ViewCompat.IMPORTANT_FOR_ACCESSIBILITY_AUTO) {
                    ViewCompat.setImportantForAccessibility(itemView, ViewCompat.IMPORTANT_FOR_ACCESSIBILITY_YES)
                }
                if (!ViewCompat.hasAccessibilityDelegate(itemView)) {
                    ViewCompat.setAccessibilityDelegate(itemView, _accessibilityDelegate?.getItemDelegate())
                }
            }
        }

        private fun invalidateDisplayListInt(holder: ViewHolder?) {
            if (holder?._itemView is ViewGroup) {
                invalidateDisplayListInt(holder?._itemView as ViewGroup?, false)
            }
        }

        private fun invalidateDisplayListInt(viewGroup: ViewGroup?, invalidateThis: Boolean) {
            for (i in viewGroup!!.childCount - 1 downTo 0) {
                val view = viewGroup.getChildAt(i)
                if (view is ViewGroup) {
                    invalidateDisplayListInt(view, true)
                }
            }
            if (!invalidateThis) {
                return
            }
            if (viewGroup.visibility == View.INVISIBLE) {
                viewGroup.visibility = View.VISIBLE
                viewGroup.visibility = View.INVISIBLE
            } else {
                val visibility = viewGroup.visibility
                viewGroup.visibility = View.INVISIBLE
                viewGroup.visibility = visibility
            }
        }

        fun recycleView(view: View?) {
            val holder = getChildViewHolderInt(view)
            if (holder!!.isTmpDetached()) {
                removeDetachedView(view, false)
            }
            if (holder.isScrap()) {
                holder.unScrap()
            } else if (holder.wasReturnedFromScrap()){
                holder.clearReturnedFromScrapFlag()
            }
            recycleViewHolderInternal(holder)
        }

        fun recycleViewInternal(view: View?) {
            recycleViewHolderInternal(getChildViewHolderInt(view))
        }

        fun recycleAndClearCachedViews() {
            val count = _cachedViews.size
            for (i in count - 1 downTo 0) {
                recycleCachedViewAt(i)
            }
            _cachedViews.clear()
        }

        fun recycleCachedViewAt(cachedViewIndex: Int) {
            val viewHolder = _cachedViews.get(cachedViewIndex)
            addViewHolderToRecycledViewPool(viewHolder)
            _cachedViews.removeAt(cachedViewIndex)
        }

        fun recycleViewHolderInternal(holder: ViewHolder?) {
            if (holder!!.isScrap() || holder._itemView?.parent != null) {
                throw IllegalArgumentException("Scrapped or attached views may not be recycled. isScrap:${holder.isScrap()} isAttached:${holder._itemView?.getParent() != null}")
            }
            if (holder.isTmpDetached()) {
                throw IllegalArgumentException("Tmp detached view should be removed from RecyclerView before it can be recycled: ${holder}")
            }
            if (holder.shouldIgnore()) {
                throw IllegalArgumentException("Trying to recycle an ignored view holder. You should first call stopIgnoringView(view) before calling recycle.")
            }
            val transientStatePreventsRecycling = holder.doesTransientStatePreventRecycling()
            val forceRecycle = _adapter != null && transientStatePreventsRecycling && _adapter!!.onFailedToRecycleView(holder as Nothing?)
            var cached = false
            var recycled = false
            if (forceRecycle || holder.isRecyclable()) {
                if (!holder.hasAnyOfTheFlags(ViewHolder.FLAG_INVALID or ViewHolder.FLAG_REMOVED or ViewHolder.FLAG_CHANGED or ViewHolder.FLAG_UPDATE)) {
                    val cachedViewSize = _cachedViews.size
                    if (cachedViewSize == _viewCacheMax && cachedViewSize > 0) {
                        recycleCachedViewAt(0)
                    }
                    if (cachedViewSize < _viewCacheMax) {
                        _cachedViews.add(holder)
                        cached = true
                    }
                }
                if (!cached) {
                    addViewHolderToRecycledViewPool(holder)
                    recycled = true
                }
            }
            _state.onViewRecycled(holder)
            if (!cached && !recycled && transientStatePreventsRecycling) {
                holder._ownerRecyclerView = null
            }
        }

        fun addViewHolderToRecycledViewPool(holder: ViewHolder?) {
            ViewCompat.setAccessibilityDelegate(holder?._itemView, null)
            dispatchViewRecycled(holder)
            holder?._ownerRecyclerView = null
            getRecycledViewPool()?.putRecycledView(holder)
        }

        fun quickRecycleScrapView(view: View?) {
            val holder = getChildViewHolderInt(view)
            holder?._scrapContainer = null
            holder?.clearReturnedFromScrapFlag()
            recycleViewHolderInternal(holder)
        }

        fun scrapView(view: View?) {
            val holder = getChildViewHolderInt(view)
            holder?.setScrapContainer(this)
            if (!holder!!.isChanged() || !supportsChangeAnimations()) {
                if (holder.isInvalid() && !holder.isRemoved() && !_adapter!!.hasStableIds()) {
                    throw IllegalArgumentException("Called scrap view with an invalid view. Invalid views cannot be reused from scrap, they should rebound from recycler pool.")
                }
                _attachedScrap.add(holder)
            } else {
                if (_changedScrap == null) {
                    _changedScrap = arrayListOf<ViewHolder?>()
                }
                _changedScrap?.add(holder)
            }
        }

        fun unscrapView(holder: ViewHolder?) {
            if (!holder!!.isChanged() || !supportsChangeAnimations() || _changedScrap == null) {
                _attachedScrap.remove(holder)
            } else {
                _changedScrap?.remove(holder)
            }
            holder._scrapContainer = null
            holder.clearReturnedFromScrapFlag()
        }

        fun getScrapCount(): Int = _attachedScrap.size

        fun getScrapViewAt(index: Int): View? = _attachedScrap.get(index)?._itemView

        fun clearScrap() = _attachedScrap.clear()

        fun getChangedScrapViewForPosition(position: Int): ViewHolder? {
            val changedScrapSize = _changedScrap!!.size
            if (_changedScrap == null || changedScrapSize == 0) {
                return null
            }
            for (i in 0..changedScrapSize - 1) {
                val holder = _changedScrap?.get(i)
                if (!holder!!.wasReturnedFromScrap() && holder.getLayoutPosition() == position) {
                    holder.addFlags(ViewHolder.FLAG_RETURNED_FROM_SCRAP)
                    return holder
                }
            }
            if (_adapter!!.hasStableIds()) {
                val offsetPosition = _adapterHelper!!.findPositionOffset(position)
                if (offsetPosition > 0 && offsetPosition < _adapter!!.getItemCount()) {
                    val id = _adapter!!.getItemId(offsetPosition)
                    for (i in 0..changedScrapSize - 1) {
                        val holder = _changedScrap?.get(i)
                        if (!holder!!.wasReturnedFromScrap() && holder.getItemId() == id) {
                            holder.addFlags(ViewHolder.FLAG_RETURNED_FROM_SCRAP)
                            return holder
                        }
                    }
                }
            }
            return null
        }

        fun getScrapViewForPosition(position: Int, type: Int, dryRun: Boolean): ViewHolder? {
            val scrapCount = _attachedScrap.size
            for (i in 0..scrapCount - 1) {
                val holder = _attachedScrap.get(i)
                if (!holder!!.wasReturnedFromScrap() && holder.getLayoutPosition() == position && !holder.isInvalid() && (_state._inPreLayout || !holder.isRemoved())) {
                    if (type != RecyclerView.INVALID_TYPE && holder.getItemViewType() != type) {
                        break
                    }
                    holder.addFlags(ViewHolder.FLAG_RETURNED_FROM_SCRAP)
                    return holder
                }
            }
            if (!dryRun) {
                val view = _childHelper?.findHiddenNonRemovedView(position, type)
                if (view != null) {
                    _itemAnimator?.endAnimation(getChildViewHolder(view))
                }
            }

            val cacheSize = _cachedViews.size
            for (i in 0..cacheSize - 1) {
                val holder = _cachedViews.get(i)
                if (!holder!!.isInvalid() && holder.getLayoutPosition() == position) {
                    if (!dryRun) {
                        _cachedViews.removeAt(i)
                    }
                    return holder
                }
            }
            return null
        }

        fun getScrapViewForId(id: Long, type: Int, dryRun: Boolean): ViewHolder? {
            val count = _attachedScrap.size
            for (i in count - 1 downTo 0) {
                val holder = _attachedScrap.get(i)
                if (holder!!.getItemId() == id && !holder.wasReturnedFromScrap()) {
                    if (type == holder.getItemViewType()) {
                        holder.addFlags(ViewHolder.FLAG_RETURNED_FROM_SCRAP)
                        if (holder.isRemoved()) {
                            if (!_state.isPreLayout()) {
                                holder.setFlags(ViewHolder.FLAG_UPDATE, ViewHolder.FLAG_UPDATE or ViewHolder.FLAG_INVALID or ViewHolder.FLAG_REMOVED)
                            }
                        }
                        return holder
                    } else if (!dryRun) {
                        _attachedScrap.removeAt(i)
                        removeDetachedView(holder._itemView, false)
                        quickRecycleScrapView(holder._itemView)
                    }
                }
            }
            val cacheSize = _cachedViews.size
            for (i in cacheSize - 1 downTo 0) {
                val holder = _cachedViews.get(i)
                if (holder!!.getItemId() == id) {
                    if (type == holder.getItemViewType()) {
                        if (!dryRun) {
                            _cachedViews.removeAt(i)
                        }
                        return holder
                    } else if (!dryRun) {
                        recycleCachedViewAt(i)
                    }
                }
            }
            return null
        }

        fun dispatchViewRecycled(holder: ViewHolder?) {
            _recyclerListener?.onViewRecycled(holder)
            _adapter?.onViewRecycled(holder as Nothing?)
            _state.onViewRecycled(holder)
        }

        fun onAdapterChanged(oldAdapter: Adapter<*>?, newAdapter: Adapter<*>?, compatibleWithPrevious: Boolean) {
            clear()
            getRecycledViewPool()?.onAdapterChanged(oldAdapter, newAdapter, compatibleWithPrevious)
        }

        fun offsetPositionRecordsForMove(from: Int, to: Int) {
            var start: Int
            var end: Int
            var inBetweenOffset: Int
            if (from < to) {
                start = from
                end = to
                inBetweenOffset = -1
            } else {
                start = to
                end = from
                inBetweenOffset = 1
            }
            val cachedCount = _cachedViews.size
            for (i in 0..cachedCount - 1) {
                val holder = _cachedViews.get(i)
                if (holder == null || holder._position < start || holder._position > end) {
                    continue
                }
                if (holder._position == from) {
                    holder.offsetPosition(to - from, false)
                } else {
                    holder.offsetPosition(inBetweenOffset, false)
                }
            }
        }

        fun offsetPositionRecordsForInsert(insertedAt: Int, count: Int) {
            val cachedCount = _cachedViews.size
            for (i in 0..cachedCount - 1) {
                val holder = _cachedViews.get(i)
                if (holder != null && holder.getLayoutPosition() >= insertedAt) {
                    holder.offsetPosition(count, true)
                }
            }
        }

        fun offsetPositionRecordsForRemove(removedFrom: Int, count: Int, applyToPreLayout: Boolean) {
            val removedEnd = removedFrom + count
            val cachedCount = _cachedViews.size
            for (i in cachedCount - 1 downTo 0) {
                val holder = _cachedViews.get(i)
                if (holder != null) {
                    if (holder.getLayoutPosition() >= removedEnd) {
                        holder.offsetPosition(-count, applyToPreLayout)
                    } else if (holder.getLayoutPosition() >= removedFrom) {
                        holder.addFlags(ViewHolder.FLAG_REMOVED)
                        recycleCachedViewAt(i)
                    }
                }
            }
        }

        fun setViewCacheExtension(extension: ViewCacheExtension?) {
            _viewCacheExtension = extension
        }

        fun setRecycledViewPool(pool: RecycledViewPool?) {
            _recyclerPool?.detach()
            _recyclerPool = pool
            if (pool != null) {
                _recyclerPool?.attach(getAdapter())
            }
        }

        fun getRecycledViewPool(): RecycledViewPool? {
            if (_recyclerPool == null) {
                _recyclerPool = RecycledViewPool()
            }
            return _recyclerPool
        }

        fun viewRangeUpdate(positionStart: Int, itemCount: Int) {
            val positionEnd = positionStart + itemCount
            val cachedCount = _cachedViews.size
            for (i in cachedCount - 1 downTo 0) {
                val holder = _cachedViews.get(i)
                if (holder == null) {
                    continue
                }
                val pos = holder.getLayoutPosition()
                if (pos >= positionStart && pos < positionEnd) {
                    holder.addFlags(ViewHolder.FLAG_UPDATE)
                    recycleCachedViewAt(i)
                }
            }
        }

        fun setAdapterPositionsAsUnknown() {
            val cachedCount = _cachedViews.size
            for (i in 0..cachedCount - 1) {
                val holder = _cachedViews.get(i)
                holder?.addFlags(ViewHolder.FLAG_ADAPTER_POSITION_UNKNOWN)
            }
        }

        fun markKnownViewsInvalid() {
            if (_adapter != null && _adapter!!.hasStableIds()) {
                val cachedCount = _cachedViews.size
                for (i in 0..cachedCount - 1) {
                    val holder = _cachedViews.get(i)
                    if (holder != null) {
                        holder.addFlags(ViewHolder.FLAG_UPDATE or ViewHolder.FLAG_INVALID)
                        holder.addChangePayload(null)
                    }
                }
            } else {
                recycleAndClearCachedViews()
            }
        }

        fun clearOldPositions() {
            val cachedCount = _cachedViews.size
            for (i in 0..cachedCount - 1) {
                val holder = _cachedViews.get(i)
                holder?.clearOldPosition()
            }
            val scrapCount = _attachedScrap.size
            for (i in 0..scrapCount - 1) {
                _attachedScrap.get(i)?.clearOldPosition()
            }
            if (_changedScrap != null) {
                val changedScrapCount = _changedScrap!!.size
                for (i in 0..changedScrapCount - 1) {
                    _changedScrap?.get(i)?.clearOldPosition()
                }
            }
        }

        fun markItemDecorInsetsDirty() {
            val cachedCount = _cachedViews.size
            for (i in 0..cachedCount - 1) {
                val holder = _cachedViews.get(i)
                val layoutParams = holder?._itemView?.getLayoutParams() as LayoutParams?
                layoutParams?._insetsDirty = true
            }
        }
    }

    abstract class AdapterDataObserver {
        open fun onChanged() { }
        open fun onItemRangeChanged(positionStart: Int, itemCount: Int) { }
        open fun onItemRangeChanged(positionStart: Int, itemCount: Int, payload: Any?) = onItemRangeChanged(positionStart, itemCount)
        open fun onItemRangeInserted(positionStart: Int, itemCount: Int) { }
        open fun onItemRangeRemoved(positionStart: Int, itemCount: Int) { }
        open fun onItemRangeMoved(fromPosition: Int, toPosition: Int, itemCount: Int) { }
    }

    class AdapterDataObservable: Observable<AdapterDataObserver>() {

        fun hasObservers(): Boolean = !mObservers.isEmpty()

        fun notifyChanged() {
            for (i in mObservers.size - 1 downTo 0) {
                mObservers.get(i).onChanged()
            }
        }

        fun notifyItemRangeChanged(positionStart: Int, itemCount: Int) {
            notifyItemRangeChanged(positionStart, itemCount, null)
        }

        fun notifyItemRangeChanged(positionStart: Int, itemCount: Int, payload: Any?) {
            for (i in mObservers.size - 1 downTo 0) {
                mObservers.get(i).onItemRangeChanged(positionStart, itemCount, payload)
            }
        }

        fun notifyItemRangeInserted(positionStart: Int, itemCount: Int) {
            for (i in mObservers.size - 1 downTo 0) {
               mObservers.get(i).onItemRangeInserted(positionStart, itemCount)
           }
        }

        fun notifyItemRangeRemoved(positionStart: Int, itemCount: Int) {
            for (i in mObservers.size - 1 downTo 0) {
                mObservers.get(i).onItemRangeRemoved(positionStart, itemCount)
            }
        }

        fun notifyItemMoved(fromPosition: Int, toPosition: Int) {
            for (i in mObservers.size - 1 downTo 0) {
                mObservers.get(i).onItemRangeMoved(fromPosition, toPosition, 1)
            }
        }
    }

    inner class RecyclerViewDataObserver: AdapterDataObserver() {

        override fun onChanged() {
            assertNotInLayoutOrScroll(null)
            if (_adapter!!.hasStableIds()) {
                _state._structureChanged = true
                setDataSetChangedAfterLayout()
            } else {
                _state._structureChanged = true
                setDataSetChangedAfterLayout()
            }
            if (!_adapterHelper!!.hasPendingUpdates()) {
                requestLayout()
            }
        }

        override fun onItemRangeChanged(positionStart: Int, itemCount: Int, payload: Any?) {
            assertNotInLayoutOrScroll(null);
            if (_adapterHelper!!.onItemRangeChanged(positionStart, itemCount, payload)) {
                triggerUpdateProcessor()
            }
        }

        override fun onItemRangeInserted(positionStart: Int, itemCount: Int) {
            assertNotInLayoutOrScroll(null);
            if (_adapterHelper!!.onItemRangeInserted(positionStart, itemCount)) {
                triggerUpdateProcessor()
            }
        }

        override fun onItemRangeRemoved(positionStart: Int, itemCount: Int) {
            assertNotInLayoutOrScroll(null);
            if (_adapterHelper!!.onItemRangeRemoved(positionStart, itemCount)) {
                triggerUpdateProcessor();
            }
        }

        override fun onItemRangeMoved(fromPosition: Int, toPosition: Int, itemCount: Int) {
            assertNotInLayoutOrScroll(null);
            if (_adapterHelper!!.onItemRangeMoved(fromPosition, toPosition, itemCount)) {
                triggerUpdateProcessor()
            }
        }

        fun triggerUpdateProcessor() {
            if (_postUpdatesOnAnimation && _hasFixedSize && _isAttached) {
                ViewCompat.postOnAnimation(this@RecyclerView, _updateChildViewsRunnable)
            } else {
                _adapterUpdateDuringMeasure = true
                requestLayout()
            }
        }
    }

    abstract class Adapter<VH: ViewHolder> {
        val _observable = RecyclerView.AdapterDataObservable()
        var _hasStableIds = false

        abstract fun onCreateViewHolder(parent: ViewGroup?, viewType: Int): VH?
        abstract fun onBindViewHolder(holder: VH?, position: Int)
        abstract fun getItemCount(): Int

        fun onBindViewHolder(holder: VH?, position: Int, payloads: MutableList<Any?>?) {
            onBindViewHolder(holder, position)
        }

        fun createViewHolder(parent: ViewGroup?, viewType: Int): VH? {
            TraceCompat.beginSection(RecyclerView.TRACE_CREATE_VIEW_TAG)
            val holder = onCreateViewHolder(parent, viewType)
            holder?._itemViewType = viewType
            TraceCompat.endSection()
            return holder
        }

        fun bindViewHolder(holder: VH?, position: Int) {
            holder?._position = position
            if (hasStableIds()) {
                holder?._itemId = getItemId(position)
            }
            holder?.setFlags(ViewHolder.FLAG_BOUND, ViewHolder.FLAG_BOUND or ViewHolder.FLAG_UPDATE or ViewHolder.FLAG_INVALID or ViewHolder.FLAG_ADAPTER_POSITION_UNKNOWN)
            TraceCompat.beginSection(RecyclerView.TRACE_BIND_VIEW_TAG)
            onBindViewHolder(holder, position, holder?.getUnmodifiedPayloads())
            holder?.clearPayload()
            TraceCompat.endSection()
        }

        open fun getItemViewType(position: Int): Int = 0

        fun setHasStableIds(hasStableIds: Boolean) {
            if (hasObservers()) {
                throw IllegalStateException("Cannot change whether this adapter has stable IDs while the adapter has registered observers.")
            }
            _hasStableIds = hasStableIds
        }

        open fun getItemId(position: Int): Long = RecyclerView.NO_ID

        fun hasStableIds(): Boolean = _hasStableIds

        open fun onViewRecycled(holder: VH?) { }

        open fun onFailedToRecycleView(holder: VH?): Boolean = false

        open fun onViewAttachedToWindow(holder: VH?) { }

        open fun onViewDetachedFromWindow(holder: VH?) { }

        fun hasObservers(): Boolean = _observable.hasObservers()

        fun registerAdapterDataObserver(observer: AdapterDataObserver?) {
            _observable.registerObserver(observer)
        }

        fun unregisterAdapterDataObserver(observer: AdapterDataObserver?) {
            _observable.unregisterObserver(observer)
        }

        open fun onAttachedToRecyclerView(recyclerView: RecyclerView?) { }

        open fun onDetachedFromRecyclerView(recyclerView: RecyclerView?) { }

        fun notifyDataSetChanged() = _observable.notifyChanged()

        fun notifyItemChanged(position: Int) = _observable.notifyItemRangeChanged(position, 1)

        fun notifyItemChanged(position: Int, payload: Any?) = _observable.notifyItemRangeChanged(position, 1, payload)

        fun notifyItemRangeChanged(positionStart: Int, itemCount: Int) = _observable.notifyItemRangeChanged(positionStart, itemCount)

        fun notifyItemRangeChanged(positionStart: Int, itemCount: Int, payload: Any?) = _observable.notifyItemRangeChanged(positionStart, itemCount, payload)

        fun notifyItemInserted(position: Int) = _observable.notifyItemRangeInserted(position, 1)

        fun notifyItemMoved(fromPosition: Int, toPosition: Int) = _observable.notifyItemMoved(fromPosition, toPosition)

        fun notifyItemRangeInserted(positionStart: Int, itemCount: Int) = _observable.notifyItemRangeInserted(positionStart, itemCount)

        fun notifyItemRemoved(position: Int) = _observable.notifyItemRangeRemoved(position, 1)

        fun notifyItemRangeRemoved(positionStart: Int, itemCount: Int) = _observable.notifyItemRangeRemoved(positionStart, itemCount)
    }

    abstract class ViewHolder {
        companion object {
            val FLAG_BOUND = 1 shl 0
            val FLAG_UPDATE = 1 shl 1
            val FLAG_INVALID = 1 shl 2
            val FLAG_REMOVED = 1 shl 3
            val FLAG_NOT_RECYCLABLE = 1 shl 4
            val FLAG_RETURNED_FROM_SCRAP = 1 shl 5
            val FLAG_CHANGED = 1 shl 6
            val FLAG_IGNORE = 1 shl 7
            val FLAG_TMP_DETACHED = 1 shl 8
            val FLAG_ADAPTER_POSITION_UNKNOWN = 1 shl 9
            val FLAG_ADAPTER_FULLUPDATE = 1 shl 10
            private val FULLUPDATE_PAYLOADS = Collections.EMPTY_LIST
        }


        var _itemView: View? = null
        var _position = RecyclerView.NO_POSITION
        var _oldPosition = RecyclerView.NO_POSITION
        var _itemId = RecyclerView.NO_ID
        var _itemViewType = RecyclerView.INVALID_TYPE
        var _preLayoutPosition = RecyclerView.NO_POSITION
        var _shadowedHolder: ViewHolder? = null
        var _shadowingHolder: ViewHolder? = null
        var _flags = 0
        var _payloads: MutableList<Any?>? = null
        var _unmodifiedPayloads: MutableList<Any?>? = null
        var _isRecyclableCount = 0
        var _scrapContainer: Recycler? = null
        var _wasImportantForAccessibilityBeforeHidden = ViewCompat.IMPORTANT_FOR_ACCESSIBILITY_AUTO
        var _ownerRecyclerView: RecyclerView? = null

        constructor(itemView: View?) {
            if (itemView == null) {
                throw IllegalArgumentException("itemView may not be null")
            }
            _itemView = itemView
        }

        fun flagRemovedAndOffsetPosition(newPosition: Int, offset: Int, applyToPreLayout: Boolean) {
            addFlags(ViewHolder.FLAG_REMOVED)
            offsetPosition(offset, applyToPreLayout)
            _position = newPosition
        }

        fun offsetPosition(offset: Int, applyToPreLayout: Boolean) {
            if (_oldPosition == RecyclerView.NO_POSITION) {
                _oldPosition = _position
            }
            if (_preLayoutPosition == RecyclerView.NO_POSITION) {
                _preLayoutPosition = _position
            }
            if (applyToPreLayout) {
                _preLayoutPosition += offset
            }
            _position += offset
            if (_itemView?.layoutParams != null) {
                (_itemView?.getLayoutParams() as LayoutParams)._insetsDirty = true
            }
        }

        fun clearOldPosition() {
            _oldPosition = RecyclerView.NO_POSITION
            _preLayoutPosition = RecyclerView.NO_POSITION
        }

        fun saveOldPosition() {
            if (_oldPosition == RecyclerView.NO_POSITION) {
                _oldPosition = _position
            }
        }

        fun shouldIgnore(): Boolean = (_flags and FLAG_IGNORE) != 0

        fun getPosition(): Int = if (_preLayoutPosition == RecyclerView.NO_POSITION) _position else _preLayoutPosition

        fun getLayoutPosition(): Int = if (_preLayoutPosition == RecyclerView.NO_POSITION) _position else _preLayoutPosition

        fun getAdapterPosition(): Int {
            if (_ownerRecyclerView == null) {
                return RecyclerView.NO_POSITION
            }
            return _ownerRecyclerView!!.getAdapterPositionFor(this)
        }

        fun getOldPosition(): Int = _oldPosition

        fun getItemId(): Long = _itemId

        fun getItemViewType(): Int = _itemViewType

        fun isScrap(): Boolean = _scrapContainer != null

        fun unScrap() {
            _scrapContainer?.unscrapView(this)
        }

        fun wasReturnedFromScrap(): Boolean = (_flags and FLAG_RETURNED_FROM_SCRAP) != 0

        fun clearReturnedFromScrapFlag() {
            _flags = _flags and FLAG_RETURNED_FROM_SCRAP.inv()
        }

        fun clearTmpDetachFlag() {
            _flags = _flags and FLAG_TMP_DETACHED.inv()
        }

        fun stopIgnoring() {
            _flags = _flags and FLAG_IGNORE.inv()
        }

        fun setScrapContainer(recycler: Recycler?) {
            _scrapContainer = recycler
        }

        fun isInvalid(): Boolean = (_flags and FLAG_INVALID) != 0

        fun needsUpdate(): Boolean = (_flags and FLAG_UPDATE) != 0

        fun isChanged(): Boolean = (_flags and FLAG_CHANGED) != 0

        fun isBound(): Boolean = (_flags and FLAG_BOUND) != 0

        fun isRemoved(): Boolean = (_flags and FLAG_REMOVED) != 0

        fun hasAnyOfTheFlags(flags: Int): Boolean = (_flags and flags) != 0

        fun isTmpDetached(): Boolean = (_flags and FLAG_TMP_DETACHED) != 0

        fun isAdapterPositionUnknown(): Boolean = (_flags and FLAG_ADAPTER_POSITION_UNKNOWN) != 0 || isInvalid()

        fun setFlags(flags: Int, mask: Int) {
            _flags = (_flags and mask.inv()) or (flags and mask)
        }

        fun addFlags(flags: Int) {
            _flags = _flags or flags
        }

        fun addChangePayload(payload: Any?) {
            if (payload == null) {
                addFlags(FLAG_ADAPTER_FULLUPDATE)
            } else if ((_flags and FLAG_ADAPTER_FULLUPDATE) == 0) {
                createPayloadsIfNeeded()
                _payloads?.add(payload)
            }
        }

        private fun createPayloadsIfNeeded() {
            if (_payloads == null) {
                _payloads = arrayListOf()
                _unmodifiedPayloads = Collections.unmodifiableList(_payloads)
            }
        }

        fun clearPayload() {
            _payloads?.clear()
            _flags = _flags and FLAG_ADAPTER_FULLUPDATE.inv()
        }

        fun getUnmodifiedPayloads(): MutableList<Any?>? {
            if ((_flags and FLAG_ADAPTER_FULLUPDATE) == 0) {
                if (_payloads == null || _payloads!!.size == 0) {
                    return FULLUPDATE_PAYLOADS
                }
                return _unmodifiedPayloads
            } else {
                return FULLUPDATE_PAYLOADS
            }
        }

        fun resetInternal() {
            _flags = 0
            _position = RecyclerView.NO_POSITION
            _oldPosition = RecyclerView.NO_POSITION
            _itemId = RecyclerView.NO_ID
            _preLayoutPosition = RecyclerView.NO_POSITION
            _isRecyclableCount = 0
            _shadowedHolder = null
            _shadowingHolder = null
            clearPayload()
            _wasImportantForAccessibilityBeforeHidden = ViewCompat.IMPORTANT_FOR_ACCESSIBILITY_AUTO
        }

        fun onEnteredHiddenState() {
            _wasImportantForAccessibilityBeforeHidden = ViewCompat.getImportantForAccessibility(_itemView)
            ViewCompat.setImportantForAccessibility(_itemView, ViewCompat.IMPORTANT_FOR_ACCESSIBILITY_NO_HIDE_DESCENDANTS)
        }

        fun onLeftHiddenState() {
            ViewCompat.setImportantForAccessibility(_itemView, _wasImportantForAccessibilityBeforeHidden)
            _wasImportantForAccessibilityBeforeHidden = ViewCompat.IMPORTANT_FOR_ACCESSIBILITY_AUTO
        }

        override fun toString(): String {
            val sb = StringBuilder("ViewHolder{${Integer.toHexString(hashCode())} position=${_position} id=${_itemId}, oldPos=${_oldPosition}, pLpos:${_preLayoutPosition}")
            if (isScrap()) {
                sb.append(" scrap")
            }
            if (isInvalid()) {
                sb.append(" invalid")
            }
            if (!isBound()) {
                sb.append(" unbound")
            }
            if (needsUpdate()) {
                sb.append(" update")
            }
            if (isRemoved()) {
                sb.append(" removed")
            }
            if (shouldIgnore()) {
                sb.append(" ignored")
            }
            if (isChanged()) {
                sb.append(" changed")
            }
            if (isTmpDetached()) {
                sb.append(" tmpDetached")
            }
            if (!isRecyclable()) {
                sb.append(" not recyclable(${_isRecyclableCount})")
            }
            if (isAdapterPositionUnknown()) {
                sb.append("undefined adapter position")
            }
            if (_itemView?.parent == null) {
                sb.append(" no parent")
            }
            sb.append("}")
            return sb.toString()
        }

        fun setIsRecyclable(recyclable: Boolean) {
            _isRecyclableCount = if (recyclable) _isRecyclableCount - 1 else _isRecyclableCount + 1
            if (_isRecyclableCount < 0) {
                _isRecyclableCount = 0
            } else if (!recyclable && _isRecyclableCount == 1) {
                _flags = _flags or FLAG_NOT_RECYCLABLE
            } else if (recyclable && _isRecyclableCount == 0) {
                _flags = _flags and FLAG_NOT_RECYCLABLE.inv()
            }
        }

        fun isRecyclable(): Boolean = (_flags and FLAG_NOT_RECYCLABLE) == 0 && !ViewCompat.hasTransientState(_itemView)

        fun shouldBeKeptAsChild(): Boolean = (_flags and FLAG_NOT_RECYCLABLE) != 0

        fun doesTransientStatePreventRecycling(): Boolean = (_flags and FLAG_NOT_RECYCLABLE) == 0 && ViewCompat.hasTransientState(_itemView)
    }

    class ItemHolderInfo {
        var holder: ViewHolder? = null
        var left = 0
        var top = 0
        var right = 0
        var bottom = 0

        constructor(holder: ViewHolder?, left: Int, top: Int, right: Int, bottom: Int) {
            this.holder = holder
            this.left = left
            this.top = top
            this.right = right
            this.bottom = bottom
        }
    }

    class State {
        var _targetPosition = RecyclerView.NO_POSITION
        var _preLayoutHolderMap = ArrayMap<ViewHolder?, ItemHolderInfo?>()
        var _postLayoutHolderMap = ArrayMap<ViewHolder?, ItemHolderInfo?>()
        var _oldChangedHolders: ArrayMap<Long, ViewHolder?>? = ArrayMap<Long, ViewHolder?>()
        var _disappearingViewsInLayoutPass = arrayListOf<View?>()
        var _data: SparseArray<Any?>? = null
        var _itemCount = 0
        var _previousLayoutItemCount = 0
        var _deletedInvisibleItemCountSincePreviousLayout = 0
        var _structureChanged = false
        var _inPreLayout = false
        var _runSimpleAnimations = false
        var _runPredictiveAnimations = false

        fun reset(): State? {
            _targetPosition = RecyclerView.NO_POSITION
            _data?.clear()
            _itemCount = 0
            _structureChanged = false
            return this
        }

        fun isPreLayout(): Boolean = _inPreLayout
        fun willRunPredictiveAnimations(): Boolean = _runPredictiveAnimations
        fun willRunSimpleAnimations(): Boolean = _runSimpleAnimations

        fun remove(resourceId: Int) {
            if (_data == null) {
                return
            }
            _data?.remove(resourceId)
        }

        fun <T> get(resourceId: Int): T? {
            if (_data == null) {
                return null
            }
            return _data?.get(resourceId) as T?
        }

        fun put(resourceId: Int, data: Any?) {
            if (_data == null) {
                _data = SparseArray()
            }
            _data?.put(resourceId, data)
        }

        fun getTargetScrollPosition(): Int = _targetPosition
        fun hasTargetScrollPosition(): Boolean = _targetPosition != RecyclerView.NO_POSITION
        fun didStructureChange(): Boolean = _structureChanged
        fun getItemCount(): Int = if (_inPreLayout) (_previousLayoutItemCount - _deletedInvisibleItemCountSincePreviousLayout) else _itemCount

        fun onViewRecycled(holder: ViewHolder?) {
            _preLayoutHolderMap.remove(holder)
            _postLayoutHolderMap.remove(holder)
            if (_oldChangedHolders != null) {
                removeFrom(_oldChangedHolders, holder)
            }
            _disappearingViewsInLayoutPass.remove(holder?._itemView)
        }

        fun onViewIgnored(holder: ViewHolder?) = onViewRecycled(holder)

        private fun removeFrom(holderMap: ArrayMap<Long, ViewHolder?>?, holder: ViewHolder?) {
            for (i in holderMap!!.size - 1 downTo 0) {
                if (holder == holderMap.valueAt(i)) {
                    holderMap.removeAt(i)
                    return
                }
            }
        }

        fun removeFromDisappearingList(child: View?) = _disappearingViewsInLayoutPass.remove(child)

        fun addToDisappearingList(child: View?) {
            if (!_disappearingViewsInLayoutPass.contains(child)) {
                _disappearingViewsInLayoutPass.add(child)
            }
        }

        override fun toString(): String {
            return "State{targetPosition=${_targetPosition}, preLayoutHolderMap=${_preLayoutHolderMap}, postLayoutHolderMap=${_postLayoutHolderMap}, data=${_data}, itemCount=${_itemCount}, previousLayoutItemCount=${_previousLayoutItemCount}, deletedInvisibleItemCountSincePreviousLayout=${_deletedInvisibleItemCountSincePreviousLayout}, structureChanged=${_structureChanged}, inPreLayout=${_inPreLayout}, runSimpleAnimations=${_runSimpleAnimations}, runPredictiveAnimations=${_runPredictiveAnimations}}"
        }
    }

    abstract class SmoothScroller {

        private var _targetPosition = RecyclerView.NO_POSITION
        private var _recyclerView: RecyclerView? = null
        private var _layoutManager: LayoutManager? = null
        private var _pendingInitialRun = false
        private var _running = false
        private var _targetView: View? = null
        private var _recyclingAction: Action? = null

        constructor() {
            _recyclingAction = Action(0, 0)
        }

        fun start(recyclerView: RecyclerView?, layoutManager: LayoutManager?) {
            _recyclerView = recyclerView
            _layoutManager = layoutManager
            if (_targetPosition == RecyclerView.NO_POSITION) {
                throw IllegalArgumentException("Invalid target position")
            }
            _recyclerView?._state?._targetPosition = _targetPosition
            _running = true;
            _pendingInitialRun = true;
            _targetView = findViewByPosition(getTargetPosition())
            onStart()
            _recyclerView?._viewFlinger?.postOnAnimation()
        }

        fun setTargetPosition(targetPosition: Int) {
            _targetPosition = targetPosition
        }

        fun getLayoutManager(): LayoutManager? = _layoutManager

        fun stop() {
            if (!_running) {
                return
            }
            onStop()
            _recyclerView?._state?._targetPosition = RecyclerView.NO_POSITION
            _targetView = null
            _targetPosition = RecyclerView.NO_POSITION
            _pendingInitialRun = false
            _running = false
            _layoutManager?.onSmoothScrollerStopped(this)
            _layoutManager = null
            _recyclerView = null
        }

        fun isPendingInitialRun(): Boolean = _pendingInitialRun
        fun isRunning(): Boolean = _running
        fun getTargetPosition(): Int = _targetPosition

        fun onAnimation(dx: Int, dy: Int) {
            val recyclerView = _recyclerView
            if (!_running || _targetPosition == RecyclerView.NO_POSITION || recyclerView == null) {
                stop()
            }
            _pendingInitialRun = false
            if (_targetView != null) {
                if (getChildPosition(_targetView) == _targetPosition) {
                    onTargetFound(_targetView, recyclerView?._state, _recyclingAction)
                    _recyclingAction?.runIfNecessary(recyclerView)
                    stop()
                } else {
                    _targetView = null
                }
            }
            if (_running) {
                onSeekTargetStep(dx, dy, recyclerView?._state, _recyclingAction)
                val hadJumpTarget = _recyclingAction!!.hasJumpTarget()
                _recyclingAction?.runIfNecessary(recyclerView)
                if (hadJumpTarget) {
                    if (_running) {
                        _pendingInitialRun = true
                        recyclerView?._viewFlinger?.postOnAnimation()
                    } else {
                        stop()
                    }
                }
            }
        }

        fun getChildPosition(view: View?): Int {
            return _recyclerView!!.getChildLayoutPosition(view)
        }

        fun getChildCount(): Int {
            return _recyclerView!!._layout!!.getChildCount()
        }

        fun findViewByPosition(position: Int): View? {
            return _recyclerView?._layout?.findViewByPosition(position)
        }

        fun instantScrollToPosition(position: Int) {
            _recyclerView?.scrollToPosition(position)
        }

        fun onChildAttachedToWindow(child: View?) {
            if (getChildPosition(child) == getTargetPosition()) {
                _targetView = child
            }
        }

        protected fun normalize(scrollVector: PointF?) {
            val magnitute = Math.sqrt((scrollVector!!.x * scrollVector.x + scrollVector.y * scrollVector.y).toDouble())
            scrollVector.x /= magnitute.toFloat()
            scrollVector.y /= magnitute.toFloat()
        }

        abstract fun onStart()
        abstract fun onStop()
        abstract fun onSeekTargetStep(dx: Int, dy: Int, state: State?, action: Action?)
        abstract fun onTargetFound(targetView: View?, state: State?, action: Action?)

        class Action {
            companion object {
                val UNDEFINED_DURATION = Integer.MIN_VALUE
            }
            var _dx = 0
            var _dy = 0
            var _duration = 0
            var _jumpToPosition = RecyclerView.NO_POSITION
            var _interpolator: Interpolator? = null
            var _changed = false;
            var _consecutiveUpdates = 0

            constructor(dx: Int, dy: Int): this(dx, dy, UNDEFINED_DURATION, null)
            constructor(dx: Int, dy: Int, duration: Int): this(dx, dy, duration, null)
            constructor(dx: Int, dy: Int, duration: Int, interpolator: Interpolator?) {
                _dx = dx
                _dy = dy
                _duration = duration
                _interpolator = interpolator
            }

            fun jumpTo(targetPosition: Int) {
                _jumpToPosition = targetPosition
            }
            fun hasJumpTarget(): Boolean = _jumpToPosition >= 0
            fun runIfNecessary(recyclerView: RecyclerView?) {
                if (_jumpToPosition >= 0) {
                    val position = _jumpToPosition
                    _jumpToPosition = RecyclerView.NO_POSITION
                    recyclerView?.jumpToPositionForSmoothScroller(position)
                    _changed = false
                    return
                }
                if (_changed) {
                    validate()
                    if (_interpolator == null) {
                        if (_duration == UNDEFINED_DURATION) {
                            recyclerView?._viewFlinger?.smoothScrollBy(_dx, _dy)
                        } else {
                            recyclerView?._viewFlinger?.smoothScrollBy(_dx, _dy, _duration)
                        }
                    } else {
                        recyclerView?._viewFlinger?.smoothScrollBy(_dx, _dy, _duration, _interpolator)
                    }
                    _consecutiveUpdates++
                    _changed = false
                } else {
                    _consecutiveUpdates = 0
                }
            }

            private fun validate() {
                if (_interpolator != null && _duration < 1) {
                    throw IllegalStateException("If you provide an interpolator, you must set a positive duration")
                } else if (_duration < 1) {
                    throw IllegalStateException("Scroll duration must be a positive number")
                }
            }

            fun getDx(): Int = _dx

            fun setDx(dx: Int) {
                _changed = true
                _dx = dx
            }

            fun getDy(): Int = _dy

            fun setDy(dy: Int) {
                _changed = true
                _dy = dy
            }

            fun getDuration(): Int = _duration

            fun setDuration(duration: Int) {
                _changed = true
                _duration = duration
            }

            fun getInterpolator(): Interpolator? = _interpolator

            fun setInterpolator(interpolator: Interpolator?) {
                _changed = true
                _interpolator = interpolator
            }

            fun update(dx: Int, dy: Int, duration: Int, interpolator: Interpolator?) {
                _dx = dx
                _dy = dy
                _duration = duration
                _interpolator = interpolator
                _changed = true
            }
        }
    }

    class LayoutParams: ViewGroup.MarginLayoutParams {
        var _viewHolder: ViewHolder? = null
        var _decorInsets = Rect()
        var _insetsDirty = true
        var _pendingInvalidate = false

        constructor(context: Context, attrs: AttributeSet?): super(context, attrs)
        constructor(width: Int, height: Int): super(width, height)
        constructor(source: ViewGroup.MarginLayoutParams?): super(source)
        constructor(source: ViewGroup.LayoutParams?): super(source)
        constructor(source: LayoutParams?): super(source as ViewGroup.LayoutParams)

        fun viewNeedsUpdate(): Boolean = _viewHolder!!.needsUpdate()
        fun isViewInvalid(): Boolean = _viewHolder!!.isInvalid()
        fun isItemRemoved(): Boolean = _viewHolder!!.isRemoved()
        fun isItemChanged(): Boolean = _viewHolder!!.isChanged()
        fun getViewPosition(): Int = _viewHolder!!.getPosition()
        fun getViewLayoutPosition(): Int = _viewHolder!!.getLayoutPosition()
        fun getViewAdapterPosition(): Int = _viewHolder!!.getAdapterPosition()
    }

    abstract class LayoutManager {
        companion object {
            fun getChildMeasureSpec(parentSize: Int, padding: Int, childDimension: Int, canScroll: Boolean): Int {
                val size = Math.max(0, parentSize - padding)
                var resultSize = 0
                var resultMode = 0
                if (canScroll) {
                    if (childDimension >= 0) {
                        resultSize = childDimension
                        resultMode = View.MeasureSpec.EXACTLY
                    } else {
                        resultSize = 0
                        resultMode = View.MeasureSpec.UNSPECIFIED
                    }
                } else {
                    if (childDimension >= 0) {
                        resultSize = childDimension
                        resultMode = View.MeasureSpec.EXACTLY
                    } else if (childDimension == ViewGroup.LayoutParams.MATCH_PARENT) {
                        resultSize = size
                        resultMode = View.MeasureSpec.EXACTLY;
                    } else if (childDimension == ViewGroup.LayoutParams.WRAP_CONTENT) {
                        resultSize = size
                        resultMode = View.MeasureSpec.AT_MOST;
                    }
                }
                return View.MeasureSpec.makeMeasureSpec(resultSize, resultMode)
            }

            fun getProperties(context: Context, attrs: AttributeSet?, defStyleAttr: Int, defStyleRes: Int): Properties? {
                val properties = Properties()
                val a = context.obtainStyledAttributes(attrs, R.styleable.RecyclerView, defStyleAttr, defStyleRes)
                properties.orientation = a.getInt(R.styleable.RecyclerView_android_orientation, VERTICAL)
                properties.spanCount = a.getInt(R.styleable.RecyclerView_spanCount, 1)
                properties.reverseLayout = a.getBoolean(R.styleable.RecyclerView_reverseLayout, false)
                properties.stackFromEnd = a.getBoolean(R.styleable.RecyclerView_stackFromEnd, false)
                a.recycle()
                return properties
            }
        }

        var _childHelper: ChildHelper? = null
        var _recyclerView: RecyclerView? = null
        var _smoothScroller: SmoothScroller? = null
        var _requestedSimpleAnimations = false
        var _isAttachedToWindow = false

        fun setRecyclerView(recyclerView: RecyclerView?) {
            if (recyclerView == null) {
                _recyclerView = null
                _childHelper = null
            } else {
                _recyclerView = recyclerView
                _childHelper = recyclerView._childHelper
            }
        }

        fun requestLayout() {
            _recyclerView?.requestLayout()
        }

        open fun assertInLayoutOrScroll(message: String?) {
            _recyclerView?.assertInLayoutOrScroll(message)
        }

        open fun assertNotInLayoutOrScroll(message: String?) {
            _recyclerView?.assertNotInLayoutOrScroll(message)
        }

        open fun supportsPredictiveItemAnimations(): Boolean = false

        fun dispatchAttachedToWindow(view: RecyclerView?) {
            _isAttachedToWindow = true
            onAttachedToWindow(view)
        }

        fun dispatchDetachedFromWindow(view: RecyclerView?, recycler: Recycler?) {
            _isAttachedToWindow = false
            onDetachedFromWindow(view, recycler)
        }

        fun isAttachedToWindow(): Boolean = _isAttachedToWindow

        fun postOnAnimation(action: Runnable?) {
            if (_recyclerView != null) {
                ViewCompat.postOnAnimation(_recyclerView, action)
            }
        }

        fun removeCallbacks(action: Runnable?): Boolean {
            if (_recyclerView != null) {
                return _recyclerView!!.removeCallbacks(action)
            }
            return false
        }

        open fun onAttachedToWindow(view: RecyclerView?) { }
        open fun onDetachedFromWindow(view: RecyclerView?) { }

        open fun onDetachedFromWindow(view: RecyclerView?, recycler: Recycler?) = onDetachedFromWindow(view)

        fun getClipToPadding(): Boolean = _recyclerView != null && _recyclerView!!._clipToPadding

        open fun onLayoutChildren(recycler: Recycler?, state: State?) {

        }

        abstract fun generateDefaultLayoutParams(): LayoutParams?

        fun checkLayoutParams(lp: LayoutParams?): Boolean = lp != null

        fun generateLayoutParams(lp: ViewGroup.LayoutParams?): LayoutParams? = LayoutParams(lp)

        fun generateLayoutParams(context: Context, attrs: AttributeSet?): LayoutParams? = LayoutParams(context, attrs)

        open fun scrollHorizontallyBy(dx: Int, recycler: Recycler?, state: State?): Int {
            return 0
        }

        open fun scrollVerticallyBy(dy: Int, recycler: Recycler?, state: State?): Int {
            return 0
        }

        open fun canScrollHorizontally(): Boolean {
            return false
        }

        open fun canScrollVertically(): Boolean {
            return false
        }

        open public fun scrollToPosition(position: Int) { }

        open public fun smoothScrollToPosition(recyclerView: RecyclerView?, state: State?, position: Int) { }

        fun startSmoothScroll(smoothScroller: SmoothScroller?) {
            if (_smoothScroller != null && smoothScroller != _smoothScroller && _smoothScroller!!.isRunning()) {
                _smoothScroller?.stop()
            }
            _smoothScroller = smoothScroller
            _smoothScroller?.start(_recyclerView, this)
        }

        fun isSmoothScrolling(): Boolean = _smoothScroller != null && _smoothScroller!!.isRunning()

        fun getLayoutDirection(): Int = ViewCompat.getLayoutDirection(_recyclerView)

        fun endAnimation(view: View?) {
            _recyclerView?._itemAnimator?.endAnimation(getChildViewHolderInt(view))
        }

        fun addDisappearingView(child: View?) = addDisappearingView(child, -1)

        fun addDisappearingView(child: View?, index: Int) = addViewInt(child, index, true)

        fun addView(child: View?) = addView(child, -1)

        fun addView(child: View?, index: Int) = addViewInt(child, index, false)

        private fun addViewInt(child: View?, index: Int, disappearing: Boolean) {
            var nindex = index
            val holder = getChildViewHolderInt(child)
            if (disappearing || holder!!.isRemoved()) {
                _recyclerView?._state?.addToDisappearingList(child)
            } else {
                _recyclerView?._state?.removeFromDisappearingList(child)
            }
            val lp = child?.layoutParams as LayoutParams
            if (holder!!.wasReturnedFromScrap() || holder.isScrap()) {
                if (holder.isScrap()) {
                    holder.unScrap()
                } else {
                    holder.clearReturnedFromScrapFlag()
                }
                _childHelper?.attachViewToParent(child, nindex, child?.layoutParams, false)
                if (RecyclerView.DISPATCH_TEMP_DETACH) {
                    ViewCompat.dispatchFinishTemporaryDetach(child)
                }
            } else if (child?.getParent() == _recyclerView) {
                val currentIndex = _childHelper!!.indexOfChild(child)
                if (nindex == -1) {
                    nindex = _childHelper!!.getChildCount()
                }
                if (currentIndex == -1) {
                    throw IllegalStateException("Added View has RecyclerView as parent but view is not a real child. Unfiltered index:${_recyclerView!!.indexOfChild(child)}")
                }
                if (currentIndex != nindex) {
                    _recyclerView?._layout?.moveView(currentIndex, nindex)
                }
            } else {
                _childHelper?.addView(child, nindex, false)
                lp._insetsDirty = true
                if (_smoothScroller != null && _smoothScroller!!.isRunning()) {
                    _smoothScroller?.onChildAttachedToWindow(child)
                }
            }
            if (lp._pendingInvalidate) {
                holder._itemView?.invalidate()
                lp._pendingInvalidate = false
            }
        }

        fun removeView(child: View?) {
            _childHelper?.removeView(child)
        }

        fun removeViewAt(index: Int) {
            val child = getChildAt(index)
            if (child != null) {
                _childHelper?.removeViewAt(index)
            }
        }

        fun removeAllViews() {
            val childCount = getChildCount()
            for (i in childCount - 1 downTo  0) {
                _childHelper?.removeViewAt(i)
            }
        }

        open fun getBaseline(): Int = -1

        fun getPosition(view: View?): Int = (view?.getLayoutParams() as LayoutParams).getViewLayoutPosition()

        fun getItemViewType(view: View?): Int = getChildViewHolderInt(view)!!.getItemViewType()

        open fun findViewByPosition(position: Int): View? {
            val childCount = getChildCount()
            for (i in 0..childCount - 1) {
                val child = getChildAt(i)
                val vh = getChildViewHolderInt(child) ?: continue
                if (vh.getLayoutPosition() == position && !vh.shouldIgnore() && (_recyclerView!!._state.isPreLayout() || !vh.isRemoved())) {
                    return child
                }
            }
            return null
        }

        fun detachView(child: View?) {
            val ind = _childHelper!!.indexOfChild(child)
            if (ind >= 0) {
                detachViewInternal(ind, child)
            }
        }

        fun detachViewAt(index: Int) {
            detachViewInternal(index, getChildAt(index))
        }

        private fun detachViewInternal(index: Int, view: View?) {
            if (RecyclerView.DISPATCH_TEMP_DETACH) {
                ViewCompat.dispatchStartTemporaryDetach(view)
            }
            _childHelper?.detachViewFromParent(index)
        }

        fun attachView(child: View?, index: Int, lp: LayoutParams?) {
            val vh = getChildViewHolderInt(child)
            if (vh!!.isRemoved()) {
                _recyclerView?._state?.addToDisappearingList(child)
            } else {
                _recyclerView?._state?.removeFromDisappearingList(child)
            }
            _childHelper?.attachViewToParent(child, index, lp, vh.isRemoved())
            if (RecyclerView.DISPATCH_TEMP_DETACH)  {
                ViewCompat.dispatchFinishTemporaryDetach(child)
            }
        }

        fun attachView(child: View?, index: Int) {
            attachView(child, index, child?.getLayoutParams() as LayoutParams)
        }

        fun attachView(child: View?) {
            attachView(child, -1)
        }

        fun removeDetachedView(child: View?) {
            _recyclerView?.removeDetachedView(child, false)
        }

        fun moveView(fromIndex: Int, toIndex: Int) {
            val view = getChildAt(fromIndex) ?: throw IllegalArgumentException("Cannot move a child from non-existing index:${fromIndex}")
            detachViewAt(fromIndex)
            attachView(view, toIndex)
        }

        fun detachAndScrapView(child: View?, recycler: Recycler?) {
            val index = _childHelper!!.indexOfChild(child)
            scrapOrRecycleView(recycler, index, child)
        }

        fun detachAndScrapViewAt(index: Int, recycler: Recycler?) {
            val child = getChildAt(index)
            scrapOrRecycleView(recycler, index, child)
        }

        fun removeAndRecycleView(child: View?, recycler: Recycler?) {
            removeView(child)
            recycler?.recycleView(child)
        }

        fun removeAndRecycleViewAt(index: Int, recycler: Recycler?) {
            val view = getChildAt(index)
            removeViewAt(index)
            recycler?.recycleView(view)
        }

        fun getChildCount(): Int = if (_childHelper != null) _childHelper!!.getChildCount() else 0

        fun getChildAt(index: Int): View? = if  (_childHelper != null) _childHelper?.getChildAt(index) else null

        fun getWidth(): Int = if (_recyclerView != null) _recyclerView!!.width else 0

        fun getHeight(): Int = if (_recyclerView != null) _recyclerView!!.height else 0

        fun getPaddingLeft(): Int = if (_recyclerView != null) _recyclerView!!.paddingLeft else 0

        fun getPaddingTop(): Int = if (_recyclerView != null) _recyclerView!!.paddingTop else 0

        fun getPaddingRight(): Int = if (_recyclerView != null) _recyclerView!!.paddingRight else 0

        fun getPaddingBottom(): Int = if (_recyclerView != null) _recyclerView!!.paddingBottom else 0

        fun getPaddingStart(): Int = if (_recyclerView != null) ViewCompat.getPaddingStart(_recyclerView) else 0

        fun getPaddingEnd(): Int = if (_recyclerView != null) ViewCompat.getPaddingEnd(_recyclerView) else 0

        fun isFocused(): Boolean = _recyclerView != null && _recyclerView!!.isFocused

        fun hasFocus(): Boolean = _recyclerView != null && _recyclerView!!.hasFocus()

        fun getFocusedChild(): View? {
            if (_recyclerView == null) {
                return null
            }
            val focused = _recyclerView!!.focusedChild
            if (focused == null || _childHelper!!.isHidden(focused)) {
                return null
            }
            return focused
        }

        fun getItemCount(): Int {
            val a = if (_recyclerView != null) _recyclerView!!.getAdapter() else null
            return if (a != null) a.getItemCount() else 0
        }

        fun offsetChildrenHorizontal(dx: Int) = _recyclerView?.offsetChildrenHorizontal(dx)

        fun offsetChildrenVertical(dy: Int) {
            _recyclerView?.offsetChildrenVertical(dy)
        }

        fun ignoreView(view: View?) {
            if (view?.getParent() != _recyclerView || _recyclerView!!.indexOfChild(view) == -1) {
                throw IllegalArgumentException("View should be fully attached to be ignored")
            }
            val vh = getChildViewHolderInt(view)
            vh?.addFlags(ViewHolder.FLAG_IGNORE)
            _recyclerView?._state?.onViewIgnored(vh)
        }

        fun stopIgnoringView(view: View?) {
            val vh = getChildViewHolderInt(view)
            vh?.stopIgnoring()
            vh?.resetInternal()
            vh?.addFlags(ViewHolder.FLAG_INVALID)
        }

        fun detachAndScrapAttachedViews(recycler: Recycler?) {
            val childCount = getChildCount()
            for (i in childCount - 1 downTo 0) {
                val v = getChildAt(i)
                scrapOrRecycleView(recycler, i, v)
            }
        }

        private fun scrapOrRecycleView(recycler: Recycler?, index: Int, view: View?) {
            val viewHolder = getChildViewHolderInt(view)
            if (viewHolder!!.shouldIgnore()) {
                return
            }
            if (viewHolder.isInvalid() && !viewHolder.isRemoved() && !viewHolder.isChanged() && !_recyclerView!!._adapter!!.hasStableIds()) {
                removeViewAt(index)
                recycler?.recycleViewHolderInternal(viewHolder)
            } else {
                detachViewAt(index)
                recycler?.scrapView(view)
            }
        }

        fun removeAndRecycleScrapInt(recycler: Recycler?) {
            val scrapCount = recycler!!.getScrapCount()
            for (i in scrapCount - 1 downTo 0) {
                val scrap = recycler.getScrapViewAt(i)
                val vh = getChildViewHolderInt(scrap)
                if (vh!!.shouldIgnore()) {
                    continue
                }
                vh.setIsRecyclable(false)
                if (vh.isTmpDetached()) {
                    _recyclerView?.removeDetachedView(scrap, false)
                }
                _recyclerView?._itemAnimator?.endAnimation(vh)
                vh.setIsRecyclable(true)
                recycler.quickRecycleScrapView(scrap)
            }
            recycler.clearScrap()
            if (scrapCount > 0) {
                _recyclerView?.invalidate()
            }
        }

        fun measureChild(child: View?, widthUsed: Int, heightUsed: Int) {
            val lp = child?.layoutParams as LayoutParams
            val insets = _recyclerView!!.getItemDecorInsetsForChild(child)!!
            var nwidthUsed = widthUsed
            var nheightUsed = heightUsed
            nwidthUsed += insets.left + insets.right
            nheightUsed += insets.top + insets.bottom
            val widthSpec = getChildMeasureSpec(getWidth(), getPaddingLeft() + getPaddingRight() + nwidthUsed, lp.width, canScrollHorizontally())
            val heightSpec = getChildMeasureSpec(getHeight(), getPaddingTop() + getPaddingBottom() + nheightUsed, lp.height, canScrollVertically())
            child?.measure(widthSpec, heightSpec)
        }

        fun measureChildWithMargins(child: View?, widthUsed: Int, heightUsed: Int) {
            val lp = child?.layoutParams as LayoutParams
            val insets = _recyclerView!!.getItemDecorInsetsForChild(child)!!
            var nwidthUsed = widthUsed
            var nheightUsed = heightUsed
            nwidthUsed += insets.left + insets.right
            nheightUsed += insets.top + insets.bottom
            val widthSpec = getChildMeasureSpec(getWidth(), getPaddingLeft() + getPaddingRight() + lp.leftMargin + lp.rightMargin + nwidthUsed, lp.width, canScrollHorizontally())
            val heightSpec = getChildMeasureSpec(getHeight(), getPaddingTop() + getPaddingBottom() + lp.topMargin + lp.bottomMargin + nheightUsed, lp.height, canScrollVertically())
            child?.measure(widthSpec, heightSpec)
        }


        fun getDecoratedMeasuredWidth(child: View?): Int {
            val insets = (child?.layoutParams as LayoutParams)._decorInsets
            return child!!.measuredWidth + insets.left + insets.right
        }

        fun getDecoratedMeasuredHeight(child: View?): Int {
            val insets = (child?.layoutParams as LayoutParams)._decorInsets
            return child!!.measuredHeight + insets.top + insets.bottom
        }

        fun layoutDecorated(child: View?, left: Int, top: Int, right: Int, bottom: Int) {
            val insets = (child?.layoutParams as LayoutParams)._decorInsets
            child?.layout(left + insets.left, top + insets.top, right - insets.right, bottom - insets.bottom)
        }

        fun getDecoratedLeft(child: View?): Int = child!!.getLeft() - getLeftDecorationWidth(child)

        fun getDecoratedTop(child: View?): Int = child!!.getTop() - getTopDecorationHeight(child)

        fun getDecoratedRight(child: View?): Int = child!!.getRight() + getRightDecorationWidth(child)

        fun getDecoratedBottom(child: View?): Int = child!!.getBottom() + getBottomDecorationHeight(child)

        fun calculateItemDecorationsForChild(child: View?, outRect: Rect?) {
            if (_recyclerView == null) {
                outRect?.set(0, 0, 0, 0)
                return
            }
            val insets = _recyclerView?.getItemDecorInsetsForChild(child)
            outRect?.set(insets)
        }

        fun getTopDecorationHeight(child: View?): Int = (child?.getLayoutParams() as LayoutParams)._decorInsets.top
        fun getBottomDecorationHeight(child: View?): Int = (child?.getLayoutParams() as LayoutParams)._decorInsets.bottom
        fun getLeftDecorationWidth(child: View?): Int = (child?.getLayoutParams() as LayoutParams)._decorInsets.left
        fun getRightDecorationWidth(child: View?): Int = (child?.getLayoutParams() as LayoutParams)._decorInsets.right
        open fun onFocusSearchFailed(focused: View?, direction: Int, recycler: Recycler?, state: State?): View? = null
        open fun onInterceptFocusSearch(focused: View?, direction: Int): View? = null

        fun requestChildRectangleOnScreen(parent: RecyclerView?, child: View?, rect: Rect?, immediate: Boolean): Boolean {
            val parentLeft = getPaddingLeft()
            val parentTop = getPaddingTop()
            val parentRight = getWidth() - getPaddingRight()
            val parentBottom = getHeight() - getPaddingBottom()
            val childLeft = child!!.left + rect!!.left
            val childTop = child.top + rect.top
            val childRight = childLeft + rect.width()
            val childBottom = childTop + rect.height()
            val offScreenLeft = Math.min(0, childLeft - parentLeft)
            val offScreenTop = Math.min(0, childTop - parentTop)
            val offScreenRight = Math.max(0, childRight - parentRight)
            val offScreenBottom = Math.max(0, childBottom - parentBottom)
            var dx: Int
            if (getLayoutDirection() == ViewCompat.LAYOUT_DIRECTION_RTL) {
                dx = if (offScreenRight != 0) offScreenRight else Math.max(offScreenLeft, childRight - parentRight)
            } else {
                dx = if (offScreenLeft != 0) offScreenLeft else Math.min(childLeft - parentLeft, offScreenRight)
            }
            val dy = if (offScreenTop != 0) offScreenTop else Math.min(childTop - parentTop, offScreenBottom)
            if (dx != 0 || dy != 0) {
                if (immediate) {
                    parent?.scrollBy(dx, dy)
                } else {
                    parent?.smoothScrollBy(dx, dy)
                }
                return true
            }
            return false
        }

        open fun onRequestChildFocus(parent: RecyclerView?, child: View?, focused: View?): Boolean = isSmoothScrolling() || parent!!.isComputingLayout()
        open fun onRequestChildFocus(parent: RecyclerView?, state: State?, child: View?, focused: View?): Boolean = onRequestChildFocus(parent, child, focused)
        open fun onAdapterChanged(oldAdapter: Adapter<*>?, newAdapter: Adapter<*>?) { }
        open fun onAddFocusables(recyclerView: RecyclerView?, views: MutableList<View?>?, direction: Int, focusableMode: Int): Boolean = false
        open fun onItemsChanged(recyclerView: RecyclerView?) { }
        open fun onItemsAdded(recyclerView: RecyclerView?, positionStart: Int, itemCount: Int) { }
        open fun onItemsRemoved(recyclerView: RecyclerView?, positionStart: Int, itemCount: Int) { }
        open fun onItemsUpdated(recyclerView: RecyclerView?, positionStart: Int, itemCount: Int) { }
        open fun onItemsUpdated(recyclerView: RecyclerView?, positionStart: Int, itemCount: Int, payload: Any?) = onItemsUpdated(recyclerView, positionStart, itemCount)
        open fun onItemsMoved(recyclerView: RecyclerView?, from: Int, to: Int, itemCount: Int) { }
        open fun computeHorizontalScrollExtent(state: State?): Int = 0
        open fun computeHorizontalScrollOffset(state: State?): Int = 0
        open fun computeHorizontalScrollRange(state: State?): Int = 0
        open fun computeVerticalScrollExtent(state: State?): Int = 0
        open fun computeVerticalScrollOffset(state: State?): Int = 0
        open fun computeVerticalScrollRange(state: State?): Int = 0
        open fun onMeasure(recycler: Recycler?, state: State?, widthSpec: Int, heightSpec: Int) = _recyclerView?.defaultOnMeasure(widthSpec, heightSpec)
        open fun setMeasuredDimension(widthSize: Int, heightSize: Int) = _recyclerView?.setMeasuredDimension(widthSize, heightSize)
        open fun getMinimumWidth(): Int = ViewCompat.getMinimumWidth(_recyclerView)
        open fun getMinimumHeight(): Int = ViewCompat.getMinimumHeight(_recyclerView)
        open fun onSaveInstanceState(): Parcelable? = null
        open fun onRestoreInstanceState(state: Parcelable?) { }

        fun stopSmoothScroller() {
            _smoothScroller?.stop()
        }

        fun onSmoothScrollerStopped(smoothScroller: SmoothScroller?) {
            if (_smoothScroller == smoothScroller) {
                _smoothScroller = null
            }
        }

        open fun onScrollStateChanged(state: Int) { }

        fun removeAndRecycleAllViews(recycler: Recycler?) {
            for (i in getChildCount() - 1  downTo 0) {
                val view = getChildAt(i)
                if (!getChildViewHolderInt(view)!!.shouldIgnore()) {
                    removeAndRecycleViewAt(i, recycler)
                }
            }
        }

        open fun onInitializeAccessibilityNodeInfo(info: AccessibilityNodeInfoCompat?) = onInitializeAccessibilityNodeInfo(_recyclerView?._recycler, _recyclerView?._state, info)

        open fun onInitializeAccessibilityNodeInfo(recycler: Recycler?, state: State?, info: AccessibilityNodeInfoCompat?) {
            if (ViewCompat.canScrollVertically(_recyclerView, -1) || ViewCompat.canScrollHorizontally(_recyclerView, -1)) {
                info?.addAction(AccessibilityNodeInfoCompat.ACTION_SCROLL_BACKWARD)
                info?.isScrollable = true
            }
            if (ViewCompat.canScrollVertically(_recyclerView, 1) || ViewCompat.canScrollHorizontally(_recyclerView, 1)) {
                info?.addAction(AccessibilityNodeInfoCompat.ACTION_SCROLL_FORWARD)
                info?.isScrollable = true
            }
            val collectionInfo= AccessibilityNodeInfoCompat.CollectionInfoCompat.obtain(
                    getRowCountForAccessibility(recycler, state),
                    getColumnCountForAccessibility(recycler, state),
                    isLayoutHierarchical(recycler, state),
                    getSelectionModeForAccessibility(recycler, state))
            info?.setCollectionInfo(collectionInfo)
        }

        open fun onInitializeAccessibilityEvent(event: AccessibilityEvent?) = onInitializeAccessibilityEvent(_recyclerView?._recycler, _recyclerView?._state, event)

        open fun onInitializeAccessibilityEvent(recycler: Recycler?, state: State?, event: AccessibilityEvent?) {
            val record = AccessibilityEventCompat.asRecord(event)
            if (_recyclerView == null || record == null) {
                return
            }
            record.isScrollable = ViewCompat.canScrollVertically(_recyclerView, 1)
                    || ViewCompat.canScrollVertically(_recyclerView, -1)
                    || ViewCompat.canScrollHorizontally(_recyclerView, -1)
                    || ViewCompat.canScrollHorizontally(_recyclerView, 1)
            if (_recyclerView?._adapter != null) {
                record.itemCount = _recyclerView!!._adapter!!.getItemCount()
            }
        }

        open fun onInitializeAccessibilityNodeInfoForItem(host: View?, info: AccessibilityNodeInfoCompat?) {
            val vh = getChildViewHolderInt(host)
            if (vh != null && !vh.isRemoved() && !_childHelper!!.isHidden(vh._itemView)) {
                onInitializeAccessibilityNodeInfoForItem(_recyclerView?._recycler, _recyclerView?._state, host, info)
            }
        }

        open fun onInitializeAccessibilityNodeInfoForItem(recycler: Recycler?, state: State?, host: View?, info: AccessibilityNodeInfoCompat?) {
            val rowIndexGuess = if (canScrollVertically()) getPosition(host) else 0
            val columnIndexGuess = if (canScrollHorizontally()) getPosition(host) else 0
            val itemInfo= AccessibilityNodeInfoCompat.CollectionItemInfoCompat.obtain(rowIndexGuess, 1, columnIndexGuess, 1, false, false)
            info?.setCollectionItemInfo(itemInfo)
        }

        fun requestSimpleAnimationsInNextLayout() {
            _requestedSimpleAnimations = true
        }

        fun getSelectionModeForAccessibility(recycler: Recycler?, state: State?): Int = AccessibilityNodeInfoCompat.CollectionInfoCompat.SELECTION_MODE_NONE

        fun getRowCountForAccessibility(recycler: Recycler?, state: State?): Int {
            if (_recyclerView == null || _recyclerView?._adapter == null) {
                return 1
            }
            return if (canScrollVertically()) _recyclerView!!._adapter!!.getItemCount() else 1
        }

        fun getColumnCountForAccessibility(recycler: Recycler?, state: State?): Int {
            if (_recyclerView == null || _recyclerView?._adapter == null) {
                return 1
            }
            return if (canScrollHorizontally()) _recyclerView!!._adapter!!.getItemCount() else 1
        }

        open fun isLayoutHierarchical(recycler: Recycler?, state: State?): Boolean = false

        fun performAccessibilityAction(action: Int, args: Bundle?): Boolean = performAccessibilityAction(_recyclerView?._recycler, _recyclerView?._state, action, args)

        fun performAccessibilityAction(recycler: Recycler?, state: State?, action: Int, args: Bundle?): Boolean {
            if (_recyclerView == null) {
                return false
            }
            var vScroll = 0
            var hScroll = 0
            when (action) {
                AccessibilityNodeInfoCompat.ACTION_SCROLL_BACKWARD -> {
                    if (ViewCompat.canScrollVertically(_recyclerView, -1)) {
                        vScroll = -(getHeight() - getPaddingTop() - getPaddingBottom())
                    }
                    if (ViewCompat.canScrollHorizontally(_recyclerView, -1)) {
                        hScroll = -(getWidth() - getPaddingLeft() - getPaddingRight())
                    }
                }
                AccessibilityNodeInfoCompat.ACTION_SCROLL_FORWARD -> {
                    if (ViewCompat.canScrollVertically(_recyclerView, 1)) {
                        vScroll = getHeight() - getPaddingTop() - getPaddingBottom()
                    }
                    if (ViewCompat.canScrollHorizontally(_recyclerView, 1)) {
                        hScroll = getWidth() - getPaddingLeft() - getPaddingRight()
                    }
                }
            }
            if (vScroll == 0 && hScroll == 0) {
                return false
            }
            _recyclerView?.scrollBy(hScroll, vScroll)
            return true
        }

        open fun performAccessibilityActionForItem(view: View?, action: Int, args: Bundle?): Boolean = performAccessibilityActionForItem(_recyclerView?._recycler, _recyclerView?._state, view, action, args)

        open fun performAccessibilityActionForItem(recycler: Recycler?, state: State?, view: View?, action: Int, args: Bundle?): Boolean = false

         class Properties {
            var orientation = 0
            var spanCount = 0
            var reverseLayout = false
            var stackFromEnd = false
        }
    }

    inner class ViewFlinger: Runnable {

        var _lastFlingX = 0
        var _lastFlingY = 0
        var _scroller: ScrollerCompat? = null
        var _interpolator = sQuinticInterpolator
        var _eatRunOnAnimationRequest = false
        var _reSchedulePostAnimationCallback = false

        constructor() {
            _scroller = ScrollerCompat.create(getContext(), sQuinticInterpolator)
        }

        fun disableRunOnAnimationRequests() {
            _reSchedulePostAnimationCallback = false
            _eatRunOnAnimationRequest = true
        }

        fun enableRunOnAnimationRequests() {
            _eatRunOnAnimationRequest = false;
            if (_reSchedulePostAnimationCallback) {
                postOnAnimation()
            }
        }

        fun postOnAnimation() {
            if (_eatRunOnAnimationRequest) {
                _reSchedulePostAnimationCallback = true
            } else {
                removeCallbacks(this)
                ViewCompat.postOnAnimation(this@RecyclerView, this)
            }
        }

        fun fling(velocityX: Int, velocityY: Int) {
            setScrollState(SCROLL_STATE_SETTLING)
            _lastFlingX = 0
            _lastFlingY = 0
            _scroller?.fling(0, 0, velocityX, velocityY, Integer.MIN_VALUE, Integer.MAX_VALUE, Integer.MIN_VALUE, Integer.MAX_VALUE)
            postOnAnimation()
        }

        fun smoothScrollBy(dx: Int, dy: Int) = smoothScrollBy(dx, dy, 0, 0)

        fun smoothScrollBy(dx: Int, dy: Int, vx: Int, vy: Int) = smoothScrollBy(dx, dy, computeScrollDuration(dx, dy, vx, vy))

        fun distanceInfluenceForSnapDuration(f: Float): Float {
            var nf = f - 0.5f
            nf *= (0.3f * Math.PI / 2.0f).toFloat()
            return Math.sin(nf.toDouble()).toFloat()
        }

        fun computeScrollDuration(dx: Int, dy: Int, vx: Int, vy: Int): Int {
            val absDx = Math.abs(dx)
            val absDy = Math.abs(dy)
            val horizontal = absDx > absDy
            val velocity = Math.sqrt((vx * vx + vy * vy).toDouble()).toInt()
            val delta = Math.sqrt((dx * dx + dy * dy).toDouble()).toInt()
            val containerSize = if (horizontal) getWidth() else getHeight()
            val halfContainerSize = containerSize / 2
            val distanceRatio = Math.min(1.0f, 1.0f * delta / containerSize)
            val distance = halfContainerSize + halfContainerSize * distanceInfluenceForSnapDuration(distanceRatio)
            var duration: Int
            if (velocity > 0) {
                duration = 4 * Math.round(1000 * Math.abs(distance / velocity)).toInt()
            } else {
                val absDelta = (if (horizontal) absDx else absDy).toFloat()
                duration = (((absDelta / containerSize) + 1) * 300).toInt()
            }
            return Math.min(duration, MAX_SCROLL_DURATION)
        }

        fun smoothScrollBy(dx: Int, dy: Int, duration: Int) = smoothScrollBy(dx, dy, duration, sQuinticInterpolator)

        fun smoothScrollBy(dx: Int, dy: Int, duration: Int, interpolator: Interpolator?) {
            if (_interpolator != interpolator) {
                _interpolator = interpolator!!
                _scroller = ScrollerCompat.create(getContext(), interpolator)
            }
            setScrollState(RecyclerView.SCROLL_STATE_SETTLING)
            _lastFlingX = 0
            _lastFlingY = 0
            _scroller?.startScroll(0, 0, dx, dy, duration)
            postOnAnimation()
        }

        fun stop() {
            removeCallbacks(this)
            _scroller?.abortAnimation()
        }

        override fun run() {
            disableRunOnAnimationRequests()
            consumePendingUpdateOperations()
            val scroller = _scroller
            val smoothScroller = _layout?._smoothScroller
            if (scroller!!.computeScrollOffset()) {
                val x = scroller.currX
                val y = scroller.currY
                val dx = x - _lastFlingX
                val dy = y - _lastFlingY
                var hresult = 0
                var vresult = 0
                _lastFlingX = x
                _lastFlingY = y
                var overscrollX = 0
                var overscrollY = 0
                if (_adapter != null) {
                    eatRequestLayout()
                    onEnterLayoutOrScroll()
                    TraceCompat.beginSection(TRACE_SCROLL_TAG)
                    if (dx != 0) {
                        hresult = _layout!!.scrollHorizontallyBy(dx, _recycler, _state)
                        overscrollX = dx - hresult
                    }
                    if (dy != 0) {
                        vresult = _layout!!.scrollVerticallyBy(dy, _recycler, _state)
                        overscrollY = dy - vresult
                    }
                    TraceCompat.endSection()
                    if (supportsChangeAnimations()) {
                        val count = _childHelper!!.getChildCount()
                        for (i in 0..count - 1) {
                            val view = _childHelper?.getChildAt(i)
                            val holder = getChildViewHolder(view)
                            if (holder != null && holder._shadowingHolder != null) {
                                val shadowingView = holder._shadowingHolder?._itemView
                                val left = view!!.left
                                val top = view.top
                                if (left != shadowingView!!.getLeft() || top != shadowingView.getTop()) {
                                    shadowingView.layout(left, top, left + shadowingView.getWidth(), top + shadowingView.getHeight())
                                }
                            }
                        }
                    }
                    onExitLayoutOrScroll()
                    resumeRequestLayout(false)
                    if (smoothScroller != null && !smoothScroller.isPendingInitialRun() && smoothScroller.isRunning()) {
                        val adapterSize = _state.getItemCount()
                        if (adapterSize == 0) {
                            smoothScroller.stop()
                        } else if (smoothScroller.getTargetPosition() >= adapterSize) {
                            smoothScroller.setTargetPosition(adapterSize - 1)
                            smoothScroller.onAnimation(dx - overscrollX, dy - overscrollY)
                        } else {
                            smoothScroller.onAnimation(dx - overscrollX, dy - overscrollY)
                        }
                    }
                }
                if (!_itemDecorations.isEmpty()) {
                    invalidate()
                }
                if (ViewCompat.getOverScrollMode(this@RecyclerView) != ViewCompat.OVER_SCROLL_NEVER) {
                    considerReleasingGlowsOnScroll(dx, dy)
                }
                if (overscrollX != 0 || overscrollY != 0) {
                    val vel = scroller.currVelocity.toInt()
                    var velX = 0
                    if (overscrollX != x) {
                        velX = if (overscrollX < 0) -vel else if (overscrollX > 0) vel else 0
                    }
                    var velY = 0
                    if (overscrollY != y) {
                        velY = if (overscrollY < 0) -vel else if (overscrollY > 0) vel else 0
                    }
                    if (ViewCompat.getOverScrollMode(this@RecyclerView) != ViewCompat.OVER_SCROLL_NEVER) {
                        absorbGlows(velX, velY)
                    }
                    if ((velX != 0 || overscrollX == x || scroller.finalX == 0) && (velY != 0 || overscrollY == y || scroller.finalY == 0)) {
                        scroller.abortAnimation()
                    }
                }
                if (hresult != 0 || vresult != 0) {
                    dispatchOnScrolled(hresult, vresult)
                }
                if (!awakenScrollBars()) {
                    invalidate()
                }
                val fullyConsumedVertical = dy != 0 && _layout!!.canScrollVertically() && vresult == dy
                val fullyConsumedHorizontal = dx != 0 && _layout!!.canScrollHorizontally() && hresult == dx
                val fullyConsumedAny = (dx == 0 && dy == 0) || fullyConsumedHorizontal || fullyConsumedVertical
                if (scroller.isFinished || !fullyConsumedAny) {
                    setScrollState(SCROLL_STATE_IDLE)
                } else {
                    postOnAnimation()
                }
            }
            if (smoothScroller != null) {
                if (smoothScroller.isPendingInitialRun()) {
                    smoothScroller.onAnimation(0, 0)
                }
                if (!_reSchedulePostAnimationCallback) {
                    smoothScroller.stop()
                }
            }
            enableRunOnAnimationRequests()
        }
    }

    abstract class ItemDecoration {

        open fun onDraw(c: Canvas?, parent: RecyclerView?, state: State?) = onDraw(c, parent)
        open fun onDraw(c: Canvas?, parent: RecyclerView?) { }
        open fun onDrawOver(c: Canvas?, parent: RecyclerView?, state: State?) = onDrawOver(c, parent)
        open fun onDrawOver(c: Canvas?, parent: RecyclerView?) { }
        open fun getItemOffsets(outRect: Rect?, itemPosition: Int, parent: RecyclerView?) = outRect?.set(0, 0, 0, 0)
        open fun getItemOffsets(outRect: Rect?, view: View?, parent: RecyclerView?, state: State?) = getItemOffsets(outRect, (view?.getLayoutParams() as LayoutParams).getViewLayoutPosition(), parent)
    }

    interface OnItemTouchListener {
        fun onInterceptTouchEvent(rv: RecyclerView?, e: MotionEvent?): Boolean
        fun onTouchEvent(rv: RecyclerView?, e: MotionEvent?)
        fun onRequestDisallowInterceptTouchEvent(disallowIntercept: Boolean)
    }

    class SimpleOnItemTouchListener: OnItemTouchListener {
        override fun onInterceptTouchEvent(rv: RecyclerView?, e: MotionEvent?): Boolean = false
        override fun onTouchEvent(rv: RecyclerView?, e: MotionEvent?) { }
        override fun onRequestDisallowInterceptTouchEvent(disallowIntercept: Boolean) { }
    }

    abstract class OnScrollListener {
        open fun onScrollStateChanged(recyclerView: RecyclerView?, newState: Int){ }
        open fun onScrolled(recyclerView: RecyclerView?, dx: Int, dy: Int){ }
    }

    interface RecyclerListener {
        fun onViewRecycled(holder: ViewHolder?)
    }

    interface OnChildAttachStateChangeListener {
        fun onChildViewAttachedToWindow(view: View?)
        fun onChildViewDetachedFromWindow(view: View?)
    }

    class SavedState: android.view.View.BaseSavedState {

        var _layoutState: Parcelable? = null

        constructor(src: Parcel?): super(src) {
            _layoutState = src?.readParcelable(LayoutManager.javaClass.classLoader)
        }

        constructor(superState: Parcelable?): super(superState)

        override fun writeToParcel(dest: Parcel?, flags: Int) {
            super.writeToParcel(dest, flags)
            dest?.writeParcelable(_layoutState, 0)
        }

        fun copyFrom(other: SavedState?) {
            _layoutState = other?._layoutState
        }

        companion object {
            val CREATOR = object: Parcelable.Creator<SavedState> {
                override fun createFromParcel(source: Parcel?): SavedState? = SavedState(source)
                override fun newArray(size: Int): Array<SavedState?>? = arrayOfNulls<SavedState>(size)
            }
        }

    }


    override fun onLayout(changed: Boolean, l: Int, t: Int, r: Int, b: Int) {
        eatRequestLayout()
        TraceCompat.beginSection(TRACE_ON_LAYOUT_TAG)
        dispatchLayout()
        TraceCompat.endSection()
        resumeRequestLayout(false)
        _firstLayoutComplete = true
    }

    override fun isNestedScrollingEnabled(): Boolean = _scrollingChildHelper!!.isNestedScrollingEnabled()

    override fun dispatchNestedPreScroll(dx: Int, dy: Int, consumed: IntArray?, offsetInWindow: IntArray?): Boolean = _scrollingChildHelper!!.dispatchNestedPreScroll(dx, dy, consumed, offsetInWindow)

    override fun stopNestedScroll() {
        _scrollingChildHelper?.stopNestedScroll()
    }

    override fun hasNestedScrollingParent(): Boolean = _scrollingChildHelper!!.hasNestedScrollingParent()

    override fun setNestedScrollingEnabled(enabled: Boolean) {
        _scrollingChildHelper?.setNestedScrollingEnabled(enabled)
    }

    override fun dispatchNestedFling(velocityX: Float, velocityY: Float, consumed: Boolean): Boolean = _scrollingChildHelper!!.dispatchNestedFling(velocityX, velocityY, consumed)

    override fun dispatchNestedScroll(dxConsumed: Int, dyConsumed: Int, dxUnconsumed: Int, dyUnconsumed: Int, offsetInWindow: IntArray?): Boolean = _scrollingChildHelper!!.dispatchNestedScroll(dxConsumed, dyConsumed, dxUnconsumed, dyUnconsumed, offsetInWindow)

    override fun dispatchNestedPreFling(velocityX: Float, velocityY: Float): Boolean = _scrollingChildHelper!!.dispatchNestedPreFling(velocityX, velocityY)

    override fun startNestedScroll(axes: Int): Boolean = _scrollingChildHelper!!.startNestedScroll(axes)


    fun dispatchOnItemTouchIntercept(e: MotionEvent?): Boolean {
        val action = e!!.getAction()
        if (action == MotionEvent.ACTION_CANCEL || action == MotionEvent.ACTION_DOWN) {
            _activeOnItemTouchListener = null
        }
        val listenerCount = _onItemTouchListeners.size
        for (i in 0..listenerCount - 1) {
            val listener = _onItemTouchListeners.get(i)
            if (listener!!.onInterceptTouchEvent(this, e) && action != MotionEvent.ACTION_CANCEL) {
                _activeOnItemTouchListener = listener
                return true
            }
        }
        return false
    }

    override fun onInterceptTouchEvent(e: MotionEvent?): Boolean {
        if (_layoutFrozen) {
            return false
        }
        if (dispatchOnItemTouchIntercept(e)) {
            cancelTouch()
            return true
        }
        if (_layout == null) {
            return false
        }
        val canScrollHorizontally = _layout!!.canScrollHorizontally()
        val canScrollVertically = _layout!!.canScrollVertically()
        if (_velocityTracker == null) {
            _velocityTracker = VelocityTracker.obtain()
        }
        _velocityTracker?.addMovement(e)
        val action = MotionEventCompat.getActionMasked(e)
        val actionIndex = MotionEventCompat.getActionIndex(e)
        when (action) {
            MotionEvent.ACTION_DOWN -> {
                if (_ignoreMotionEventTillDown) {
                    _ignoreMotionEventTillDown = false
                }
                _scrollPointerId = MotionEventCompat.getPointerId(e, 0)
                _initialTouchX = (e!!.getX() + 0.5f).toInt()
                _lastTouchX = _initialTouchX
                _initialTouchY = (e.getY() + 0.5f).toInt()
                _lastTouchY = _initialTouchY
                if (_scrollState == SCROLL_STATE_SETTLING) {
                    getParent().requestDisallowInterceptTouchEvent(true)
                    setScrollState(SCROLL_STATE_DRAGGING)
                }
                var nestedScrollAxis = ViewCompat.SCROLL_AXIS_NONE
                if (canScrollHorizontally) {
                    nestedScrollAxis = nestedScrollAxis or ViewCompat.SCROLL_AXIS_HORIZONTAL
                }
                if (canScrollVertically) {
                    nestedScrollAxis = nestedScrollAxis or ViewCompat.SCROLL_AXIS_VERTICAL
                }
                startNestedScroll(nestedScrollAxis)
            }
            MotionEventCompat.ACTION_POINTER_DOWN -> {
                _scrollPointerId = MotionEventCompat.getPointerId(e, actionIndex);
                _initialTouchX = (MotionEventCompat.getX(e, actionIndex) + 0.5f).toInt()
                _lastTouchX = _initialTouchX
                _initialTouchY = (MotionEventCompat.getY(e, actionIndex) + 0.5f).toInt()
                _lastTouchY = _initialTouchY
            }
            MotionEvent.ACTION_MOVE -> {
                val index = MotionEventCompat.findPointerIndex(e, _scrollPointerId)
                if (index < 0) {
                    return false
                }
                val x = (MotionEventCompat.getX(e, index) + 0.5f).toInt()
                val y = (MotionEventCompat.getY(e, index) + 0.5f).toInt()
                if (_scrollState != SCROLL_STATE_DRAGGING) {
                    val dx = x - _initialTouchX
                    val dy = y - _initialTouchY
                    var startScroll = false
                    if (canScrollHorizontally && Math.abs(dx) > _touchSlop) {
                        _lastTouchX = _initialTouchX + _touchSlop * (if (dx < 0) -1 else 1)
                        startScroll = true
                    }
                    if (canScrollVertically && Math.abs(dy) > _touchSlop) {
                        _lastTouchY = _initialTouchY + _touchSlop * (if (dy < 0) -1 else 1)
                        startScroll = true
                    }
                    if (startScroll) {
                        val parent = getParent()
                        if (parent != null) {
                            parent.requestDisallowInterceptTouchEvent(true)
                        }
                        setScrollState(SCROLL_STATE_DRAGGING)
                    }
                }
            }
            MotionEventCompat.ACTION_POINTER_UP -> onPointerUp(e)
            MotionEvent.ACTION_UP -> {
                _velocityTracker?.clear()
                stopNestedScroll()
            }
            MotionEvent.ACTION_CANCEL -> cancelTouch()
        }
        return _scrollState == SCROLL_STATE_DRAGGING
    }


    override fun computeHorizontalScrollExtent(): Int = if (_layout!!.canScrollHorizontally()) _layout!!.computeHorizontalScrollExtent(_state) else 0

    override fun computeHorizontalScrollOffset(): Int = if (_layout!!.canScrollHorizontally()) _layout!!.computeHorizontalScrollOffset(_state) else 0

    override fun computeHorizontalScrollRange(): Int = if (_layout!!.canScrollHorizontally()) _layout!!.computeHorizontalScrollRange(_state) else 0

    override fun computeVerticalScrollExtent(): Int = if (_layout!!.canScrollVertically()) _layout!!.computeVerticalScrollExtent(_state) else 0

    override fun computeVerticalScrollOffset(): Int = if (_layout!!.canScrollVertically()) _layout!!.computeVerticalScrollOffset(_state) else 0

    override fun computeVerticalScrollRange(): Int = if (_layout!!.canScrollVertically()) _layout!!.computeVerticalScrollRange(_state) else 0

    override fun getChildDrawingOrder(childCount: Int, i: Int): Int {
        if (_childDrawingOrderCallback == null) {
            return super.getChildDrawingOrder(childCount, i)
        } else {
            return _childDrawingOrderCallback!!.onGetChildDrawingOrder(childCount, i)
        }
    }

    public override fun drawChild(canvas: Canvas?, child: View?, drawingTime: Long): Boolean {
        return super.drawChild(canvas, child, drawingTime)
    }

    override fun onDraw(c: Canvas?) {
        super.onDraw(c);
        val count = _itemDecorations.size
        for (i in 0..count - 1) {
            _itemDecorations.get(i)?.onDraw(c, this, _state)
        }
    }

    override fun draw(c: Canvas?) {
        super.draw(c)
        val count = _itemDecorations.size
        for (i in 0..count - 1) {
            _itemDecorations.get(i)?.onDrawOver(c, this, _state)
        }
        var needsInvalidate = false
        if (_leftGlow != null && !_leftGlow!!.isFinished()) {
            val restore = c!!.save()
            val padding = if (_clipToPadding) getPaddingBottom() else 0
            c.rotate(270.0f)
            c.translate((-getHeight() + padding).toFloat(), 0.0f)
            needsInvalidate = _leftGlow != null && _leftGlow!!.draw(c)
            c.restoreToCount(restore)
        }
        if (_topGlow != null && !_topGlow!!.isFinished()) {
            val restore = c!!.save()
            if (_clipToPadding) {
                c.translate(getPaddingLeft().toFloat(), getPaddingTop().toFloat())
            }
            needsInvalidate = needsInvalidate or (_topGlow != null && _topGlow!!.draw(c))
            c.restoreToCount(restore)
        }
        if (_rightGlow != null && !_rightGlow!!.isFinished()) {
            val restore = c!!.save()
            val width = getWidth()
            val padding = if (_clipToPadding) getPaddingTop() else 0
            c.rotate(90.0f)
            c.translate(-padding.toFloat(), -width.toFloat())
            needsInvalidate = needsInvalidate or (_rightGlow != null && _rightGlow!!.draw(c))
            c.restoreToCount(restore)
        }
        if (_bottomGlow != null && !_bottomGlow!!.isFinished()) {
            val restore = c!!.save()
            c.rotate(180.0f)
            if (_clipToPadding) {
                c.translate((-getWidth() + getPaddingRight()).toFloat(), (-getHeight() + getPaddingBottom()).toFloat())
            } else {
                c.translate(-getWidth().toFloat(), -getHeight().toFloat())
            }
            needsInvalidate = needsInvalidate or (_bottomGlow != null && _bottomGlow!!.draw(c))
            c.restoreToCount(restore)
        }
        if (!needsInvalidate && _itemAnimator != null && _itemDecorations.size > 0 && _itemAnimator!!.isRunning()) {
            needsInvalidate = true
        }
        if (needsInvalidate) {
            ViewCompat.postInvalidateOnAnimation(this)
        }
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        if (_adapterUpdateDuringMeasure) {
            eatRequestLayout()
            processAdapterUpdatesAndSetAnimationFlags()
            if (_state._runPredictiveAnimations) {
                _state._inPreLayout = true
            } else {
                _adapterHelper?.consumeUpdatesInOnePass()
                _state._inPreLayout = false
            }
            _adapterUpdateDuringMeasure = false
            resumeRequestLayout(false)
        }
        if (_adapter != null) {
            _state._itemCount = _adapter!!.getItemCount()
        } else {
            _state._itemCount = 0
        }
        if (_layout == null) {
            defaultOnMeasure(widthMeasureSpec, heightMeasureSpec)
        } else {
            _layout?.onMeasure(_recycler, _state, widthMeasureSpec, heightMeasureSpec)
        }
        _state._inPreLayout = false
    }

    override fun onSizeChanged(w: Int, h: Int, oldw: Int, oldh: Int) {
        super.onSizeChanged(w, h, oldw, oldh)
        if (w != oldw || h != oldh) {
            invalidateGlows()
        }
    }

    override fun removeDetachedView(child: View?, animate: Boolean) {
        val vh = getChildViewHolderInt(child)
        if (vh != null) {
            if (vh.isTmpDetached()) {
                vh.clearTmpDetachFlag()
            } else if (!vh.shouldIgnore()) {
                throw IllegalArgumentException("Called removeDetachedView with a view which is not flagged as tmp detached.${vh}")
            }
        }
        dispatchChildDetached(child)
        super.removeDetachedView(child, animate)
    }

    override fun getBaseline(): Int {
        if (_layout != null) {
            return _layout!!.getBaseline()
        } else {
            return super.getBaseline()
        }
    }

    override fun onSaveInstanceState(): Parcelable? {
        val state = SavedState(super.onSaveInstanceState())
        if (_pendingSavedState != null) {
            state.copyFrom(_pendingSavedState)
        } else if (_layout != null) {
            state._layoutState = _layout?.onSaveInstanceState()
        } else {
            state._layoutState = null
        }
        return state
    }

    override fun onRestoreInstanceState(state: Parcelable?) {
        _pendingSavedState = state as SavedState?
        super.onRestoreInstanceState(_pendingSavedState?.getSuperState())
        if (_layout != null && _pendingSavedState?._layoutState != null) {
            _layout?.onRestoreInstanceState(_pendingSavedState?._layoutState)
        }
    }

    override fun dispatchSaveInstanceState(container: SparseArray<Parcelable>?) {
        dispatchFreezeSelfOnly(container)
    }

    override fun dispatchRestoreInstanceState(container: SparseArray<Parcelable>?) {
        dispatchThawSelfOnly(container)
    }

    override fun scrollTo(x: Int, y: Int) {
        throw UnsupportedOperationException("RecyclerView does not support scrolling to an absolute position.")
    }

    override fun scrollBy(x: Int, y: Int) {
        if (_layout == null) {
            return
        }
        if (_layoutFrozen) {
            return
        }
        val canScrollHorizontal = _layout!!.canScrollHorizontally()
        val canScrollVertical = _layout!!.canScrollVertically()
        if (canScrollHorizontal || canScrollVertical) {
            scrollByInternal(if (canScrollHorizontal)  x else 0, if (canScrollVertical) y else 0, null)
        }
    }

    override fun focusSearch(focused: View?, direction: Int): View? {
        var result = _layout?.onInterceptFocusSearch(focused, direction)
        if (result != null) {
            return result
        }
        val ff = FocusFinder.getInstance()
        result = ff.findNextFocus(this, focused, direction)
        if (result == null && _adapter != null && _layout != null && !isComputingLayout() && !_layoutFrozen) {
            eatRequestLayout()
            result = _layout?.onFocusSearchFailed(focused, direction, _recycler, _state)
            resumeRequestLayout(false)
        }
        return if (result != null) result else super.focusSearch(focused, direction)
    }

    override fun requestChildFocus(child: View?, focused: View?) {
        if (!_layout!!.onRequestChildFocus(this, _state, child, focused) && focused != null) {
            _tempRect.set(0, 0, focused.getWidth(), focused.getHeight())
            val focusedLayoutParams = focused.getLayoutParams()
            if (focusedLayoutParams is LayoutParams) {
                val lp = focusedLayoutParams
                if (!lp._insetsDirty) {
                    val insets = lp._decorInsets
                    _tempRect.left -= insets.left
                    _tempRect.right += insets.right
                    _tempRect.top -= insets.top
                    _tempRect.bottom += insets.bottom
                }
            }
            offsetDescendantRectToMyCoords(focused, _tempRect)
            offsetRectIntoDescendantCoords(child, _tempRect)
            requestChildRectangleOnScreen(child, _tempRect, !_firstLayoutComplete)
        }
        super.requestChildFocus(child, focused)
    }


    override fun requestChildRectangleOnScreen(child: View?, rectangle: Rect?, immediate: Boolean): Boolean = _layout!!.requestChildRectangleOnScreen(this, child, rectangle, immediate)

    override fun addFocusables(views: ArrayList<View?>?, direction: Int, focusableMode: Int) {
        if (_layout == null || !_layout!!.onAddFocusables(this, views, direction, focusableMode)) {
            super.addFocusables(views, direction, focusableMode)
        }
    }

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()
        _layoutOrScrollCounter = 0
        _isAttached = true
        _firstLayoutComplete = false
        _layout?.dispatchAttachedToWindow(this)
        _postedAnimatorRunner = false
    }

    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()
        _itemAnimator?.endAnimations()
        _firstLayoutComplete = false
        stopScroll()
        _isAttached = false
        _layout?.dispatchDetachedFromWindow(this, _recycler)
        removeCallbacks(_itemAnimatorRunner)
    }

    override fun requestDisallowInterceptTouchEvent(disallowIntercept: Boolean) {
        val listenerCount = _onItemTouchListeners.size
        for (i in 0..listenerCount - 1) {
            val listener = _onItemTouchListeners.get(i)
            listener?.onRequestDisallowInterceptTouchEvent(disallowIntercept)
        }
        super.requestDisallowInterceptTouchEvent(disallowIntercept)
    }

    fun shouldDeferAccessibilityEvent(event: AccessibilityEvent?): Boolean {
        if (isComputingLayout()) {
            var type = 0
            if (event != null) {
                type = AccessibilityEventCompat.getContentChangeTypes(event)
            }
            if (type == 0) {
                type = AccessibilityEventCompat.CONTENT_CHANGE_TYPE_UNDEFINED
            }
            _eatenAccessibilityChangeFlags = _eatenAccessibilityChangeFlags or type
            return true
        }
        return false
    }

    override fun sendAccessibilityEventUnchecked(event: AccessibilityEvent?) {
        if (shouldDeferAccessibilityEvent(event)) {
            return
        }
        super.sendAccessibilityEventUnchecked(event);
    }

    override fun requestLayout() {
        if (!_eatRequestLayout && !_layoutFrozen) {
            super.requestLayout()
        } else {
            _layoutRequestEaten = true
        }
    }

    override fun checkLayoutParams(p: ViewGroup.LayoutParams?): Boolean = p is LayoutParams && _layout!!.checkLayoutParams(p)

    override fun generateDefaultLayoutParams(): ViewGroup.LayoutParams? {
        if (_layout == null) {
            throw IllegalStateException("RecyclerView has no LayoutManager")
        }
        return _layout?.generateDefaultLayoutParams()
    }

    override fun generateLayoutParams(p: ViewGroup.LayoutParams?): ViewGroup.LayoutParams? {
        if (_layout == null) {
            throw IllegalStateException("RecyclerView has no LayoutManager")
        }
        return _layout?.generateLayoutParams(p)
    }

    override fun generateLayoutParams(attrs: AttributeSet?): ViewGroup.LayoutParams? {
        if (_layout == null) {
            throw IllegalStateException("RecyclerView has no LayoutManager")
        }
        return _layout?.generateLayoutParams(getContext(), attrs);
    }

    fun findViewHolderForPosition(position: Int): ViewHolder? = findViewHolderForPosition(position, false)

    fun findViewHolderForLayoutPosition(position: Int): ViewHolder? = findViewHolderForPosition(position, false)

    fun findViewHolderForAdapterPosition(position: Int): ViewHolder? {
        if (_dataSetHasChangedAfterLayout) {
            return null
        }
        val childCount = _childHelper!!.getUnfilteredChildCount()
        for (i in 0..childCount - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getUnfilteredChildAt(i))
            if (holder != null && !holder.isRemoved() && getAdapterPositionFor(holder) == position) {
                return holder
            }
        }
        return null
    }

    fun findViewHolderForItemId(id: Long): ViewHolder? {
        val childCount = _childHelper!!.getUnfilteredChildCount()
        for (i in 0..childCount - 1) {
            val holder = getChildViewHolderInt(_childHelper?.getUnfilteredChildAt(i))
            if (holder != null && holder.getItemId() == id) {
                return holder
            }
        }
        return null
    }

    fun findChildViewUnder(x: Float, y: Float): View? {
        val count = _childHelper!!.getChildCount()
        for (i in count - 1 downTo 0) {
            val child = _childHelper?.getChildAt(i)
            val translationX = ViewCompat.getTranslationX(child)
            val translationY = ViewCompat.getTranslationY(child)
            if (x >= child!!.getLeft() + translationX && x <= child.getRight() + translationX && y >= child.getTop() + translationY && y <= child.getBottom() + translationY) {
                return child
            }
        }
        return null
    }

    fun getChildPosition(child: View?): Int = getChildAdapterPosition(child)

    fun getChildAdapterPosition(child: View?): Int {
        val holder = getChildViewHolderInt(child)
        return if (holder != null) holder.getAdapterPosition() else NO_POSITION
    }

    fun getChildItemId(child: View?): Long {
        if (_adapter == null || !_adapter!!.hasStableIds()) {
            return NO_ID;
        }
        val holder = getChildViewHolderInt(child)
        return if (holder != null) holder.getItemId() else NO_ID
    }

    fun setItemAnimator(animator: ItemAnimator?) {
        if (_itemAnimator != null) {
            _itemAnimator?.endAnimations()
            _itemAnimator?.setListener(null)
        }
        _itemAnimator = animator
        _itemAnimator?.setListener(_itemAnimatorListener)
    }

    fun getItemAnimator(): ItemAnimator? = _itemAnimator

    fun invalidateItemDecorations() {
        if (_itemDecorations.size == 0) {
            return
        }
        _layout?.assertNotInLayoutOrScroll("Cannot invalidate item decorations during a scroll or layout")
        markItemDecorInsetsDirty()
        requestLayout()
    }

    fun getScrollFactor(): Float {
        if (_scrollFactor == Float.MIN_VALUE) {
            val outValue = TypedValue()
            if (getContext().getTheme().resolveAttribute(android.R.attr.listPreferredItemHeight, outValue, true)) {
                _scrollFactor = outValue.getDimension(getContext().getResources().getDisplayMetrics())
            } else {
                return 0.0f
            }
        }
        return _scrollFactor
    }

    override fun onGenericMotionEvent(event: MotionEvent?): Boolean {
        if (_layout == null) {
            return false
        }
        if (_layoutFrozen) {
            return false
        }
        if ((MotionEventCompat.getSource(event) and InputDeviceCompat.SOURCE_CLASS_POINTER) != 0) {
            if (event!!.getAction() == MotionEventCompat.ACTION_SCROLL) {
                var vScroll: Float
                var hScroll: Float
                if (_layout!!.canScrollVertically()) {
                    vScroll = -MotionEventCompat.getAxisValue(event, MotionEventCompat.AXIS_VSCROLL)
                } else {
                    vScroll = 0.0f
                }
                if (_layout!!.canScrollHorizontally()) {
                    hScroll = MotionEventCompat.getAxisValue(event, MotionEventCompat.AXIS_HSCROLL);
                } else {
                    hScroll = 0.0f
                }
                if (vScroll != 0.0f || hScroll != 0.0f) {
                    val scrollFactor = getScrollFactor()
                    scrollByInternal((hScroll * scrollFactor).toInt(), (vScroll * scrollFactor).toInt(), event)
                }
            }
        }
        return false
    }

    fun isLayoutFrozen(): Boolean =_layoutFrozen
    fun getMinFlingVelocity(): Int = _minFlingVelocity
    fun getMaxFlingVelocity(): Int = _maxFlingVelocity
    override fun isAttachedToWindow(): Boolean = _isAttached

    fun addOnItemTouchListener(listener: OnItemTouchListener?) {
        _onItemTouchListeners.add(listener)
    }

    fun removeOnItemTouchListener(listener: OnItemTouchListener?) {
        _onItemTouchListeners.remove(listener)
        if (_activeOnItemTouchListener == listener) {
            _activeOnItemTouchListener = null
        }
    }

    fun smoothScrollToPosition(position: Int) {
        if (_layoutFrozen) {
            return
        }
        if (_layout == null) {
            return
        }
        _layout?.smoothScrollToPosition(this, _state, position)
    }

    fun setOnScrollListener(listener: OnScrollListener?) {
        _scrollListener = listener
    }

    fun addOnScrollListener(listener: OnScrollListener?) {
        if (_scrollListeners == null) {
            _scrollListeners = arrayListOf<OnScrollListener?>()
        }
        _scrollListeners?.add(listener)
    }

    fun removeOnScrollListener(listener: OnScrollListener?) {
        _scrollListeners?.remove(listener)
    }

    fun clearOnScrollListeners() {
        _scrollListeners?.clear()
    }

    fun addItemDecoration(decor: ItemDecoration?, index: Int) {
        _layout?.assertNotInLayoutOrScroll("Cannot add item decoration during a scroll  or layout")
        if (_itemDecorations.isEmpty()) {
            setWillNotDraw(false)
        }
        if (index < 0) {
            _itemDecorations.add(decor)
        } else {
            _itemDecorations.add(index, decor)
        }
        markItemDecorInsetsDirty()
        requestLayout()
    }

    fun addItemDecoration(decor: ItemDecoration?) = addItemDecoration(decor, -1)

    fun removeItemDecoration(decor: ItemDecoration?) {
        _layout?.assertNotInLayoutOrScroll("Cannot remove item decoration during a scroll  or layout")
        _itemDecorations.remove(decor)
        if (_itemDecorations.isEmpty()) {
            setWillNotDraw(ViewCompat.getOverScrollMode(this) == ViewCompat.OVER_SCROLL_NEVER)
        }
        markItemDecorInsetsDirty()
        requestLayout()
    }

    fun setChildDrawingOrderCallback(childDrawingOrderCallback: ChildDrawingOrderCallback?) {
        if (childDrawingOrderCallback == _childDrawingOrderCallback) {
            return
        }
        _childDrawingOrderCallback = childDrawingOrderCallback
        setChildrenDrawingOrderEnabled(_childDrawingOrderCallback != null)
    }

    fun setRecyclerListener(listener: RecyclerListener?) {
        _recyclerListener = listener
    }

    fun addOnChildAttachStateChangeListener(listener: OnChildAttachStateChangeListener?) {
        if (_onChildAttachStateListeners == null) {
            _onChildAttachStateListeners = arrayListOf<OnChildAttachStateChangeListener?>()
        }
        _onChildAttachStateListeners?.add(listener)
    }

    fun removeOnChildAttachStateChangeListener(listener: OnChildAttachStateChangeListener?) {
        if (_onChildAttachStateListeners == null) {
            return;
        }
        _onChildAttachStateListeners?.remove(listener)
    }

    fun clearOnChildAttachStateChangeListeners() {
        _onChildAttachStateListeners?.clear()
    }

    fun getRecycledViewPool(): RecycledViewPool? = _recycler.getRecycledViewPool()

    fun setRecycledViewPool(pool: RecycledViewPool?) {
        _recycler.setRecycledViewPool(pool)
    }

    fun setViewCacheExtension(extension: ViewCacheExtension?) {
        _recycler.setViewCacheExtension(extension)
    }

    fun setItemViewCacheSize(size: Int) {
        _recycler.setViewCacheSize(size)
    }

}