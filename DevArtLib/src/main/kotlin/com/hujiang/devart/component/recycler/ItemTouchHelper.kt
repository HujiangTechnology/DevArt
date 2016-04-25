package com.hujiang.devart.component.recycler

import android.graphics.Canvas
import android.graphics.Rect
import android.os.Build
import android.support.v4.animation.AnimatorCompatHelper
import android.support.v4.animation.AnimatorListenerCompat
import android.support.v4.animation.AnimatorUpdateListenerCompat
import android.support.v4.animation.ValueAnimatorCompat
import android.support.v4.view.GestureDetectorCompat
import android.support.v4.view.MotionEventCompat
import android.support.v4.view.VelocityTrackerCompat
import android.support.v4.view.ViewCompat
import android.view.*
import android.view.animation.Interpolator
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/23/16.
 */
class ItemTouchHelper: RecyclerView.ItemDecoration, RecyclerView.OnChildAttachStateChangeListener {

    companion object {
        val UP = 1
        val DOWN = 1 shl 1
        val LEFT = 1 shl 2
        val RIGHT = 1 shl 3
        val START = LEFT shl 2
        val END = RIGHT shl 2
        val ACTION_STATE_IDLE = 0
        val ACTION_STATE_SWIPE = 1
        val ACTION_STATE_DRAG = 2
        val ANIMATION_TYPE_SWIPE_SUCCESS = 1 shl 1
        val ANIMATION_TYPE_SWIPE_CANCEL = 1 shl 2
        val ANIMATION_TYPE_DRAG = 1 shl 3
        val ACTIVE_POINTER_ID_NONE = -1
        val DIRECTION_FLAG_COUNT = 8
        val ACTION_MODE_IDLE_MASK = (1 shl DIRECTION_FLAG_COUNT) - 1
        val ACTION_MODE_SWIPE_MASK = ACTION_MODE_IDLE_MASK shl DIRECTION_FLAG_COUNT
        val ACTION_MODE_DRAG_MASK = ACTION_MODE_SWIPE_MASK shl DIRECTION_FLAG_COUNT

        fun hitTest(child: View?, x: Float, y: Float, left: Float, top: Float): Boolean {
            return x >= left && x <= left + child!!.getWidth() && y >= top && y <= top + child.getHeight()
        }
    }

    val _pendingCleanup = arrayListOf<View?>()
    var _tmpPosition = FloatArray(2)
    var _selected: RecyclerView.ViewHolder? = null
    var _initialTouchX = 0.0f
    var _initialTouchY = 0.0f
    var _dx = 0.0f
    var _dy = 0.0f
    var _selectedStartX = 0.0f
    var _selectedStartY = 0.0f
    var _activePointerId = ACTIVE_POINTER_ID_NONE
    var _callback: Callback? = null
    var _actionState = ACTION_STATE_IDLE;
    var _selectedFlags = 0
    var _recoverAnimations = arrayListOf<RecoverAnimation?>()
    var _slop = 0
    private var _recyclerView: RecyclerView? = null
    private fun getScrollRunnable(): Runnable? = _scrollRunnable
    private val _scrollRunnable = Runnable {
            if (_selected != null && scrollIfNecessary()) {
                if (_selected != null) {
                    moveIfNecessary(_selected)
                }
                _recyclerView?.removeCallbacks(getScrollRunnable())
                ViewCompat.postOnAnimation(_recyclerView, getScrollRunnable())
            }
        }

    private var _velocityTracker: VelocityTracker? = null
    private var _swapTargets: MutableList<RecyclerView.ViewHolder?>? = null
    private var _distances: MutableList<Int>? = null
    private var _childDrawingOrderCallback: RecyclerView.ChildDrawingOrderCallback? = null
    private var _overdrawChild: View? = null
    private var _overdrawChildPosition = -1
    private var _gestureDetector: GestureDetectorCompat? = null

    private val _onItemTouchListener = object: RecyclerView.OnItemTouchListener {

        override fun onInterceptTouchEvent(rv: RecyclerView?, event: MotionEvent?): Boolean {
            _gestureDetector?.onTouchEvent(event)
            val action = MotionEventCompat.getActionMasked(event)
            if (action == MotionEvent.ACTION_DOWN) {
                _activePointerId = MotionEventCompat.getPointerId(event, 0)
                _initialTouchX = event!!.getX()
                _initialTouchY = event.getY()
                obtainVelocityTracker()
                if (_selected == null) {
                    val animation = findAnimation(event)
                    if (animation != null) {
                        _initialTouchX -= animation._x
                        _initialTouchY -= animation._y
                        endRecoverAnimation(animation._viewHolder, true)
                        if (_pendingCleanup.remove(animation._viewHolder?._itemView)) {
                            _callback?.clearView(_recyclerView, animation._viewHolder)
                        }
                        select(animation._viewHolder, animation._actionState)
                        updateDxDy(event, _selectedFlags, 0)
                    }
                }
            } else if (action == MotionEvent.ACTION_CANCEL || action == MotionEvent.ACTION_UP) {
                _activePointerId = ACTIVE_POINTER_ID_NONE
                select(null, ACTION_STATE_IDLE)
            } else if (_activePointerId != ACTIVE_POINTER_ID_NONE) {
                val index = MotionEventCompat.findPointerIndex(event, _activePointerId)
                if (index >= 0) {
                    checkSelectForSwipe(action, event, index)
                }
            }
            _velocityTracker?.addMovement(event)
            return _selected != null
        }

        override fun onTouchEvent(rv: RecyclerView?, event: MotionEvent?) {
            _gestureDetector?.onTouchEvent(event)
            _velocityTracker?.addMovement(event)
            if (_activePointerId == ACTIVE_POINTER_ID_NONE) {
                return
            }
            val action = MotionEventCompat.getActionMasked(event)
            val activePointerIndex = MotionEventCompat.findPointerIndex(event, _activePointerId)
            if (activePointerIndex >= 0) {
                checkSelectForSwipe(action, event, activePointerIndex)
            }
            val viewHolder = _selected
            if (viewHolder == null) {
                return
            }
            when (action) {
                MotionEvent.ACTION_MOVE -> {
                    if (activePointerIndex >= 0) {
                        updateDxDy(event, _selectedFlags, activePointerIndex)
                        moveIfNecessary(viewHolder)
                        _recyclerView?.removeCallbacks(_scrollRunnable)
                        _scrollRunnable.run()
                        _recyclerView?.invalidate()
                    }
                }
                MotionEvent.ACTION_CANCEL, MotionEvent.ACTION_UP -> {
                    _velocityTracker?.computeCurrentVelocity(1000, _recyclerView!!.getMaxFlingVelocity().toFloat())
                    select(null, ACTION_STATE_IDLE)
                    _activePointerId = ACTIVE_POINTER_ID_NONE
                }
                MotionEvent.ACTION_POINTER_UP -> {
                    val pointerIndex = MotionEventCompat.getActionIndex(event)
                    val pointerId = MotionEventCompat.getPointerId(event, pointerIndex)
                    if (pointerId == _activePointerId) {
                        _velocityTracker?.computeCurrentVelocity(1000, _recyclerView!!.getMaxFlingVelocity().toFloat())
                        val newPointerIndex = if (pointerIndex == 0) 1 else 0
                        _activePointerId = MotionEventCompat.getPointerId(event, newPointerIndex)
                        updateDxDy(event, _selectedFlags, pointerIndex)
                    }
                }
            }
        }

        override fun onRequestDisallowInterceptTouchEvent(disallowIntercept: Boolean) {
            if (!disallowIntercept) {
                return
            }
            select(null, ACTION_STATE_IDLE)
        }
    }

    private var _tmpRect: Rect? = null
    private var _dragScrollStartTimeInMs = 0L

    constructor(callback: Callback?) {
        _callback = callback
    }

    private fun obtainVelocityTracker() {
        _velocityTracker?.recycle()
        _velocityTracker = VelocityTracker.obtain()
    }

    fun endRecoverAnimation(viewHolder: RecyclerView.ViewHolder?, override: Boolean): Int {
        val recoverAnimSize = _recoverAnimations.size
        for (i in recoverAnimSize - 1 downTo 0) {
            val anim = _recoverAnimations.get(i)
            if (anim?._viewHolder == viewHolder) {
                anim?._overridden =  anim!!._overridden  or override
                if (!anim._ended) {
                    anim.cancel()
                }
                _recoverAnimations.removeAt(i)
                anim._viewHolder?.setIsRecyclable(true)
                return anim._animationType
            }
        }
        return 0;
    }

    private fun findAnimation(event: MotionEvent?):RecoverAnimation? {
        if (_recoverAnimations.isEmpty()) {
            return null
        }
        val target = findChildView(event)
        for (i in _recoverAnimations.size - 1 downTo 0) {
            val  anim = _recoverAnimations.get(i)
            if (anim?._viewHolder?._itemView == target) {
                return anim
            }
        }
        return null
    }

    private fun findSwipedView(motionEvent: MotionEvent?): RecyclerView.ViewHolder? {
        val lm = _recyclerView?.getLayoutManager()
        if (_activePointerId == ACTIVE_POINTER_ID_NONE) {
            return null
        }
        val pointerIndex = MotionEventCompat.findPointerIndex(motionEvent, _activePointerId)
        val dx = MotionEventCompat.getX(motionEvent, pointerIndex) - _initialTouchX
        val dy = MotionEventCompat.getY(motionEvent, pointerIndex) - _initialTouchY
        val absDx = Math.abs(dx)
        val absDy = Math.abs(dy)
        if (absDx < _slop && absDy < _slop) {
            return null
        }
        if (absDx > absDy && lm!!.canScrollHorizontally()) {
            return null
        } else if (absDy > absDx && lm!!.canScrollVertically()) {
            return null
        }
        val child = findChildView(motionEvent)
        if (child == null) {
            return null
        }
        return _recyclerView?.getChildViewHolder(child)
    }

    private fun checkSelectForSwipe(action: Int, motionEvent: MotionEvent?, pointerIndex: Int): Boolean {
        if (_selected != null || action != MotionEvent.ACTION_MOVE || _actionState == ACTION_STATE_DRAG || !_callback!!.isItemViewSwipeEnabled()) {
            return false
        }
        if (_recyclerView!!.getScrollState() == RecyclerView.SCROLL_STATE_DRAGGING) {
            return false
        }
        val vh = findSwipedView(motionEvent)
        if (vh == null) {
            return false
        }
        val movementFlags = _callback!!.getAbsoluteMovementFlags(_recyclerView, vh)
        val swipeFlags = (movementFlags and ACTION_MODE_SWIPE_MASK) shr (DIRECTION_FLAG_COUNT * ACTION_STATE_SWIPE)
        if (swipeFlags == 0) {
            return false
        }
        val x = MotionEventCompat.getX(motionEvent, pointerIndex)
        val y = MotionEventCompat.getY(motionEvent, pointerIndex)
        val dx = x - _initialTouchX
        val dy = y - _initialTouchY
        val absDx = Math.abs(dx)
        val absDy = Math.abs(dy)
        if (absDx < _slop && absDy < _slop) {
            return false
        }
        if (absDx > absDy) {
            if (dx < 0 && (swipeFlags and LEFT) == 0) {
                return false
            }
            if (dx > 0 && (swipeFlags and RIGHT) == 0) {
                return false
            }
        } else {
            if (dy < 0 && (swipeFlags and UP) == 0) {
                return false
            }
            if (dy > 0 && (swipeFlags and DOWN) == 0) {
                return false
            }
        }
        _dx = 0.0f
        _dy = 0.0f
        _activePointerId = MotionEventCompat.getPointerId(motionEvent, 0)
        select(vh, ACTION_STATE_SWIPE)
        return true
    }


    private fun updateDxDy(ev: MotionEvent?, directionFlags: Int, pointerIndex: Int) {
        val x = MotionEventCompat.getX(ev, pointerIndex)
        val y = MotionEventCompat.getY(ev, pointerIndex)
        _dx = x - _initialTouchX
        _dy = y - _initialTouchY
        if ((directionFlags and LEFT) == 0) {
            _dx = Math.max(0.0f, _dx)
        }
        if ((directionFlags and RIGHT) == 0) {
            _dx = Math.min(0.0f, _dx)
        }
        if ((directionFlags and UP) == 0) {
            _dy = Math.max(0.0f, _dy)
        }
        if ((directionFlags and DOWN) == 0) {
            _dy = Math.min(0.0f, _dy)
        }
    }

    private fun moveIfNecessary(viewHolder: RecyclerView.ViewHolder?) {
        if (_recyclerView!!.isLayoutRequested()) {
            return
        }
        if (_actionState != ACTION_STATE_DRAG) {
            return
        }
        val threshold = _callback!!.getMoveThreshold(viewHolder)
        val x = (_selectedStartX + _dx).toInt()
        val y = (_selectedStartY + _dy).toInt()
        if (Math.abs(y - viewHolder!!._itemView!!.getTop()) < viewHolder._itemView!!.getHeight() * threshold && Math.abs(x - viewHolder._itemView!!.getLeft()) < viewHolder._itemView!!.getWidth() * threshold) {
            return
        }
        val swapTargets = findSwapTargets(viewHolder)
        if (swapTargets!!.size == 0) {
            return
        }
        val target = _callback?.chooseDropTarget(viewHolder, swapTargets, x, y)
        if (target == null) {
            _swapTargets?.clear()
            _distances?.clear()
            return
        }
        val toPosition = target.getAdapterPosition()
        val fromPosition = viewHolder.getAdapterPosition()
        if (_callback!!.onMove(_recyclerView, viewHolder, target)) {
            _callback?.onMoved(_recyclerView, viewHolder, fromPosition, target, toPosition, x, y)
        }
    }

    private fun findSwapTargets(viewHolder: RecyclerView.ViewHolder?): MutableList<RecyclerView.ViewHolder?>? {
        if (_swapTargets == null) {
            _swapTargets = arrayListOf<RecyclerView.ViewHolder?>()
            _distances = arrayListOf<Int>()
        } else {
            _swapTargets?.clear()
            _distances?.clear()
        }
        val margin = _callback!!.getBoundingBoxMargin()
        val left = Math.round(_selectedStartX + _dx) - margin
        val top = Math.round(_selectedStartY + _dy) - margin
        val right = left + viewHolder!!._itemView!!.getWidth() + 2 * margin
        val bottom = top + viewHolder._itemView!!.getHeight() + 2 * margin
        val centerX = (left + right) / 2
        val centerY = (top + bottom) / 2
        val lm = _recyclerView?.getLayoutManager()
        val childCount = lm!!.getChildCount()
        for (i in 0..childCount - 1) {
            val other = lm.getChildAt(i)
            if (other == viewHolder._itemView) {
                continue
            }
            if (other!!.getBottom() < top || other.getTop() > bottom || other.getRight() < left || other.getLeft() > right) {
                continue
            }
            val otherVh = _recyclerView?.getChildViewHolder(other)
            if (_callback!!.canDropOver(_recyclerView, _selected, otherVh)) {
                val dx = Math.abs(centerX - (other.getLeft() + other.getRight()) / 2)
                val dy = Math.abs(centerY - (other.getTop() + other.getBottom()) / 2)
                val dist = dx * dx + dy * dy
                var pos = 0
                val cnt = _swapTargets!!.size
                for (j in 0..cnt - 1) {
                    if (dist > _distances!!.get(j)) {
                        pos++
                    } else {
                        break
                    }
                }
                _swapTargets?.add(pos, otherVh)
                _distances?.add(pos, dist)
            }
        }
        return _swapTargets
    }

    fun scrollIfNecessary(): Boolean {
        if (_selected == null) {
            _dragScrollStartTimeInMs = Long.MIN_VALUE
            return false
        }
        val now = System.currentTimeMillis()
        val scrollDuration = if (_dragScrollStartTimeInMs == Long.MIN_VALUE) 0 else now - _dragScrollStartTimeInMs
        val lm = _recyclerView?.getLayoutManager()
        if (_tmpRect == null) {
            _tmpRect = Rect()
        }
        var scrollX = 0
        var scrollY = 0
        lm?.calculateItemDecorationsForChild(_selected?._itemView, _tmpRect)
        if (lm!!.canScrollHorizontally()) {
            val curX = (_selectedStartX + _dx).toInt()
            val leftDiff = curX - _tmpRect!!.left - _recyclerView!!.getPaddingLeft()
            if (_dx < 0 && leftDiff < 0) {
                scrollX = leftDiff
            } else if (_dx > 0) {
                val rightDiff = curX + _selected!!._itemView!!.getWidth() + _tmpRect!!.right - (_recyclerView!!.getWidth() - _recyclerView!!.getPaddingRight())
                if (rightDiff > 0) {
                    scrollX = rightDiff
                }
            }
        }
        if (lm.canScrollVertically()) {
            val curY = (_selectedStartY + _dy).toInt()
            val topDiff = curY - _tmpRect!!.top - _recyclerView!!.getPaddingTop()
            if (_dy < 0 && topDiff < 0) {
                scrollY = topDiff
            } else if (_dy > 0) {
                val bottomDiff = curY + _selected!!._itemView!!.getHeight() + _tmpRect!!.bottom - (_recyclerView!!.getHeight() - _recyclerView!!.getPaddingBottom())
                if (bottomDiff > 0) {
                    scrollY = bottomDiff
                }
            }
        }
        if (scrollX != 0) {
            scrollX = _callback!!.interpolateOutOfBoundsScroll(_recyclerView, _selected!!._itemView!!.getWidth(), scrollX, _recyclerView!!.getWidth(), scrollDuration)
        }
        if (scrollY != 0) {
            scrollY = _callback!!.interpolateOutOfBoundsScroll(_recyclerView, _selected!!._itemView!!.getHeight(), scrollY, _recyclerView!!.getHeight(), scrollDuration)
        }
        if (scrollX != 0 || scrollY != 0) {
            if (_dragScrollStartTimeInMs == Long.MIN_VALUE) {
                _dragScrollStartTimeInMs = now
            }
            _recyclerView?.scrollBy(scrollX, scrollY)
            return true
        }
        _dragScrollStartTimeInMs = Long.MIN_VALUE
        return false
    }

    private fun addChildDrawingOrderCallback() {
        if (Build.VERSION.SDK_INT >= 21) {
            return
        }
        if (_childDrawingOrderCallback == null) {
            _childDrawingOrderCallback = object: RecyclerView.ChildDrawingOrderCallback {

                override fun onGetChildDrawingOrder(childCount: Int, i: Int): Int {
                    if (_overdrawChild == null) {
                        return i
                    }
                    var childPosition = _overdrawChildPosition
                    if (childPosition == -1) {
                        childPosition = _recyclerView!!.indexOfChild(_overdrawChild)
                        _overdrawChildPosition = childPosition
                    }
                    if (i == childCount - 1) {
                        return childPosition
                    }
                    return if (i < childPosition) i else i + 1
                }
            }
        }
        _recyclerView?.setChildDrawingOrderCallback(_childDrawingOrderCallback)
    }

    private fun swipeIfNecessary(viewHolder: RecyclerView.ViewHolder?): Int {
        if (_actionState == ACTION_STATE_DRAG) {
            return 0
        }
        val originalMovementFlags = _callback!!.getMovementFlags(_recyclerView, viewHolder)
        val absoluteMovementFlags = _callback!!.convertToAbsoluteDirection(originalMovementFlags, ViewCompat.getLayoutDirection(_recyclerView))
        val flags = (absoluteMovementFlags and ACTION_MODE_SWIPE_MASK) shr (ACTION_STATE_SWIPE * DIRECTION_FLAG_COUNT)
        if (flags == 0) {
            return 0
        }
        val originalFlags = (originalMovementFlags and ACTION_MODE_SWIPE_MASK) shr (ACTION_STATE_SWIPE * DIRECTION_FLAG_COUNT)
        var swipeDir: Int
        if (Math.abs(_dx) > Math.abs(_dy)) {
            swipeDir = checkHorizontalSwipe(viewHolder, flags)
            if (swipeDir > 0) {
                if ((originalFlags and swipeDir) == 0) {
                    return Callback.convertToRelativeDirection(swipeDir, ViewCompat.getLayoutDirection(_recyclerView))
                }
                return swipeDir
            }
            swipeDir = checkVerticalSwipe(viewHolder, flags)
            if (swipeDir > 0) {
                return swipeDir
            }
        } else {
            swipeDir = checkVerticalSwipe(viewHolder, flags)
            if (swipeDir > 0) {
                return swipeDir
            }
            swipeDir = checkHorizontalSwipe(viewHolder, flags)
            if (swipeDir > 0) {
                if ((originalFlags and swipeDir) == 0) {
                    return Callback.convertToRelativeDirection(swipeDir, ViewCompat.getLayoutDirection(_recyclerView))
                }
                return swipeDir
            }
        }
        return 0
    }

    private fun checkHorizontalSwipe(viewHolder: RecyclerView.ViewHolder?, flags: Int): Int {
        if ((flags and (LEFT or RIGHT)) != 0) {
            val dirFlag = if (_dx > 0)  RIGHT else LEFT
            if (_velocityTracker != null && _activePointerId > -1) {
                val xVelocity = VelocityTrackerCompat.getXVelocity(_velocityTracker, _activePointerId)
                val velDirFlag = if (xVelocity > 0.0f) RIGHT else LEFT
                if ((velDirFlag and flags) != 0 && dirFlag == velDirFlag &&Math.abs(xVelocity) >= _recyclerView!!.getMinFlingVelocity()) {
                    return velDirFlag
                }
            }
            val threshold = _recyclerView!!.getWidth() * _callback!!.getSwipeThreshold(viewHolder)
            if ((flags and dirFlag) != 0 && Math.abs(_dx) > threshold) {
                return dirFlag
            }
        }
        return 0
    }

    private fun checkVerticalSwipe(viewHolder: RecyclerView.ViewHolder?, flags: Int): Int {
        if ((flags and (UP or DOWN)) != 0) {
            val dirFlag = if (_dy > 0)  DOWN else UP
            if (_velocityTracker != null && _activePointerId > -1) {
                val yVelocity = VelocityTrackerCompat.getYVelocity(_velocityTracker, _activePointerId)
                val velDirFlag = if (yVelocity > 0.0f)  DOWN else UP
                if ((velDirFlag and flags) != 0 && velDirFlag == dirFlag && Math.abs(yVelocity) >= _recyclerView!!.getMinFlingVelocity()) {
                    return velDirFlag
                }
            }
            val threshold = _recyclerView!!.getHeight() * _callback!!.getSwipeThreshold(viewHolder)
            if ((flags and dirFlag) != 0 && Math.abs(_dy) > threshold) {
                return dirFlag
            }
        }
        return 0
    }

    private fun releaseVelocityTracker() {
        if (_velocityTracker != null) {
            _velocityTracker?.recycle()
            _velocityTracker = null
        }
    }

    private fun getSelectedDxDy(outPosition: FloatArray?) {
        if ((_selectedFlags and (LEFT or RIGHT)) != 0) {
            outPosition!![0] = _selectedStartX + _dx - _selected!!._itemView!!.getLeft()
        } else {
            outPosition!![0] = ViewCompat.getTranslationX(_selected!!._itemView)
        }
        if ((_selectedFlags and (UP or DOWN)) != 0) {
            outPosition[1] = _selectedStartY + _dy - _selected!!._itemView!!.getTop()
        } else {
            outPosition[1] = ViewCompat.getTranslationY(_selected!!._itemView)
        }
    }

    private fun select(selected: RecyclerView.ViewHolder?, actionState: Int) {
        if (selected == _selected && actionState == _actionState) {
            return
        }
        _dragScrollStartTimeInMs = Long.MIN_VALUE
        val prevActionState = _actionState
        endRecoverAnimation(selected, true)
        _actionState = actionState
        if (actionState == ACTION_STATE_DRAG) {
            _overdrawChild = selected?._itemView
            addChildDrawingOrderCallback()
        }
        var actionStateMask = (1 shl (DIRECTION_FLAG_COUNT + DIRECTION_FLAG_COUNT * actionState)) - 1
        var preventLayout = false
        if (_selected != null) {
            val prevSelected = _selected
            if (prevSelected?._itemView?.getParent() != null) {
                val swipeDir = if (prevActionState == ACTION_STATE_DRAG) 0 else swipeIfNecessary(prevSelected)
                releaseVelocityTracker()

                var targetTranslateX: Float
                var targetTranslateY: Float
                var animationType: Int
                when (swipeDir) {
                    LEFT, RIGHT, START, END -> {
                        targetTranslateY = 0.0f
                        targetTranslateX = Math.signum(_dx) * _recyclerView!!.getWidth()
                    }
                    UP, DOWN -> {
                        targetTranslateX = 0.0f
                        targetTranslateY = Math.signum(_dy) * _recyclerView!!.getHeight()
                    }
                    else -> {
                        targetTranslateX = 0.0f
                        targetTranslateY = 0.0f
                    }
                }
                if (prevActionState == ACTION_STATE_DRAG) {
                    animationType = ANIMATION_TYPE_DRAG
                } else if (swipeDir > 0) {
                    animationType = ANIMATION_TYPE_SWIPE_SUCCESS
                } else {
                    animationType = ANIMATION_TYPE_SWIPE_CANCEL
                }
                getSelectedDxDy(_tmpPosition)
                val currentTranslateX = _tmpPosition[0]
                val currentTranslateY = _tmpPosition[1]
                val rv = object: RecoverAnimation(prevSelected, animationType, prevActionState, currentTranslateX, currentTranslateY, targetTranslateX, targetTranslateY) {

                    override fun onAnimationEnd(animation: ValueAnimatorCompat?) {
                        super.onAnimationEnd(animation);
                        if (_overridden) {
                            return
                        }
                        if (swipeDir <= 0) {
                            _callback?.clearView(_recyclerView, prevSelected)
                        } else {
                            _pendingCleanup.add(prevSelected?._itemView)
                            _isPendingCleanup = true
                            if (swipeDir > 0) {
                                postDispatchSwipe(this, swipeDir)
                            }
                        }
                        if (_overdrawChild == prevSelected?._itemView) {
                            removeChildDrawingOrderCallbackIfNecessary(prevSelected?._itemView)
                        }
                    }
                }
                val duration = _callback!!.getAnimationDuration(_recyclerView, animationType, targetTranslateX - currentTranslateX, targetTranslateY - currentTranslateY)
                rv.setDuration(duration)
                _recoverAnimations.add(rv)
                rv.start()
                preventLayout = true
            } else {
                removeChildDrawingOrderCallbackIfNecessary(prevSelected?._itemView)
                _callback?.clearView(_recyclerView, prevSelected)
            }
            _selected = null
        }
        if (selected != null) {
            _selectedFlags = (_callback!!.getAbsoluteMovementFlags(_recyclerView, selected) and actionStateMask) shr (_actionState * DIRECTION_FLAG_COUNT)
            _selectedStartX = selected._itemView!!.getLeft().toFloat()
            _selectedStartY = selected._itemView!!.getTop().toFloat()
            _selected = selected
            if (actionState == ACTION_STATE_DRAG) {
                _selected?._itemView?.performHapticFeedback(HapticFeedbackConstants.LONG_PRESS)
            }
        }
        val rvParent = _recyclerView?.getParent()
        if (rvParent != null) {
            rvParent.requestDisallowInterceptTouchEvent(_selected != null)
        }
        if (!preventLayout) {
            _recyclerView?.getLayoutManager()?.requestSimpleAnimationsInNextLayout()
        }
        _callback?.onSelectedChanged(_selected, _actionState)
        _recyclerView?.invalidate()
    }

    private fun removeChildDrawingOrderCallbackIfNecessary(view: View?) {
        if (view == _overdrawChild) {
            _overdrawChild = null
            if (_childDrawingOrderCallback != null) {
                _recyclerView?.setChildDrawingOrderCallback(null)
            }
        }
    }

    private fun postDispatchSwipe(anim: RecoverAnimation?, swipeDir: Int) {
        var runnable: Runnable? = null
        runnable = object: Runnable {
            override fun run() {
                if (_recyclerView != null && _recyclerView!!.isAttachedToWindow() && !anim!!._overridden && anim._viewHolder!!.getAdapterPosition() != RecyclerView.NO_POSITION) {
                    val animator = _recyclerView?.getItemAnimator()
                    if ((animator == null || !animator.isRunning(null)) && !hasRunningRecoverAnim()) {
                        _callback?.onSwiped(anim._viewHolder, swipeDir)
                    } else {
                        _recyclerView?.post(runnable)
                    }
                }
            }
        }
        _recyclerView?.post(runnable)
    }

    private fun hasRunningRecoverAnim(): Boolean {
        val size = _recoverAnimations.size
        for (i in 0..size - 1) {
            if (!_recoverAnimations.get(i)!!._ended) {
                return true
            }
        }
        return false
    }


    interface ViewDropHandler {
        fun prepareForDrop(view: View?, target: View?, x: Int, y: Int)
    }

    open class RecoverAnimation: AnimatorListenerCompat {

        var _startDx = 0.0f
        var _startDy = 0.0f
        var _targetX = 0.0f
        var _targetY = 0.0f
        var _viewHolder: RecyclerView.ViewHolder? = null
        var _actionState = 0
        var _valueAnimator: ValueAnimatorCompat? = null
        var _animationType = 0
        var _isPendingCleanup = false
        var _x = 0.0f
        var _y = 0.0f
        var _overridden = false
        var _ended = false
        var _fraction = 0.0f

        constructor(viewHolder: RecyclerView.ViewHolder?, animationType: Int, actionState: Int, startDx: Float, startDy: Float, targetX: Float, targetY: Float) {
            _actionState = actionState
            _animationType = animationType
            _viewHolder = viewHolder
            _startDx = startDx
            _startDy = startDy
            _targetX = targetX
            _targetY = targetY;
            _valueAnimator = AnimatorCompatHelper.emptyValueAnimator()
            _valueAnimator?.addUpdateListener(object: AnimatorUpdateListenerCompat {
                override fun onAnimationUpdate(animation: ValueAnimatorCompat?) {
                    setFraction(animation!!.getAnimatedFraction())
                }
            })
            _valueAnimator?.setTarget(viewHolder?._itemView)
            _valueAnimator?.addListener(this)
            setFraction(0f)
        }

        fun setDuration(duration: Long) {
            _valueAnimator?.setDuration(duration)
        }

        fun start() {
            _viewHolder?.setIsRecyclable(false)
            _valueAnimator?.start()
        }

        fun cancel() {
            _valueAnimator?.cancel()
        }

        fun setFraction(fraction: Float) {
            _fraction = fraction
        }

        fun update() {
            if (_startDx == _targetX) {
                _x = ViewCompat.getTranslationX(_viewHolder?._itemView)
            } else {
                _x = _startDx + _fraction * (_targetX - _startDx)
            }
            if (_startDy == _targetY) {
                _y = ViewCompat.getTranslationY(_viewHolder?._itemView)
            } else {
                _y = _startDy + _fraction * (_targetY - _startDy)
            }
        }

        override fun onAnimationStart(animation: ValueAnimatorCompat?) { }

        override fun onAnimationEnd(animation: ValueAnimatorCompat?) { }

        override fun onAnimationCancel(animation: ValueAnimatorCompat?) { }

        override fun onAnimationRepeat(animation: ValueAnimatorCompat?) { }

    }

    abstract class Callback {

        companion object {
            val DEFAULT_DRAG_ANIMATION_DURATION = 200L
            val DEFAULT_SWIPE_ANIMATION_DURATION = 250L
            val RELATIVE_DIR_FLAGS = START or END or ((START or END) shl DIRECTION_FLAG_COUNT) or ((START or END) shl (2 * DIRECTION_FLAG_COUNT))
            val ABS_HORIZONTAL_DIR_FLAGS = LEFT or RIGHT or ((LEFT or RIGHT) shl DIRECTION_FLAG_COUNT) or ((LEFT or RIGHT) shl (2 * DIRECTION_FLAG_COUNT))
            var _UICallback: ItemTouchUIUtil? = null

            val _dragScrollInterpolator = object: Interpolator {
                override fun getInterpolation(t: Float): Float {
                    return t * t * t * t * t
                }
            }

            val _dragViewScrollCapInterpolator = object: Interpolator {
                override fun getInterpolation(t: Float): Float {
                    var nt = t - 1.0f
                    return nt * nt * nt * nt * nt + 1.0f
                }
            };
            val DRAG_SCROLL_ACCELERATION_LIMIT_TIME_MS = 2000L

            fun getDefaultUIUtil(): ItemTouchUIUtil? = _UICallback

            fun convertToRelativeDirection(flags: Int, layoutDirection: Int): Int {
                var nflags = flags
                var masked = nflags and ABS_HORIZONTAL_DIR_FLAGS
                if (masked == 0) {
                    return nflags
                }
                nflags = nflags and masked.inv()
                if (layoutDirection == ViewCompat.LAYOUT_DIRECTION_LTR) {
                    nflags = nflags or (masked shl 2)
                    return nflags
                } else {
                    nflags = nflags or ((masked shl 1) and ABS_HORIZONTAL_DIR_FLAGS.inv())
                    nflags = nflags or (((masked shl 1) and ABS_HORIZONTAL_DIR_FLAGS) shl 2)
                }
                return nflags
            }

            fun makeMovementFlags(dragFlags: Int, swipeFlags: Int): Int = makeFlag(ACTION_STATE_IDLE, swipeFlags or dragFlags) or makeFlag(ACTION_STATE_SWIPE, swipeFlags) or makeFlag(ACTION_STATE_DRAG, dragFlags)

            fun makeFlag(actionState: Int, directions: Int): Int = directions shl (actionState * DIRECTION_FLAG_COUNT)
        }

        var _cachedMaxScrollSpeed = -1

        init {
            if (Build.VERSION.SDK_INT >= 21) {
                _UICallback = ItemTouchUIUtilImpl.Lollipop()
            } else if (Build.VERSION.SDK_INT >= 11) {
                _UICallback = ItemTouchUIUtilImpl.Honeycomb()
            } else {
                _UICallback = ItemTouchUIUtilImpl.Gingerbread()
            }
        }

        abstract fun getMovementFlags(recyclerView: RecyclerView?, viewHolder: RecyclerView.ViewHolder?): Int

        fun convertToAbsoluteDirection(flags: Int, layoutDirection: Int): Int {
            var nflags = flags
            var masked = nflags and RELATIVE_DIR_FLAGS
            if (masked == 0) {
                return nflags
            }
            nflags = nflags and masked.inv()
            if (layoutDirection == ViewCompat.LAYOUT_DIRECTION_LTR) {
                nflags = nflags or (masked shr 2)
                return nflags
            } else {
                nflags = nflags or ((masked shr 1) and RELATIVE_DIR_FLAGS.inv())
                nflags = nflags or (((masked shr 1) and RELATIVE_DIR_FLAGS) shr 2)
            }
            return nflags
        }

        fun getAbsoluteMovementFlags(recyclerView: RecyclerView?, viewHolder: RecyclerView.ViewHolder?): Int{
            val flags = getMovementFlags(recyclerView, viewHolder)
            return convertToAbsoluteDirection(flags, ViewCompat.getLayoutDirection(recyclerView))
        }

        fun hasDragFlag(recyclerView: RecyclerView?, viewHolder: RecyclerView.ViewHolder?): Boolean {
            val flags = getAbsoluteMovementFlags(recyclerView, viewHolder)
            return (flags and ACTION_MODE_DRAG_MASK) != 0
        }

        fun hasSwipeFlag(recyclerView: RecyclerView?, viewHolder: RecyclerView.ViewHolder?): Boolean {
            val flags = getAbsoluteMovementFlags(recyclerView, viewHolder)
            return (flags and ACTION_MODE_SWIPE_MASK) != 0
        }

        fun canDropOver(recyclerView: RecyclerView?, current: RecyclerView.ViewHolder?, target: RecyclerView.ViewHolder?): Boolean {
            return true
        }

        abstract fun onMove(recyclerView: RecyclerView?, viewHolder: RecyclerView.ViewHolder?, target: RecyclerView.ViewHolder?): Boolean

        open fun isLongPressDragEnabled(): Boolean {
            return true
        }

        open fun isItemViewSwipeEnabled(): Boolean {
            return true
        }

        open fun getBoundingBoxMargin(): Int {
            return 0
        }

        open fun getSwipeThreshold(viewHolder: RecyclerView.ViewHolder?): Float {
            return 0.5f
        }

        open fun getMoveThreshold(viewHolder: RecyclerView.ViewHolder?): Float {
            return 0.5f
        }

        fun chooseDropTarget(selected: RecyclerView.ViewHolder?, dropTargets: MutableList<RecyclerView.ViewHolder?>?, curX: Int, curY: Int): RecyclerView.ViewHolder? {
            var right = curX + selected!!._itemView!!.getWidth()
            var bottom = curY + selected._itemView!!.getHeight()
            var winner: RecyclerView.ViewHolder? = null
            var winnerScore = -1
            val dx = curX - selected._itemView!!.getLeft()
            val dy = curY - selected._itemView!!.getTop()
            val targetsSize = dropTargets!!.size
            for (i in 0..targetsSize - 1) {
                val target = dropTargets.get(i)
                if (dx > 0) {
                    val diff = target!!._itemView!!.getRight() - right
                    if (diff < 0 && target._itemView!!.getRight() > selected._itemView!!.getRight()) {
                        val score = Math.abs(diff)
                        if (score > winnerScore) {
                            winnerScore = score
                            winner = target
                        }
                    }
                }
                if (dx < 0) {
                    val diff = target!!._itemView!!.getLeft() - curX
                    if (diff > 0 && target._itemView!!.getLeft() < selected._itemView!!.getLeft()) {
                        val score = Math.abs(diff)
                        if (score > winnerScore) {
                            winnerScore = score
                            winner = target
                        }
                    }
                }
                if (dy < 0) {
                    val diff = target!!._itemView!!.getTop() - curY
                    if (diff > 0 && target._itemView!!.getTop() < selected._itemView!!.getTop()) {
                        val score = Math.abs(diff)
                        if (score > winnerScore) {
                            winnerScore = score
                            winner = target
                        }
                    }
                }
                if (dy > 0) {
                    val diff = target!!._itemView!!.getBottom() - bottom
                    if (diff < 0 && target._itemView!!.getBottom() > selected._itemView!!.getBottom()) {
                        val score = Math.abs(diff)
                        if (score > winnerScore) {
                            winnerScore = score
                            winner = target
                        }
                    }
                }
            }
            return winner
        }

        abstract fun onSwiped(viewHolder: RecyclerView.ViewHolder?, direction: Int)

        fun onSelectedChanged(viewHolder: RecyclerView.ViewHolder?, actionState: Int) {
            if (viewHolder != null) {
                _UICallback?.onSelected(viewHolder._itemView)
            }
        }

        fun getMaxDragScroll(recyclerView: RecyclerView?): Int {
            if (_cachedMaxScrollSpeed == -1) {
                _cachedMaxScrollSpeed = recyclerView!!.getResources().getDimensionPixelSize(R.dimen.item_touch_helper_max_drag_scroll_per_frame)
            }
            return _cachedMaxScrollSpeed
        }

        fun onMoved(recyclerView: RecyclerView?, viewHolder: RecyclerView.ViewHolder?, fromPos: Int, target: RecyclerView.ViewHolder?, toPos: Int, x: Int, y: Int) {
            val layoutManager = recyclerView?.getLayoutManager()
            if (layoutManager is ViewDropHandler) {
                layoutManager.prepareForDrop(viewHolder?._itemView, target?._itemView, x, y)
                return
            }
            if (layoutManager!!.canScrollHorizontally()) {
                val minLeft = layoutManager.getDecoratedLeft(target?._itemView)
                if (minLeft <= recyclerView!!.getPaddingLeft()) {
                    recyclerView.scrollToPosition(toPos)
                }
                val maxRight = layoutManager.getDecoratedRight(target?._itemView)
                if (maxRight >= recyclerView.getWidth() - recyclerView.getPaddingRight()) {
                    recyclerView.scrollToPosition(toPos)
                }
            }
            if (layoutManager.canScrollVertically()) {
                val minTop = layoutManager.getDecoratedTop(target?._itemView)
                if (minTop <= recyclerView!!.getPaddingTop()) {
                    recyclerView.scrollToPosition(toPos)
                }
                val maxBottom = layoutManager.getDecoratedBottom(target?._itemView)
                if (maxBottom >= recyclerView.getHeight() - recyclerView.getPaddingBottom()) {
                    recyclerView.scrollToPosition(toPos)
                }
            }
        }

        fun onDraw(c: Canvas?, parent: RecyclerView?, selected: RecyclerView.ViewHolder?, recoverAnimationList: MutableList<ItemTouchHelper.RecoverAnimation?>?, actionState: Int, dX: Float, dY: Float) {
            val recoverAnimSize = recoverAnimationList!!.size
            for (i in 0..recoverAnimSize - 1) {
                val anim = recoverAnimationList.get(i)
                anim?.update()
                val count = c!!.save()
                onChildDraw(c, parent, anim?._viewHolder, anim!!._x, anim._y, anim._actionState, false)
                c.restoreToCount(count)
            }
            if (selected != null) {
                val count = c!!.save()
                onChildDraw(c, parent, selected, dX, dY, actionState, true)
                c.restoreToCount(count)
            }
        }

        fun onDrawOver(c: Canvas?, parent: RecyclerView?, selected: RecyclerView.ViewHolder?, recoverAnimationList: MutableList<ItemTouchHelper.RecoverAnimation?>?, actionState: Int, dX: Float, dY: Float) {
            val recoverAnimSize = recoverAnimationList!!.size
            for (i in 0..recoverAnimSize - 1) {
                val anim = recoverAnimationList.get(i)
                val count = c!!.save()
                onChildDrawOver(c, parent, anim?._viewHolder, anim!!._x, anim._y, anim._actionState, false)
                c.restoreToCount(count)
            }
            if (selected != null) {
                val count = c!!.save()
                onChildDrawOver(c, parent, selected, dX, dY, actionState, true)
                c.restoreToCount(count)
            }
            var hasRunningAnimation = false
            for (i in recoverAnimSize - 1 downTo 0) {
                val anim = recoverAnimationList.get(i)
                if (anim!!._ended && !anim._isPendingCleanup) {
                    recoverAnimationList.removeAt(i)
                    anim._viewHolder?.setIsRecyclable(true)
                } else if (!anim._ended) {
                    hasRunningAnimation = true
                }
            }
            if (hasRunningAnimation) {
                parent?.invalidate()
            }
        }

        fun clearView(recyclerView: RecyclerView?, viewHolder: RecyclerView.ViewHolder?) {
            _UICallback?.clearView(viewHolder?._itemView)
        }

        fun onChildDraw(c: Canvas?, recyclerView: RecyclerView?, viewHolder: RecyclerView.ViewHolder?, dX: Float, dY: Float, actionState: Int, isCurrentlyActive: Boolean) {
            _UICallback?.onDraw(c, recyclerView, viewHolder?._itemView, dX, dY, actionState, isCurrentlyActive)
        }

        fun onChildDrawOver(c: Canvas?, recyclerView: RecyclerView?, viewHolder: RecyclerView.ViewHolder?, dX: Float, dY: Float, actionState: Int, isCurrentlyActive: Boolean) {
            _UICallback?.onDrawOver(c, recyclerView, viewHolder?._itemView, dX, dY, actionState, isCurrentlyActive)
        }

        fun getAnimationDuration(recyclerView: RecyclerView?, animationType: Int, animateDx: Float, animateDy: Float): Long {
            val itemAnimator = recyclerView?.getItemAnimator()
            if (itemAnimator == null) {
                return if (animationType == ANIMATION_TYPE_DRAG)  DEFAULT_DRAG_ANIMATION_DURATION else DEFAULT_SWIPE_ANIMATION_DURATION
            } else {
                return if (animationType == ANIMATION_TYPE_DRAG) itemAnimator.getMoveDuration() else itemAnimator.getRemoveDuration()
            }
        }

        fun interpolateOutOfBoundsScroll(recyclerView: RecyclerView?, viewSize: Int, viewSizeOutOfBounds: Int, totalSize: Int, msSinceStartScroll: Long): Int {
            val maxScroll = getMaxDragScroll(recyclerView)
            val absOutOfBounds = Math.abs(viewSizeOutOfBounds)
            val direction = Math.signum(viewSizeOutOfBounds.toDouble()).toInt()
            val outOfBoundsRatio = Math.min(1f, 1f * absOutOfBounds / viewSize)
            val cappedScroll = (direction * maxScroll * _dragViewScrollCapInterpolator.getInterpolation(outOfBoundsRatio)).toInt()
            var timeRatio: Float
            if (msSinceStartScroll > DRAG_SCROLL_ACCELERATION_LIMIT_TIME_MS) {
                timeRatio = 1.0f
            } else {
                timeRatio = (msSinceStartScroll / DRAG_SCROLL_ACCELERATION_LIMIT_TIME_MS).toFloat()
            }
            val value = (cappedScroll * _dragScrollInterpolator.getInterpolation(timeRatio)).toInt()
            if (value == 0) {
                return if (viewSizeOutOfBounds > 0) 1 else -1
            }
            return value
        }
    }

    inner class ItemTouchHelperGestureListener: GestureDetector.SimpleOnGestureListener() {

        override fun onDown(e: MotionEvent?): Boolean {
            return true;
        }

        override fun onLongPress(e: MotionEvent?) {
            val child = findChildView(e)
            if (child != null) {
                val vh = _recyclerView?.getChildViewHolder(child)
                if (vh != null) {
                    if (!_callback!!.hasDragFlag(_recyclerView, vh)) {
                        return;
                    }
                    val pointerId = MotionEventCompat.getPointerId(e, 0)
                    if (pointerId == _activePointerId) {
                        val index = MotionEventCompat.findPointerIndex(e, _activePointerId)
                        val x = MotionEventCompat.getX(e, index);
                        val y = MotionEventCompat.getY(e, index);
                        _initialTouchX = x
                        _initialTouchY = y
                        _dx = 0.0f
                        _dy = 0.0f
                        if (_callback!!.isLongPressDragEnabled()) {
                            select(vh, ACTION_STATE_DRAG)
                        }
                    }
                }
            }
        }
    }

    abstract class SimpleCallback: Callback {

        var _defaultSwipeDirs = 0
        var _defaultDragDirs = 0

        constructor(dragDirs: Int, swipeDirs: Int) {
            _defaultSwipeDirs = swipeDirs
            _defaultDragDirs = dragDirs
        }

        open fun setDefaultSwipeDirs(defaultSwipeDirs: Int) {
            _defaultSwipeDirs = defaultSwipeDirs
        }

        open fun setDefaultDragDirs(defaultDragDirs: Int) {
            _defaultDragDirs = defaultDragDirs
        }

        open fun getSwipeDirs(recyclerView: RecyclerView?, viewHolder: RecyclerView.ViewHolder?): Int {
            return _defaultSwipeDirs
        }
        open fun getDragDirs(recyclerView: RecyclerView?, viewHolder: RecyclerView.ViewHolder?): Int {
            return _defaultDragDirs
        }

        override fun getMovementFlags(recyclerView: RecyclerView?, viewHolder: RecyclerView.ViewHolder?): Int {
            return makeMovementFlags(getDragDirs(recyclerView, viewHolder), getSwipeDirs(recyclerView, viewHolder))
        }
    }

    fun findChildView(event: MotionEvent?): View? {
        val x = event!!.getX()
        val y = event.getY()
        if (_selected != null) {
            val selectedView = _selected?._itemView
            if (hitTest(selectedView, x, y, _selectedStartX + _dx, _selectedStartY + _dy)) {
                return selectedView
            }
        }
        for (i in _recoverAnimations.size - 1 downTo 0) {
            val anim = _recoverAnimations.get(i)
            val view = anim?._viewHolder?._itemView
            if (hitTest(view, x, y, anim!!._x, anim._y)) {
                return view
            }
        }
        return _recyclerView?.findChildViewUnder(x, y)
    }

    override fun onChildViewAttachedToWindow(view: View?) { }

    override fun onChildViewDetachedFromWindow(view: View?) {
        removeChildDrawingOrderCallbackIfNecessary(view)
        val holder = _recyclerView?.getChildViewHolder(view)
        if (holder == null) {
            return
        }
        if (_selected != null && holder == _selected) {
            select(null, ACTION_STATE_IDLE)
        } else {
            endRecoverAnimation(holder, false)
            if (_pendingCleanup.remove(holder._itemView)) {
                _callback?.clearView(_recyclerView, holder)
            }
        }
    }

    override fun onDraw(c: Canvas?, parent: RecyclerView?, state: RecyclerView.State?) {
        _overdrawChildPosition = -1
        var dx = 0.0f
        var dy = 0.0f
        if (_selected != null) {
            getSelectedDxDy(_tmpPosition)
            dx = _tmpPosition[0]
            dy = _tmpPosition[1]
        }
        _callback?.onDraw(c, parent, _selected, _recoverAnimations, _actionState, dx, dy)
    }

    override fun onDrawOver(c: Canvas?, parent: RecyclerView?, state: RecyclerView.State?) {
        var dx = 0.0f
        var dy = 0.0f
        if (_selected != null) {
            getSelectedDxDy(_tmpPosition)
            dx = _tmpPosition[0]
            dy = _tmpPosition[1]
        }
        _callback?.onDrawOver(c, parent, _selected, _recoverAnimations, _actionState, dx, dy)
    }

    override fun getItemOffsets(outRect: Rect?, view: View?, parent: RecyclerView?, state: RecyclerView.State?) {
        outRect?.setEmpty()
    }

    fun startSwipe(viewHolder: RecyclerView.ViewHolder?) {
        if (!_callback!!.hasSwipeFlag(_recyclerView, viewHolder)) {
            return
        }
        if (viewHolder?._itemView?.getParent() != _recyclerView) {
            return
        }
        obtainVelocityTracker()
        _dx = 0.0f
        _dy = 0.0f
        select(viewHolder, ACTION_STATE_SWIPE)
    }

    fun startDrag(viewHolder: RecyclerView.ViewHolder?) {
        if (!_callback!!.hasDragFlag(_recyclerView, viewHolder)) {
            return
        }
        if (viewHolder?._itemView?.getParent() != _recyclerView) {
            return
        }
        obtainVelocityTracker()
        _dx = 0.0f
        _dy = 0.0f
        select(viewHolder, ACTION_STATE_DRAG)
    }

    fun attachToRecyclerView(recyclerView: RecyclerView?) {
        if (_recyclerView == recyclerView) {
            return
        }
        if (_recyclerView != null) {
            destroyCallbacks()
        }
        _recyclerView = recyclerView;
        if (_recyclerView != null) {
            setupCallbacks()
        }
    }

    private fun setupCallbacks() {
        val vc = ViewConfiguration.get(_recyclerView?.getContext())
        _slop = vc.getScaledTouchSlop()
        _recyclerView?.addItemDecoration(this)
        _recyclerView?.addOnItemTouchListener(_onItemTouchListener)
        _recyclerView?.addOnChildAttachStateChangeListener(this)
        initGestureDetector()
    }

    private fun destroyCallbacks() {
        _recyclerView?.removeItemDecoration(this)
        _recyclerView?.removeOnItemTouchListener(_onItemTouchListener)
        _recyclerView?.removeOnChildAttachStateChangeListener(this)
        val recoverAnimSize = _recoverAnimations.size
        for (i in recoverAnimSize - 1 downTo 0) {
            val recoverAnimation = _recoverAnimations.get(0)
            _callback?.clearView(_recyclerView, recoverAnimation?._viewHolder)
        }
        _recoverAnimations.clear()
        _overdrawChild = null
        _overdrawChildPosition = -1
        releaseVelocityTracker()
    }

    private fun initGestureDetector() {
        if (_gestureDetector != null) {
            return
        }
        _gestureDetector = GestureDetectorCompat(_recyclerView?.getContext(), ItemTouchHelperGestureListener())
    }
}