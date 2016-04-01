package com.hujiang.devart.component.draglist

import android.graphics.Point
import android.view.*
import android.widget.AdapterView

/**
 * Created by rarnu on 4/1/16.
 */
class DragController: FloatViewManager, View.OnTouchListener, GestureDetector.OnGestureListener {

    companion object {
        val ON_DOWN = 0
        val ON_DRAG = 1
        val ON_LONG_PRESS = 2
        val CLICK_REMOVE = 0
        val FLING_REMOVE = 1
        val MISS = -1
    }

    private var _dragInitMode = ON_DOWN
    private var _sortEnabled = true
    private var _removeMode = 0
    private var _removeEnabled = false
    private var _isRemoving = false
    private var _detector: GestureDetector? = null
    private var _flingRemoveDetector: GestureDetector? = null
    private var _touchSlop = 0
    private var _hitPos = MISS
    private var _flingHitPos = MISS
    private var _clickRemoveHitPos = MISS
    private var _tempLoc = IntArray(2)
    private var _itemX = 0
    private var _itemY = 0
    private var _currX = 0
    private var _currY = 0
    private var _dragging = false
    private var _flingSpeed = 500.0f
    private var _dragHandleId = 0
    private var _clickRemoveId = 0
    private var _flingHandleId = 0
    private var _canDrag = false
    private var _listview: DragListView? = null
    private var _positionX = 0
    private var _flingRemoveListener = object : GestureDetector.SimpleOnGestureListener() {
        override fun onFling(e1: MotionEvent?, e2: MotionEvent?, velocityX: Float, velocityY: Float): Boolean {
            if (_removeEnabled && _isRemoving) {
                val w = _listview!!.width
                val minPos = w * 1.0f / 5;
                if (velocityX > _flingSpeed) {
                    if (_positionX > -minPos) {
                        _listview?.stopDragWithVelocity(true, velocityX)
                    }
                } else if (velocityX < -_flingSpeed) {
                    if (_positionX < minPos) {
                        _listview?.stopDragWithVelocity(true, velocityX)
                    }
                }
                _isRemoving = false
            }
            return false
        }
    }


    constructor(listView: DragListView?): this(listView, 0, ON_DOWN, FLING_REMOVE)

    constructor(listView: DragListView?, dragHandleId: Int, dragInitMode: Int, removeMode: Int): this(listView, dragHandleId, dragInitMode, removeMode, 0)

    constructor(listView: DragListView?, dragHandleId: Int, dragInitMode: Int, removeMode: Int, clickRemoveId: Int): this(listView, dragHandleId, dragInitMode, removeMode, clickRemoveId, 0)

    constructor(listView: DragListView?, dragHandleId: Int, dragInitMode: Int, removeMode: Int, clickRemoveId: Int, flingHandleId: Int): super(listView) {
        _listview = listView
        _detector = GestureDetector(listView!!.context, this)
        _flingRemoveDetector = GestureDetector(listView.context, _flingRemoveListener)
        _flingRemoveDetector?.setIsLongpressEnabled(false)
        _touchSlop = ViewConfiguration.get(listView.context).scaledTouchSlop
        _dragHandleId = dragHandleId
        _clickRemoveId = clickRemoveId
        _flingHandleId = flingHandleId
        setRemoveMode(removeMode)
        setDragInitMode(dragInitMode)
    }

    fun getTouchSlop(): Int = _touchSlop

    fun setTouchSlop(touchSlop: Int) {
        _touchSlop = touchSlop
    }

    fun getFlingSpeed(): Float = _flingSpeed

    fun setFlingSpeed(flingSpeed: Float) {
        _flingSpeed = flingSpeed
    }

    fun getDragInitMode(): Int = _dragInitMode

    fun setDragInitMode(mode: Int) {
        _dragInitMode = mode
    }

    fun isSortEnabled(): Boolean = _sortEnabled

    fun setSortEnabled(enabled: Boolean) {
        _sortEnabled = enabled
    }

    fun getRemoveMode(): Int = _removeMode

    fun setRemoveMode(mode: Int) {
        _removeMode = mode
    }

    fun isRemoveEnabled(): Boolean = _removeEnabled

    fun setRemoveEnabled(enabled: Boolean) {
        _removeEnabled = enabled
    }

    fun setDragHandleId(id: Int) {
        _dragHandleId = id
    }

    fun setFlingHandleId(id: Int) {
        _flingHandleId = id
    }

    fun setClickRemoveId(id: Int) {
        _clickRemoveId = id
    }

    fun startDrag(position: Int, deltaX: Int, deltaY: Int): Boolean {
        var dragFlags = 0
        if (_sortEnabled && !_isRemoving) {
            dragFlags = dragFlags or DragListView.DRAG_POS_Y or DragListView.DRAG_NEG_Y
        }
        if (_removeEnabled && _isRemoving) {
            dragFlags = dragFlags or DragListView.DRAG_POS_X
            dragFlags = dragFlags or DragListView.DRAG_NEG_X
        }
        _dragging = _listview!!.startDrag(position - _listview!!.headerViewsCount, dragFlags, deltaX, deltaY)
        return _dragging
    }

    override fun onTouch(v: View?, ev: MotionEvent?): Boolean {
        if (!_listview!!.isDragEnabled() || _listview!!.listViewIntercepted()) {
            return false
        }
        _detector?.onTouchEvent(ev)
        if (_removeEnabled && _dragging && _removeMode == FLING_REMOVE) {
            _flingRemoveDetector?.onTouchEvent(ev)
        }
        val action = ev!!.action and MotionEvent.ACTION_MASK
        when (action) {
            MotionEvent.ACTION_DOWN -> {
                _currX = ev.x.toInt()
                _currY = ev.y.toInt()
            }
            MotionEvent.ACTION_UP -> {
                if (_removeEnabled && _isRemoving) {
                    val x = if (_positionX >= 0) _positionX else -_positionX
                    val removePoint = _listview!!.width / 2
                    if (x > removePoint) {
                        _listview?.stopDragWithVelocity(true, 0.0f)
                    }
                }
                _isRemoving = false
                _dragging = false
            }
            MotionEvent.ACTION_CANCEL -> {
                _isRemoving = false
                _dragging = false
            }
        }
        return false
    }

    override fun onDragFloatView(floatView: View?, location: Point?, touch: Point?) {
        if (_removeEnabled && _isRemoving) {
            _positionX = location!!.x
        }
    }

    fun startDragPosition(ev: MotionEvent?): Int = dragHandleHitPosition(ev)

    fun startFlingPosition(ev: MotionEvent?): Int = if (_removeMode == FLING_REMOVE) flingHandleHitPosition(ev) else MISS

    fun dragHandleHitPosition(ev: MotionEvent?): Int = viewIdHitPosition(ev, _dragHandleId)

    fun flingHandleHitPosition(ev: MotionEvent?): Int = viewIdHitPosition(ev, _flingHandleId)

    fun viewIdHitPosition(ev: MotionEvent?, id: Int): Int {
        val x = ev!!.x.toInt()
        val y = ev.y.toInt()
        val touchPos = _listview!!.pointToPosition(x, y)
        val numHeaders = _listview!!.headerViewsCount
        val numFooters = _listview!!.footerViewsCount
        val count = _listview!!.count
        if (touchPos != AdapterView.INVALID_POSITION && touchPos >= numHeaders && touchPos < (count - numFooters)) {
            val item = _listview!!.getChildAt(touchPos - _listview!!.firstVisiblePosition)
            val rawX = ev.rawX.toInt()
            val rawY = ev.rawY.toInt()
            val dragBox = if (id == 0) item else item.findViewById(id)
            if (dragBox != null) {
                dragBox.getLocationOnScreen(_tempLoc)
                if (rawX > _tempLoc[0] && rawY > _tempLoc[1] && rawX < _tempLoc[0] + dragBox.width && rawY < _tempLoc[1] + dragBox.height) {
                    _itemX = item.left
                    _itemY = item.top
                    return touchPos
                }
            }
        }
        return MISS
    }

    override fun onSingleTapUp(ev: MotionEvent?): Boolean {
        if (_removeEnabled && _removeMode == CLICK_REMOVE) {
            if (_clickRemoveHitPos != MISS) {
                _listview!!.removeItem(_clickRemoveHitPos - _listview!!.headerViewsCount)
            }
        }
        return true;
    }

    override fun onDown(ev: MotionEvent?): Boolean {
        if (_removeEnabled && _removeMode == CLICK_REMOVE) {
            _clickRemoveHitPos = viewIdHitPosition(ev, _clickRemoveId)
        }
        _hitPos = startDragPosition(ev)
        if (_hitPos != MISS && _dragInitMode == ON_DOWN) {
            startDrag(_hitPos, ev!!.x.toInt() - _itemX, ev.y.toInt() - _itemY)
        }
        _isRemoving = false
        _canDrag = true
        _positionX = 0
        _flingHitPos = startFlingPosition(ev)
        return true
    }

    override fun onFling(e1: MotionEvent?, e2: MotionEvent?, velocityX: Float, velocityY: Float): Boolean = false

    override fun onScroll(e1: MotionEvent?, e2: MotionEvent?, distanceX: Float, distanceY: Float): Boolean {
        val x1 = e1!!.x.toInt()
        val y1 = e1.y.toInt()
        val x2 = e2!!.x.toInt()
        val y2 = e2.y.toInt()
        val deltaX = x2 - _itemX
        val deltaY = y2 - _itemY
        if (_canDrag && !_dragging && (_hitPos != MISS || _flingHitPos != MISS)) {
            if (_hitPos != MISS) {
                if (_dragInitMode == ON_DRAG && Math.abs(y2 - y1) > _touchSlop && _sortEnabled) {
                    startDrag(_hitPos, deltaX, deltaY)
                } else if (_dragInitMode != ON_DOWN && Math.abs(x2 - x1) > _touchSlop && _removeEnabled) {
                    _isRemoving = true
                    startDrag(_flingHitPos, deltaX, deltaY)
                }
            } else if (_flingHitPos != MISS) {
                if (Math.abs(x2 - x1) > _touchSlop && _removeEnabled) {
                    _isRemoving = true
                    startDrag(_flingHitPos, deltaX, deltaY)
                } else if (Math.abs(y2 - y1) > _touchSlop) {
                    _canDrag = false
                }
            }
        }
        return false
    }

    override fun onShowPress(e: MotionEvent?) { }

    override fun onLongPress(e: MotionEvent?) {
        if (_hitPos != MISS && _dragInitMode == ON_LONG_PRESS) {
            _listview!!.performHapticFeedback(HapticFeedbackConstants.LONG_PRESS)
            startDrag(_hitPos, _currX - _itemX, _currY - _itemY)
        }
    }
}