package com.hujiang.devart.component.draggrid

import android.app.Activity
import android.content.Context
import android.graphics.Point
import android.os.Handler
import android.os.SystemClock
import android.util.AttributeSet
import android.util.DisplayMetrics
import android.view.MotionEvent
import android.view.View
import android.view.ViewGroup
import android.view.animation.*
import android.widget.AdapterView
import android.widget.ImageView
import java.util.*

/**
 * Created by rarnu on 4/14/16.
 */
class DraggableGridView: ViewGroup, View.OnTouchListener, View.OnClickListener, View.OnLongClickListener {

    companion object {
        val childRatio = 0.9f
        val animT = 150
    }

    protected var _colCount = 0
    protected var _childSize = 0
    protected var _padding = 0
    protected var _dpi = 0
    protected var _scroll = 0
    protected var _lastDelta = 0.0f
    protected var _handler = Handler()
    protected var _dragged = -1
    protected var _lastX = -1
    protected var _lastY = -1
    protected var _lastTarget = -1
    protected var _enabled = true
    protected var _touching = false
    protected var _newPositions = arrayListOf<Int>()
    protected var _onRearrangeListener: OnRearrangeListener? = null
    protected var _secondaryOnClickListener: OnClickListener? = null
    private var _onItemClickListener: AdapterView.OnItemClickListener? = null

    protected var _updateTask = object : Runnable {
        override fun run() {
            if (_dragged != -1) {
                if (_lastY < _padding * 3 && _scroll > 0)
                    _scroll -= 20
                else if (_lastY > bottom - top - (_padding * 3) && _scroll < getMaxScroll())
                    _scroll += 20
            }
            else if (_lastDelta != 0.0f && !_touching) {
                _scroll += _lastDelta.toInt()
                _lastDelta *= 0.9f
                if (Math.abs(_lastDelta) < .25)
                    _lastDelta = 0.0f
            }
            clampScroll()
            onLayout(true, left, top, right, bottom)
            _handler.postDelayed(this, 25)
        }
    }

    protected fun clampScroll() {
        val stretch = 3
        val overreach = (height / 2).toInt()
        var max = getMaxScroll()
        max = Math.max(max, 0)
        if (_scroll < -overreach) {
            _scroll = -overreach
            _lastDelta = 0.0f
        } else if (_scroll > max + overreach) {
            _scroll = max + overreach
            _lastDelta = 0.0f
        } else if (_scroll < 0) {
            if (_scroll >= -stretch) {
                _scroll = 0
            } else if (!_touching) {
                _scroll -= _scroll / stretch
            }
        } else if (_scroll > max) {
            if (_scroll <= max + stretch) {
                _scroll = max
            } else if (!_touching) {
                _scroll += (max - _scroll) / stretch
            }
        }
    }

    protected fun getMaxScroll(): Int {
        val rowCount = Math.ceil(childCount.toDouble()/ _colCount).toInt()
        val max = rowCount * _childSize + (rowCount + 1) * _padding - height
        return max
    }

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        setListeners()
        _handler.removeCallbacks(_updateTask)
        _handler.postAtTime(_updateTask, SystemClock.uptimeMillis() + 500)
        isChildrenDrawingOrderEnabled = true
        val metrics = DisplayMetrics()
        (context as Activity).windowManager.defaultDisplay.getMetrics(metrics)
        _dpi = metrics.densityDpi
    }

    protected fun setListeners() {
        setOnTouchListener(this)
        super.setOnClickListener(this)
        setOnLongClickListener(this)
    }

    override fun setOnClickListener(l: OnClickListener?) {
        _secondaryOnClickListener = l
    }

    override fun addView(child: View?) {
        super.addView(child)
        _newPositions.add(-1)
    }

    override fun removeViewAt(index: Int) {
        super.removeViewAt(index)
        _newPositions.remove(index)
    }

    override fun onLayout(changed: Boolean, l: Int, t: Int, r: Int, b: Int) {
        var w = (r - l) * 1.0f / (_dpi * 1.0f / 160f)
        _colCount = 2
        var sub = 240
        w -= 280
        while (w > 0) {
            _colCount++
            w -= sub
            sub += 40
        }
        _childSize = (r - l) / _colCount
        _childSize = Math.round(_childSize * childRatio)
        _padding = ((r - l) - (_childSize * _colCount)) / (_colCount + 1)

        for (i in 0..childCount - 1) {
            if (i != _dragged) {
                val xy = getCoorFromIndex(i)
                getChildAt(i).layout(xy.x, xy.y, xy.x + _childSize, xy.y + _childSize)
            }
        }
    }

    protected fun getCoorFromIndex(index: Int): Point {
        val col = index % _colCount
        val row = index / _colCount
        return Point(_padding + (_childSize + _padding) * col, _padding + (_childSize + _padding) * row - _scroll)
    }

    protected fun getTargetFromCoor(x: Int, y: Int): Int {
        if (getColOrRowFromCoor(y + _scroll) == -1) {
            return -1
        }
        val leftPos = getIndexFromCoor(x - (_childSize / 4), y)
        val rightPos = getIndexFromCoor(x + (_childSize / 4), y)
        if (leftPos == -1 && rightPos == -1) {
            return -1
        }
        if (leftPos == rightPos) {
            return -1
        }
        var target = -1
        if (rightPos > -1) {
            target = rightPos
        } else if (leftPos > -1) {
            target = leftPos + 1
        }
        if (_dragged < target) {
            return target - 1
        }
        return target
    }

    fun getIndexFromCoor(x: Int, y: Int): Int {
        val col = getColOrRowFromCoor(x)
        val row = getColOrRowFromCoor(y + _scroll)
        if (col == -1 || row == -1) {
            return -1
        }
        val index = row * _colCount + col
        if (index >= childCount) {
            return -1
        }
        return index
    }

    protected fun getColOrRowFromCoor(coor: Int): Int {
        var ncoor = coor - _padding
        var i = 0
        while (ncoor > 0) {
            if (ncoor < _childSize) {
                return i
            }
            ncoor -= (_childSize + _padding)
            ++i
        }
        return -1
    }

    protected fun animateGap(target: Int) {
        for (i in 0..childCount - 1) {
            val v = getChildAt(i)
            if (i == _dragged) {
                continue
            }
            var newPos = i
            if (_dragged < target && i >= _dragged + 1 && i <= target) {
                newPos--
            } else if (target < _dragged && i >= target && i < _dragged) {
                newPos++
            }
            var oldPos = i
            if (_newPositions[i] != -1)
                oldPos = _newPositions[i]
            if (oldPos == newPos) {
                continue
            }
            val oldXY = getCoorFromIndex(oldPos)
            val newXY = getCoorFromIndex(newPos)
            val oldOffset = Point(oldXY.x - v.left, oldXY.y - v.top)
            val newOffset = Point(newXY.x - v.left, newXY.y - v.top)
            val translate = TranslateAnimation(Animation.ABSOLUTE, oldOffset.x.toFloat(), Animation.ABSOLUTE, newOffset.x.toFloat(), Animation.ABSOLUTE, oldOffset.y.toFloat(), Animation.ABSOLUTE, newOffset.y.toFloat())
            translate.duration = animT.toLong()
            translate.isFillEnabled = true
            translate.fillAfter = true
            v.clearAnimation()
            v.startAnimation(translate)
            _newPositions[i] = newPos
        }
    }

    protected fun reorderChildren() {
        _onRearrangeListener?.onRearrange(_dragged, _lastTarget)
        val children = arrayListOf<View>()
        for (i in 0..childCount - 1) {
            getChildAt(i).clearAnimation()
            children.add(getChildAt(i))
        }
        removeAllViews()
        while (_dragged != _lastTarget) {
            if (_lastTarget == children.size) {
                children.add(children.removeAt(_dragged))
                _dragged = _lastTarget
            } else if (_dragged < _lastTarget) {
                Collections.swap(children, _dragged, _dragged + 1)
                _dragged++
            } else if (_dragged > _lastTarget) {
                Collections.swap(children, _dragged, _dragged - 1)
                _dragged--
            }
        }
        for (i in 0..children.size - 1) {
            _newPositions[i] = -1
            addView(children[i])
        }
        onLayout(true, left, top, right, bottom)
    }

    override fun onTouch(v: View?, event: MotionEvent?): Boolean {
        val action = event!!.action
        when (action and MotionEvent.ACTION_MASK) {
            MotionEvent.ACTION_DOWN -> {
                _enabled = true
                _lastX = event.x.toInt()
                _lastY = event.y.toInt()
                _touching = true
            }
            MotionEvent.ACTION_MOVE -> {
                val delta = _lastY - event.y.toInt()
                if (_dragged != -1) {
                    val x = event.x.toInt()
                    val y = event.y.toInt()
                    val l = x - (3 * _childSize / 4)
                    val t = y - (3 * _childSize / 4)
                    getChildAt(_dragged).layout(l, t, l + (_childSize * 3 / 2), t + (_childSize * 3 / 2))
                    val target = getTargetFromCoor(x, y)
                    if (_lastTarget != target) {
                        if (target != -1) {
                            animateGap(target)
                            _lastTarget = target
                        }
                    }
                } else {
                    _scroll += delta
                    clampScroll()
                    if (Math.abs(delta) > 2) {
                        _enabled = false
                    }
                    onLayout(true, left, top, right, bottom)
                }
                _lastX = event.x.toInt()
                _lastY = event.y.toInt()
                _lastDelta = delta.toFloat()
            }
            MotionEvent.ACTION_UP -> {
                if (_dragged != -1) {
                    val v = getChildAt(_dragged)
                    if (_lastTarget != -1) {
                        reorderChildren()
                    } else {
                        val xy = getCoorFromIndex(_dragged)
                        v.layout(xy.x, xy.y, xy.x + _childSize, xy.y + _childSize)
                    }
                    v.clearAnimation()
                    if (v is ImageView) {
                        v.imageAlpha = 255
                    }
                    _lastTarget = -1
                    _dragged = -1
                }
                _touching = false
            }
        }
        if (_dragged != -1) {
            return true
        }
        return false
    }

    override fun onClick(v: View?) {
        if (_enabled) {
            _secondaryOnClickListener?.onClick(v)
            if (_onItemClickListener != null && getLastIndex() != -1) {
                _onItemClickListener?.onItemClick(null, getChildAt(getLastIndex()), getLastIndex(), getLastIndex().toLong() / _colCount)
            }
        }
    }

    fun getLastIndex(): Int = getIndexFromCoor(_lastX, _lastY)

    override fun onLongClick(v: View?): Boolean {
        if (!_enabled) {
            return false
        }
        val index = getLastIndex()
        if (index != -1) {
            _dragged = index
            animateDragged()
            return true
        }
        return false
    }

    protected fun animateDragged() {
        val v = getChildAt(_dragged)
        val x = getCoorFromIndex(_dragged).x + _childSize / 2
        val y = getCoorFromIndex(_dragged).y + _childSize / 2
        val l = x - (3 * _childSize / 4)
        val t = y - (3 * _childSize / 4)
        v.layout(l, t, l + (_childSize * 3 / 2), t + (_childSize * 3 / 2))
        val animSet = AnimationSet(true)
        val scale = ScaleAnimation(0.667f, 1.0f, 0.667f, 1.0f, _childSize * 3.0f / 4, _childSize * 3.0f / 4)
        scale.duration = animT.toLong()
        val alpha = AlphaAnimation(1.0f, 0.5f)
        alpha.duration = animT.toLong()
        animSet.addAnimation(scale)
        animSet.addAnimation(alpha)
        animSet.isFillEnabled = true
        animSet.fillAfter = true
        v.clearAnimation()
        v.startAnimation(animSet)
    }

    override fun getChildDrawingOrder(childCount: Int, i: Int): Int {
        if (_dragged == -1) {
            return i
        } else if (i == childCount - 1) {
            return _dragged
        } else if (i >= _dragged) {
            return i + 1
        }
        return i
    }

    fun getIndexOf(child: View?): Int {
        for (i in 0..childCount - 1) {
            if (getChildAt(i) == child) {
                return i
            }
        }
        return -1
    }

    fun scrollToTop() {
        _scroll = 0
    }

    fun scrollToBottom() {
        _scroll = Integer.MAX_VALUE
        clampScroll()
    }

    fun setOnRearrangeListener(l: OnRearrangeListener?) {
        _onRearrangeListener = l
    }

    fun setOnItemClickListener(l: AdapterView.OnItemClickListener?) {
        _onItemClickListener = l
    }
}