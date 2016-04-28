package com.hujiang.devart.component.gesturelock

import android.content.Context
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Paint
import android.graphics.Path
import android.util.AttributeSet
import android.view.MotionEvent
import android.view.View
import java.util.*

/**
 * Created by rarnu on 4/5/16.
 */
class GestureLockView : View {

    companion object {
        private val OUT_CYCLE_NORMAL = Color.rgb(108, 119, 138)
        private val OUT_CYCLE_ONTOUCH = Color.rgb(25, 66, 103)
        private val INNER_CYCLE_TOUCHED = Color.rgb(2, 210, 255)
        private val INNER_CYCLE_NOTOUCH = Color.rgb(100, 100, 100)
        private val LINE_COLOR = Color.argb(127, 2, 210, 255)
        private val ERROR_COLOR = Color.argb(127, 255, 0, 0)
        private val INNER_CYCLE_ERROR_COLOR = Color.rgb(255, 0, 0)
    }

    private var _paintNormal: Paint? = null
    private var _paintOnTouch: Paint? = null
    private var _paintInnerCycle: Paint? = null
    private var _paintLines: Paint? = null
    private var _paintKeyError: Paint? = null
    private var _cycles: Array<LockCircle?>? = null
    private var _linePath = Path()
    private var _linedCycles = arrayListOf<Int>()
    private var _onGestureFinishListener: OnGestureFinishListener? = null
    var onGestureFinishListener: OnGestureFinishListener?
        get() = _onGestureFinishListener
        set(value) { _onGestureFinishListener = value }
    private var _key = ""
    var key: String
        get() = _key
        set(value) { _key = value }
    private var _eventX = 0
    private var _eventY = 0
    private var _canContinue = true
    private var _result = false
    private var _timer: Timer? = null
    private var _circleInLine = 3
    var circleInLine: Int
        get() = _circleInLine
        set(value) { _circleInLine = value }

    interface OnGestureFinishListener {
        fun OnGestureFinish(success: Boolean, key: String?)
    }

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int) : super(context, attrs, defStyle) {
        init()
    }

    constructor(context: Context, attrs: AttributeSet?) : this(context, attrs, 0)

    constructor(context: Context) : this(context, null)

    private fun init() {
        _paintNormal = Paint()
        _paintNormal?.isAntiAlias = true
        _paintNormal?.strokeWidth = 5.toFloat()
        _paintNormal?.style = Paint.Style.STROKE

        _paintOnTouch = Paint()
        _paintOnTouch?.isAntiAlias = true
        _paintOnTouch?.strokeWidth = 10.toFloat()
        _paintOnTouch?.style = Paint.Style.STROKE

        _paintInnerCycle = Paint()
        _paintInnerCycle?.isAntiAlias = true
        _paintInnerCycle?.style = Paint.Style.FILL

        _paintLines = Paint()
        _paintLines?.isAntiAlias = true
        _paintLines?.style = Paint.Style.STROKE
        _paintLines?.strokeWidth = 25.toFloat()

        _paintKeyError = Paint()
        _paintKeyError?.isAntiAlias = true
        _paintKeyError?.style = Paint.Style.STROKE
        _paintKeyError?.strokeWidth = 3.toFloat()
    }


    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        super.onMeasure(widthMeasureSpec, widthMeasureSpec)
    }

    override fun onLayout(changed: Boolean, left: Int, top: Int, right: Int, bottom: Int) {
        super.onLayout(changed, left, top, right, bottom)
        var perSize = width / (_circleInLine * 2)
        if (_cycles == null && perSize > 0) {
            _cycles = arrayOfNulls<LockCircle>(_circleInLine * _circleInLine)
            for (i in 0.._circleInLine - 1) {
                for (j in 0.._circleInLine - 1) {
                    val cycle = LockCircle()
                    cycle.num = i * _circleInLine + j
                    cycle.ox = perSize * (j * 2 + 1)
                    cycle.oy = perSize * (i * 2 + 1)
                    cycle.r = perSize * 0.5f
                    _cycles!![i * _circleInLine + j] = cycle
                }
            }
        }
    }

    override fun onDraw(canvas: Canvas?) {
        super.onDraw(canvas)
        for (i in 0.._cycles!!.size - 1) {
            if (!_canContinue && !_result) {
                _paintOnTouch?.color = ERROR_COLOR
                _paintInnerCycle?.color = INNER_CYCLE_ERROR_COLOR
                _paintLines?.color = ERROR_COLOR
            } else if (_cycles!![i]!!.onTouch) {
                _paintOnTouch?.color = OUT_CYCLE_ONTOUCH
                _paintInnerCycle?.color = INNER_CYCLE_TOUCHED
                _paintLines?.color = LINE_COLOR
            } else {
                _paintNormal?.color = OUT_CYCLE_NORMAL
                _paintInnerCycle?.color = INNER_CYCLE_TOUCHED
                _paintLines?.color = LINE_COLOR
            }
            if (_cycles!![i]!!.onTouch) {
                if (_canContinue || _result) {
                    _paintInnerCycle?.color = INNER_CYCLE_TOUCHED
                } else {
                    _paintInnerCycle?.color = INNER_CYCLE_ERROR_COLOR
                }
                canvas?.drawCircle(_cycles!![i]!!.ox.toFloat(), _cycles!![i]!!.oy.toFloat(), _cycles!![i]!!.r, _paintOnTouch)
                drawInnerBlueCycle(_cycles!![i], canvas)
            } else {
                _paintInnerCycle?.color = INNER_CYCLE_NOTOUCH
                canvas?.drawCircle(_cycles!![i]!!.ox.toFloat(), _cycles!![i]!!.oy.toFloat(), _cycles!![i]!!.r, _paintNormal)
                drawInnerBlueCycle(_cycles!![i], canvas)
            }
        }
        drawLine(canvas)
    }

    private fun drawInnerBlueCycle(myCycle: LockCircle?, canvas: Canvas?) {
        canvas?.drawCircle(myCycle!!.ox.toFloat(), myCycle.oy.toFloat(), myCycle.r / 1.5f, _paintInnerCycle)
    }

    private fun drawLine(canvas: Canvas?) {
        _linePath.reset()
        if (_linedCycles.size > 0) {
            for (i in 0.._linedCycles.size - 1) {
                val index = _linedCycles[i]
                val x = _cycles!![index]!!.ox
                val y = _cycles!![index]!!.oy
                if (i == 0) {
                    _linePath.moveTo(x.toFloat(), y.toFloat())
                } else {
                    _linePath.lineTo(x.toFloat(), y.toFloat())
                }
            }
            if (_canContinue) {
                _linePath.lineTo(_eventX.toFloat(), _eventY.toFloat())
            } else {
                _linePath.lineTo(_cycles!![_linedCycles[_linedCycles.size - 1]]!!.ox.toFloat(), _cycles!![_linedCycles[_linedCycles.size - 1]]!!.oy.toFloat())
            }
            canvas?.drawPath(_linePath, _paintLines)

        }
    }

    override fun onTouchEvent(event: MotionEvent?): Boolean {
        if (_canContinue) {
            when (event!!.action) {
                MotionEvent.ACTION_DOWN,
                MotionEvent.ACTION_MOVE -> {
                    _eventX = event.x.toInt()
                    _eventY = event.y.toInt()
                    for (i in 0.._cycles!!.size - 1) {
                        if (_cycles!![i]!!.isPointIn(_eventX, _eventY)) {
                            _cycles!![i]!!.onTouch = true
                            if (!_linedCycles.contains(_cycles!![i]!!.num)) {
                                _linedCycles.add(_cycles!![i]!!.num)
                            }
                        }
                    }
                }
                MotionEvent.ACTION_UP -> {
                    _canContinue = false
                    val sb = StringBuffer()
                    for (i in 0.._linedCycles.size - 1) {
                        sb.append(String.format("%02d", _linedCycles[i]))
                    }
                    _result = _key == sb.toString()
                    _onGestureFinishListener?.OnGestureFinish(_result, sb.toString())
                    _timer = Timer()
                    _timer?.schedule(object : TimerTask() {
                        override fun run() {
                            _eventX = 0
                            _eventY = 0
                            for (i in 0.._cycles!!.size - 1) {
                                _cycles!![i]!!.onTouch = false
                            }
                            _linedCycles.clear()
                            _linePath.reset()
                            _canContinue = true
                            postInvalidate()
                        }
                    }, 1000)
                }
            }
            invalidate()
        }
        return true
    }
}


