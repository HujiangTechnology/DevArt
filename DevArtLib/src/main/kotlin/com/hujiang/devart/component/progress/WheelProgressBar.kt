package com.hujiang.devart.component.progress

import android.content.Context
import android.content.res.TypedArray
import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.RectF
import android.graphics.Shader
import android.util.AttributeSet
import android.view.View
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/15/16.
 */
class WheelProgressBar: View {

    private var _layoutHeight = 0
    private var _layoutWidth = 0
    private var _fullRadius = 100
    private var _circleRadius = 80
    private var _barLength = 60
    private var _barWidth = 20
    private var _rimWidth = 20
    private var _textSize = 20
    private var _contourSize = 0.0f
    private var _paddingTop = 5
    private var _paddingBottom = 5
    private var _paddingLeft = 5
    private var _paddingRight = 5
    private var _barColor = 0xAA000000.toInt()
    private var _contourColor = 0xAA000000.toInt()
    private var _circleColor = 0x00000000.toInt()
    private var _rimColor = 0xAADDDDDD.toInt()
    private var _textColor = 0xFF000000.toInt()
    private var _barPaint = Paint()
    private var _circlePaint = Paint()
    private var _rimPaint = Paint()
    private var _textPaint = Paint()
    private var _contourPaint = Paint()
    private var _innerCircleBounds = RectF()
    private var _circleBounds = RectF()
    private var _circleOuterContour = RectF()
    private var _circleInnerContour = RectF()
    private var _spinSpeed = 2.0f
    private var _delayMillis = 10
    private var _progress = 0.0f
    private var _isSpinning = false
    private var _text: String? = ""
    private var _splitText: List<String?>? = listOf()

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        parseAttributes(context.obtainStyledAttributes(attrs, R.styleable.WheelProgressBar))
    }

    private fun parseAttributes(a: TypedArray) {
        _barWidth = a.getDimension(R.styleable.WheelProgressBar_pwBarWidth, _barWidth.toFloat()).toInt()
        _rimWidth = a.getDimension(R.styleable.WheelProgressBar_pwRimWidth, _rimWidth.toFloat()).toInt()
        _spinSpeed = a.getDimension(R.styleable.WheelProgressBar_pwSpinSpeed, _spinSpeed)
        _barLength = a.getDimension(R.styleable.WheelProgressBar_pwBarLength, _barLength.toFloat()).toInt()
        _delayMillis = a.getInteger(R.styleable.WheelProgressBar_pwDelayMillis, _delayMillis)
        if (_delayMillis < 0) {
            _delayMillis = 10
        }
        if (a.hasValue(R.styleable.WheelProgressBar_pwText)) {
            setText(a.getString(R.styleable.WheelProgressBar_pwText))
        }
        _barColor = a.getColor(R.styleable.WheelProgressBar_pwBarColor, _barColor)
        _textColor = a.getColor(R.styleable.WheelProgressBar_pwTextColor, _textColor)
        _rimColor = a.getColor(R.styleable.WheelProgressBar_pwRimColor, _rimColor)
        _circleColor = a.getColor(R.styleable.WheelProgressBar_pwCircleColor, _circleColor)
        _contourColor = a.getColor(R.styleable.WheelProgressBar_pwContourColor, _contourColor)
        _textSize = a.getDimension(R.styleable.WheelProgressBar_pwTextSize, _textSize.toFloat()).toInt()
        _contourSize = a.getDimension(R.styleable.WheelProgressBar_pwContourSize, _contourSize)
        a.recycle()
    }

    fun setText(text: String?) {
        _text = text
        _splitText = _text?.split("\n")
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec)
        var size: Int
        val width = measuredWidth
        val height = measuredHeight
        val widthWithoutPadding = width - paddingLeft - paddingRight
        val heightWithoutPadding = height - paddingTop - paddingBottom
        val heightMode = MeasureSpec.getMode(heightMeasureSpec)
        val widthMode = MeasureSpec.getMode(widthMeasureSpec)
        if (heightMode != MeasureSpec.UNSPECIFIED && widthMode != MeasureSpec.UNSPECIFIED) {
            if (widthWithoutPadding > heightWithoutPadding) {
                size = heightWithoutPadding
            } else {
                size = widthWithoutPadding
            }
        } else {
            size = Math.max(heightWithoutPadding, widthWithoutPadding)
        }
        setMeasuredDimension(size + paddingLeft + paddingRight, size + paddingTop + paddingBottom)
    }

    override fun onSizeChanged(w: Int, h: Int, oldw: Int, oldh: Int) {
        super.onSizeChanged(w, h, oldw, oldh)
        _layoutWidth = w
        _layoutHeight = h
        setupBounds()
        setupPaints()
        invalidate()
    }

    private fun setupPaints() {
        _barPaint.color = _barColor
        _barPaint.isAntiAlias = true
        _barPaint.style = Paint.Style.STROKE
        _barPaint.strokeWidth = _barWidth.toFloat()
        _rimPaint.color = _rimColor
        _rimPaint.isAntiAlias = true
        _rimPaint.style = Paint.Style.STROKE
        _rimPaint.strokeWidth = _rimWidth.toFloat()
        _circlePaint.color = _circleColor
        _circlePaint.isAntiAlias = true
        _circlePaint.style = Paint.Style.FILL
        _textPaint.color = _textColor
        _textPaint.style = Paint.Style.FILL
        _textPaint.isAntiAlias = true
        _textPaint.textSize = _textSize.toFloat()
        _contourPaint.color = _contourColor
        _contourPaint.isAntiAlias = true
        _contourPaint.style = Paint.Style.STROKE
        _contourPaint.strokeWidth = _contourSize
    }

    private fun setupBounds() {
        val minValue = Math.min(_layoutWidth, _layoutHeight)
        val xOffset = _layoutWidth - minValue
        val yOffset = _layoutHeight - minValue
        _paddingTop = paddingTop + (yOffset / 2)
        _paddingBottom = paddingBottom + (yOffset / 2)
        _paddingLeft = paddingLeft + (xOffset / 2)
        _paddingRight = paddingRight + (xOffset / 2)
        _innerCircleBounds = RectF(_paddingLeft + (1.5f * _barWidth), _paddingTop + (1.5f * _barWidth), width - _paddingRight - (1.5f * _barWidth), height - _paddingBottom - (1.5f * _barWidth))
        _circleBounds = RectF((_paddingLeft + _barWidth).toFloat(), (_paddingTop + _barWidth).toFloat(), (width - _paddingRight - _barWidth).toFloat(), (height - _paddingBottom - _barWidth).toFloat())
        _circleInnerContour = RectF(_circleBounds.left + (_rimWidth / 2.0f) + (_contourSize / 2.0f), _circleBounds.top + (_rimWidth / 2.0f) + (_contourSize / 2.0f), _circleBounds.right - (_rimWidth / 2.0f) - (_contourSize / 2.0f), _circleBounds.bottom - (_rimWidth / 2.0f) - (_contourSize / 2.0f))
        _circleOuterContour = RectF(_circleBounds.left - (_rimWidth / 2.0f) - (_contourSize / 2.0f), _circleBounds.top - (_rimWidth / 2.0f) - (_contourSize / 2.0f), _circleBounds.right + (_rimWidth / 2.0f) + (_contourSize / 2.0f), _circleBounds.bottom + (_rimWidth / 2.0f) + (_contourSize / 2.0f))
        _fullRadius = (width - _paddingRight - _barWidth) / 2
        _circleRadius = (_fullRadius - _barWidth) + 1
    }

    override fun onDraw(canvas: Canvas?) {
        super.onDraw(canvas)
        canvas?.drawArc(_innerCircleBounds, 360.0f, 360.0f, false, _circlePaint)
        canvas?.drawArc(_circleBounds, 360.0f, 360.0f, false, _rimPaint)
        canvas?.drawArc(_circleOuterContour, 360.0f, 360.0f, false, _contourPaint)
        if (_isSpinning) {
            canvas?.drawArc(_circleBounds, _progress - 90, _barLength.toFloat(), false, _barPaint)
        } else {
            canvas?.drawArc(_circleBounds, -90.0f, _progress, false, _barPaint)
        }
        val textHeight = _textPaint.descent() - _textPaint.ascent()
        val verticalTextOffset = (textHeight / 2) - _textPaint.descent()
        for (line in _splitText!!) {
            val horizontalTextOffset = _textPaint.measureText(line) / 2
            canvas?.drawText(line, width / 2 - horizontalTextOffset, height / 2 + verticalTextOffset, _textPaint)
        }
        if (_isSpinning) {
            scheduleRedraw()
        }
    }

    private fun scheduleRedraw() {
        _progress += _spinSpeed
        if (_progress > 360.0f) {
            _progress = 0.0f
        }
        postInvalidateDelayed(_delayMillis.toLong())
    }

    fun isSpinning(): Boolean = _isSpinning

    fun resetCount() {
        _progress = 0.0f
        setText("0%")
        invalidate()
    }

    fun stopSpinning() {
        _isSpinning = false
        _progress = 0.0f
        postInvalidate()
    }

    fun startSpinning() {
        _isSpinning = true
        postInvalidate()
    }

    fun incrementProgress() = incrementProgress(1)

    fun incrementProgress(amount: Int) {
        _isSpinning = false
        _progress += amount
        if (_progress > 360) {
            _progress %= 360
        }
        postInvalidate()
    }

    fun getProgress(): Int = _progress.toInt()

    fun setProgress(i: Int) {
        _isSpinning = false
        _progress = i.toFloat()
        postInvalidate()
    }

    fun getCircleRadius(): Int = _circleRadius

    fun setCircleRadius(circleRadius: Int) {
        _circleRadius = circleRadius
    }

    fun getBarLength(): Int = _barLength

    fun setBarLength(barLength: Int) {
        _barLength = barLength
    }

    fun getBarWidth(): Int = _barWidth

    fun setBarWidth(barWidth: Int) {
        _barWidth = barWidth
        _barPaint.strokeWidth = _barWidth.toFloat()
    }

    fun getTextSize(): Int = _textSize

    fun setTextSize(textSize: Int) {
        _textSize = textSize
        _textPaint.textSize = _textSize.toFloat()
    }

    fun getNPaddingTop(): Int = _paddingTop

    fun setNPaddingTop(paddingTop: Int) {
        _paddingTop = paddingTop
    }

    fun getNPaddingBottom(): Int = _paddingBottom

    fun setNPaddingBottom(paddingBottom: Int) {
        _paddingBottom = paddingBottom
    }

    fun getNPaddingLeft(): Int = _paddingLeft

    fun setNPaddingLeft(paddingLeft: Int) {
        _paddingLeft = paddingLeft
    }

    fun getNPaddingRight(): Int = _paddingRight

    fun setPaddingRight(paddingRight: Int) {
        _paddingRight = paddingRight
    }

    fun getBarColor(): Int = _barColor

    fun setBarColor(barColor: Int) {
        _barColor = barColor
        _barPaint.color = _barColor
    }

    fun getCircleColor(): Int = _circleColor

    fun setCircleColor(circleColor: Int) {
        _circleColor = circleColor
        _circlePaint.color = _circleColor
    }

    fun getRimColor(): Int = _rimColor

    fun setRimColor(rimColor: Int) {
        _rimColor = rimColor
        _rimPaint.color = _rimColor
    }

    fun getRimShader(): Shader? = _rimPaint.shader

    fun setRimShader(shader: Shader?) {
        _rimPaint.shader = shader
    }

    fun getTextColor(): Int = _textColor

    fun setTextColor(textColor: Int) {
        _textColor = textColor
        _textPaint.color = _textColor
    }

    fun getSpinSpeed(): Float = _spinSpeed

    fun setSpinSpeed(spinSpeed: Float) {
        _spinSpeed = spinSpeed
    }

    fun getRimWidth(): Int = _rimWidth

    fun setRimWidth(rimWidth: Int) {
        _rimWidth = rimWidth
        _rimPaint.strokeWidth = _rimWidth.toFloat()
    }

    fun getDelayMillis(): Int = _delayMillis

    fun setDelayMillis(delayMillis: Int) {
        _delayMillis = delayMillis
    }

    fun getContourColor(): Int = _contourColor

    fun setContourColor(contourColor: Int) {
        _contourColor = contourColor
        _contourPaint.color = _contourColor
    }

    fun getContourSize(): Float = _contourSize

    fun setContourSize(contourSize: Float) {
        _contourSize = contourSize
        _contourPaint.strokeWidth = _contourSize
    }

}