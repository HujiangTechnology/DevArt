package com.hujiang.devart.component.cropper

import android.content.Context
import android.graphics.*
import android.graphics.drawable.BitmapDrawable
import android.util.AttributeSet
import android.view.MotionEvent
import android.widget.ImageView
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/26/16.
 */
class CropImageView: ImageView {

    companion object {
        val GUIDELINES_OFF = 0
        val GUIDELINES_ON_TOUCH = 1
        val GUIDELINES_ON = 2
    }

    private var _borderPaint: Paint? = null
    private var _guidelinePaint: Paint? = null
    private var _cornerPaint: Paint? = null
    private var _surroundingAreaOverlayPaint: Paint? = null
    private var _handleRadius = 0.0f
    private var _snapRadius = 0.0f
    private var _cornerThickness = 0.0f
    private var _borderThickness = 0.0f
    private var _cornerLength = 0.0f
    private var _bitmapRect = RectF()
    private var _touchOffset = PointF()
    private var _pressedHandle: Handle? = null
    private var _fixAspectRatio = false
    private var _aspectRatioX = 1
    private var _aspectRatioY = 1
    private var _guidelinesMode = 1

    constructor(context: Context): super(context) {
        init(context, null)
    }
    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        init(context, attrs)
    }
    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        init(context, attrs)
    }

    private fun init(context: Context, attrs: AttributeSet?) {
        val typedArray = context.obtainStyledAttributes(attrs, R.styleable.CropImageView, 0, 0)
        _guidelinesMode = typedArray.getInteger(R.styleable.CropImageView_guidelines, 1)
        _fixAspectRatio = typedArray.getBoolean(R.styleable.CropImageView_fixAspectRatio, false)
        _aspectRatioX = typedArray.getInteger(R.styleable.CropImageView_aspectRatioX, 1)
        _aspectRatioY = typedArray.getInteger(R.styleable.CropImageView_aspectRatioY, 1)
        typedArray.recycle()
        val resources = context.resources
        _borderPaint = PaintUtil.newBorderPaint(resources)
        _guidelinePaint = PaintUtil.newGuidelinePaint(resources)
        _surroundingAreaOverlayPaint = PaintUtil.newSurroundingAreaOverlayPaint(resources)
        _cornerPaint = PaintUtil.newCornerPaint(resources)
        _handleRadius = resources.getDimension(R.dimen.target_radius)
        _snapRadius = resources.getDimension(R.dimen.snap_radius)
        _borderThickness = resources.getDimension(R.dimen.border_thickness)
        _cornerThickness = resources.getDimension(R.dimen.corner_thickness)
        _cornerLength = resources.getDimension(R.dimen.corner_length)
    }

    override fun onLayout(changed: Boolean, left: Int, top: Int, right: Int, bottom: Int) {
        super.onLayout(changed, left, top, right, bottom)
        _bitmapRect = getBitmapRect()
        initCropWindow(_bitmapRect)
    }

    private fun getBitmapRect(): RectF {
        if (drawable == null) {
            return RectF()
        }
        val matrixValues = FloatArray(9)
        imageMatrix.getValues(matrixValues)
        val scaleX = matrixValues[Matrix.MSCALE_X]
        val scaleY = matrixValues[Matrix.MSCALE_Y]
        val transX = matrixValues[Matrix.MTRANS_X]
        val transY = matrixValues[Matrix.MTRANS_Y]
        val drawableIntrinsicWidth = drawable.intrinsicWidth
        val drawableIntrinsicHeight = drawable.intrinsicHeight
        val drawableDisplayWidth = Math.round(drawableIntrinsicWidth * scaleX)
        val drawableDisplayHeight = Math.round(drawableIntrinsicHeight * scaleY)
        val left = Math.max(transX, 0.0f)
        val top = Math.max(transY, 0.0f)
        val right = Math.min(left + drawableDisplayWidth, width.toFloat())
        val bottom = Math.min(top + drawableDisplayHeight, height.toFloat())
        return RectF(left, top, right, bottom)
    }

    private fun initCropWindow(bitmapRect: RectF) {
        if (_fixAspectRatio) {
            initCropWindowWithFixedAspectRatio(bitmapRect)
        } else {
            val horizontalPadding = 0.1f * bitmapRect.width()
            val verticalPadding = 0.1f * bitmapRect.height()
            Edge.LEFT.setCoordinate(bitmapRect.left + horizontalPadding)
            Edge.TOP.setCoordinate(bitmapRect.top + verticalPadding)
            Edge.RIGHT.setCoordinate(bitmapRect.right - horizontalPadding)
            Edge.BOTTOM.setCoordinate(bitmapRect.bottom - verticalPadding)
        }
    }

    private fun initCropWindowWithFixedAspectRatio(bitmapRect: RectF) {
        if (AspectRatioUtil.calculateAspectRatio(bitmapRect) > getTargetAspectRatio()) {
            val cropWidth = AspectRatioUtil.calculateWidth(bitmapRect.height(), getTargetAspectRatio())
            Edge.LEFT.setCoordinate(bitmapRect.centerX() - cropWidth / 2f)
            Edge.TOP.setCoordinate(bitmapRect.top)
            Edge.RIGHT.setCoordinate(bitmapRect.centerX() + cropWidth / 2f)
            Edge.BOTTOM.setCoordinate(bitmapRect.bottom)
        } else {
            val cropHeight = AspectRatioUtil.calculateHeight(bitmapRect.width(), getTargetAspectRatio())
            Edge.LEFT.setCoordinate(bitmapRect.left)
            Edge.TOP.setCoordinate(bitmapRect.centerY() - cropHeight / 2f)
            Edge.RIGHT.setCoordinate(bitmapRect.right)
            Edge.BOTTOM.setCoordinate(bitmapRect.centerY() + cropHeight / 2f)
        }
    }

    private fun getTargetAspectRatio(): Float = _aspectRatioX * 1.0f / _aspectRatioY

    override fun onDraw(canvas: Canvas?) {
        super.onDraw(canvas)
        drawDarkenedSurroundingArea(canvas!!)
        drawGuidelines(canvas)
        drawBorder(canvas)
        drawCorners(canvas)
    }

    private fun drawCorners(canvas: Canvas) {
        val left = Edge.LEFT.getCoordinate()
        val top = Edge.TOP.getCoordinate()
        val right = Edge.RIGHT.getCoordinate()
        val bottom = Edge.BOTTOM.getCoordinate()
        val lateralOffset = (_cornerThickness - _borderThickness) / 2.0f
        val startOffset = _cornerThickness - (_borderThickness / 2.0f)
        canvas.drawLine(left - lateralOffset, top - startOffset, left - lateralOffset, top + _cornerLength, _cornerPaint)
        canvas.drawLine(left - startOffset, top - lateralOffset, left + _cornerLength, top - lateralOffset, _cornerPaint)
        canvas.drawLine(right + lateralOffset, top - startOffset, right + lateralOffset, top + _cornerLength, _cornerPaint)
        canvas.drawLine(right + startOffset, top - lateralOffset, right - _cornerLength, top - lateralOffset, _cornerPaint)
        canvas.drawLine(left - lateralOffset, bottom + startOffset, left - lateralOffset, bottom - _cornerLength, _cornerPaint)
        canvas.drawLine(left - startOffset, bottom + lateralOffset, left + _cornerLength, bottom + lateralOffset, _cornerPaint)
        canvas.drawLine(right + lateralOffset, bottom + startOffset, right + lateralOffset, bottom - _cornerLength, _cornerPaint)
        canvas.drawLine(right + startOffset, bottom + lateralOffset, right - _cornerLength, bottom + lateralOffset, _cornerPaint)
    }

    private fun drawBorder(canvas: Canvas) {
        canvas.drawRect(Edge.LEFT.getCoordinate(), Edge.TOP.getCoordinate(), Edge.RIGHT.getCoordinate(), Edge.BOTTOM.getCoordinate(), _borderPaint)
    }

    private fun drawDarkenedSurroundingArea(canvas: Canvas) {
        val bitmapRect = _bitmapRect
        val left = Edge.LEFT.getCoordinate()
        val top = Edge.TOP.getCoordinate()
        val right = Edge.RIGHT.getCoordinate()
        val bottom = Edge.BOTTOM.getCoordinate()
        canvas.drawRect(bitmapRect.left, bitmapRect.top, bitmapRect.right, top, _surroundingAreaOverlayPaint)
        canvas.drawRect(bitmapRect.left, bottom, bitmapRect.right, bitmapRect.bottom, _surroundingAreaOverlayPaint)
        canvas.drawRect(bitmapRect.left, top, left, bottom, _surroundingAreaOverlayPaint)
        canvas.drawRect(right, top, bitmapRect.right, bottom, _surroundingAreaOverlayPaint)
    }

    private fun shouldGuidelinesBeShown(): Boolean = _guidelinesMode == GUIDELINES_ON || (_guidelinesMode == GUIDELINES_ON_TOUCH) && (_pressedHandle != null)

    private fun drawGuidelines(canvas: Canvas) {
        if (!shouldGuidelinesBeShown()) {
            return
        }
        val left = Edge.LEFT.getCoordinate()
        val top = Edge.TOP.getCoordinate()
        val right = Edge.RIGHT.getCoordinate()
        val bottom = Edge.BOTTOM.getCoordinate()
        val oneThirdCropWidth = Edge.getWidth() / 3
        val x1 = left + oneThirdCropWidth
        canvas.drawLine(x1, top, x1, bottom, _guidelinePaint)
        val x2 = right - oneThirdCropWidth
        canvas.drawLine(x2, top, x2, bottom, _guidelinePaint)
        val oneThirdCropHeight = Edge.getHeight() / 3
        val y1 = top + oneThirdCropHeight
        canvas.drawLine(left, y1, right, y1, _guidelinePaint)
        val y2 = bottom - oneThirdCropHeight
        canvas.drawLine(left, y2, right, y2, _guidelinePaint)
    }

    override fun onTouchEvent(event: MotionEvent?): Boolean {
        if (!isEnabled) {
            return false
        }
        when (event!!.action) {
            MotionEvent.ACTION_DOWN -> {
                onActionDown(event.x, event.y)
                return true
            }
            MotionEvent.ACTION_UP, MotionEvent.ACTION_CANCEL -> {
                parent.requestDisallowInterceptTouchEvent(false)
                onActionUp()
                return true
            }
            MotionEvent.ACTION_MOVE -> {
                onActionMove(event.x, event.y)
                parent.requestDisallowInterceptTouchEvent(true)
                return true
            }
            else -> return false
        }
    }

    private fun onActionDown(x: Float, y: Float) {
        val left = Edge.LEFT.getCoordinate()
        val top = Edge.TOP.getCoordinate()
        val right = Edge.RIGHT.getCoordinate()
        val bottom = Edge.BOTTOM.getCoordinate()
        _pressedHandle = HandleUtil.getPressedHandle(x, y, left, top, right, bottom, _handleRadius)
        if (_pressedHandle != null) {
            HandleUtil.getOffset(_pressedHandle, x, y, left, top, right, bottom, _touchOffset)
            invalidate()
        }
    }

    private fun onActionUp() {
        if (_pressedHandle != null) {
            _pressedHandle = null
            invalidate()
        }
    }

    private fun onActionMove(x: Float, y: Float) {
        if (_pressedHandle == null) {
            return
        }
        var nx = x + _touchOffset.x
        var ny = y + _touchOffset.y
        if (_fixAspectRatio) {
            _pressedHandle?.updateCropWindow(nx, ny, getTargetAspectRatio(), _bitmapRect, _snapRadius)
        } else {
            _pressedHandle?.updateCropWindow(nx, ny, _bitmapRect, _snapRadius)
        }
        invalidate()
    }

    fun getCroppedImage(): Bitmap? {
        if (drawable == null || drawable !is BitmapDrawable) {
            return null
        }
        val matrixValues = FloatArray(9)
        imageMatrix.getValues(matrixValues)
        val scaleX = matrixValues[Matrix.MSCALE_X]
        val scaleY = matrixValues[Matrix.MSCALE_Y]
        val transX = matrixValues[Matrix.MTRANS_X]
        val transY = matrixValues[Matrix.MTRANS_Y]
        val bitmapLeft = if (transX < 0) Math.abs(transX) else 0.0f
        val bitmapTop = if (transY < 0) Math.abs(transY) else 0.0f
        val originalBitmap = (drawable as BitmapDrawable).bitmap
        val cropX = (bitmapLeft + Edge.LEFT.getCoordinate()) / scaleX
        val cropY = (bitmapTop + Edge.TOP.getCoordinate()) / scaleY
        val cropWidth = Math.min(Edge.getWidth() / scaleX, originalBitmap.width - cropX)
        val cropHeight = Math.min(Edge.getHeight() / scaleY, originalBitmap.height - cropY)
        return Bitmap.createBitmap(originalBitmap, cropX.toInt(), cropY.toInt(), cropWidth.toInt(), cropHeight.toInt())
    }

    fun setGuidelines(guidelinesMode: Int) {
        _guidelinesMode = guidelinesMode
        invalidate()
    }

    fun setFixedAspectRatio(fixAspectRatio: Boolean) {
        _fixAspectRatio = fixAspectRatio
        requestLayout()
    }

    fun setAspectRatio(aspectRatioX: Int, aspectRatioY: Int) {
        if (aspectRatioX <= 0 || aspectRatioY <= 0) {
            throw IllegalArgumentException("Cannot set aspect ratio value to a number less than or equal to 0.")
        }
        _aspectRatioX = aspectRatioX
        _aspectRatioY = aspectRatioY
        if (_fixAspectRatio) {
            requestLayout()
        }
    }

}