package com.hujiang.devart.component.touchimage

import android.content.Context
import android.graphics.Bitmap
import android.graphics.Canvas
import android.graphics.Matrix
import android.graphics.PointF
import android.graphics.drawable.BitmapDrawable
import android.util.AttributeSet
import android.util.FloatMath
import android.view.MotionEvent
import android.widget.ImageView

/**
 * Created by rarnu on 3/30/16.
 */
class TouchImageView : ImageView {

    companion object {
        private val NONE = 0
        private val DRAG = 1
        private val ZOOM = 2

        private val DOUBLE_CLICK_TIME_SPACE = 300
        private val DOUBLE_POINT_DISTANCE = 10
        private var MAX_SCALE = 3.0f

        private val DRAG_LEFT = 0
        private val DRAG_RIGHT = 1
        private val DRAG_TOP = 2
        private val DRAG_DOWN = 3

        private fun spacing(event: MotionEvent?): Float {
            var x = event!!.getX(0) - event.getX(1)
            if (x < 0) {
                x = -x
            }
            var y = event.getY(0) - event.getY(1)
            if (y < 0) {
                y = -y
            }
            return FloatMath.sqrt(x * x + y * y)
        }

        private fun midPoint(point: PointF?, event: MotionEvent?) {
            val x = event!!.getX(0) + event.getX(1)
            val y = event.getY(0) + event.getY(1)
            point?.set(x / 2, y / 2)
        }

        private fun distance(point2: MotionEvent?, point1: PointF?): Float {
            var x = point1!!.x -point2!!.x
            if (x < 0) {
                x = -x
            }
            var y = point1.y -point2.y
            if (y < 0) {
                y = -y
            }
            return FloatMath.sqrt(x * x + y * y)
        }

        private fun fspacing(event: MotionEvent?): Float {
            var x = event!!.getX(0) - event.getX(1)
            if (x < 0) {
                x = -x
            }
            var y = event.getY(0) - event.getY(1)
            if (y < 0) {
                y = -y
            }
            return FloatMath.sqrt(x * x + y * y)
        }

    }

    private var _down = PointF()
    private var _mid = PointF()
    private var _oldDist = 1.0f
    private var _matrix = Matrix()
    private var _preMatrix = Matrix()
    private var _savedMatrix = Matrix()
    private var _mode = NONE
    private var _isBig = false
    private var _widthScreen = 0
    private var _heightScreen = 0
    private var _touchImgWidth = 0
    private var _touchImgHeight = 0
    private var _defaultScale = 1.0f
    private var _lastClickTime = 0L
    private var _touchImg: Bitmap? = null

    constructor(context: Context) : super(context)

    constructor(context: Context, attrs: AttributeSet?) : super(context, attrs)

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int) : super(context, attrs, defStyle)

    fun initImageView(screenWidth: Int, screenHeight: Int) {
        _widthScreen = screenWidth
        _heightScreen = screenHeight
        _touchImg = (drawable as BitmapDrawable).bitmap
        _touchImgWidth = _touchImg!!.width
        _touchImgHeight = _touchImg!!.height
        val scaleX = _widthScreen * 1.0f / _touchImgWidth
        val scaleY = _heightScreen * 1.0f / _touchImgHeight
        _defaultScale = if (scaleX < scaleY) scaleX else scaleY

        val subX = (_widthScreen - _touchImgWidth * _defaultScale) / 2
        val subY = (_heightScreen - _touchImgHeight * _defaultScale) / 2
        scaleType = ScaleType.MATRIX
        _preMatrix.reset()
        _preMatrix.postScale(_defaultScale, _defaultScale)
        _preMatrix.postTranslate(subX, subY)
        _matrix.set(_preMatrix)
        invalidate()
    }

    override fun onDraw(canvas: Canvas?) {
        if (_touchImg != null) {
            canvas?.save()
            canvas?.drawBitmap(_touchImg, _matrix, null)
            canvas?.restore()
        }
    }

    override fun onTouchEvent(event: MotionEvent?): Boolean {
        when (event!!.action and MotionEvent.ACTION_MASK) {
            MotionEvent.ACTION_DOWN -> {
                _mode = DRAG
                _down.x = event.x
                _down.y = event.y
                _savedMatrix.set(_matrix)
                if (event.eventTime - _lastClickTime < DOUBLE_CLICK_TIME_SPACE) {
                    changeSize(event.x, event.y)
                }
                _lastClickTime = event.eventTime
            }
            MotionEvent.ACTION_POINTER_DOWN -> {
                _oldDist = spacing(event);
                if (_oldDist > DOUBLE_POINT_DISTANCE) {
                    _mode = ZOOM;
                    _savedMatrix.set(_matrix)
                    midPoint(_mid, event)
                }
            }

            MotionEvent.ACTION_MOVE -> {
                if (_mode == ZOOM) {
                    val newDist = spacing(event)
                    val scale = newDist / _oldDist
                    if (scale > 1.01 || scale < 0.99) {
                        _preMatrix.set(_savedMatrix);
                        _preMatrix.postScale(scale, scale, _mid.x, _mid.y)
                        if (canZoom()) {
                            _matrix.set(_preMatrix)
                            invalidate()
                        }
                    }
                } else if (_mode == DRAG) {
                    if (1.0f < distance(event, _down)) {
                        _preMatrix.set(_savedMatrix)
                        _preMatrix.postTranslate(event.x - _down.x, 0.0f)
                        if (event.x > _down.x) {
                            if (canDrag(DRAG_RIGHT)) {
                                _savedMatrix.set(_preMatrix)
                            } else {
                                _preMatrix.set(_savedMatrix)
                            }
                        } else {
                            if (canDrag(DRAG_LEFT)) {
                                _savedMatrix.set(_preMatrix)
                            } else {
                                _preMatrix.set(_savedMatrix)
                            }
                        }
                        _preMatrix.postTranslate(0.0f, event.y - _down.y)
                        if (event.y > _down.y) {
                            if (canDrag(DRAG_DOWN)) {
                                _savedMatrix.set(_preMatrix)
                            } else {
                                _preMatrix.set(_savedMatrix)
                            }
                        } else {
                            if (canDrag(DRAG_TOP)) {
                                _savedMatrix.set(_preMatrix)
                            } else {
                                _preMatrix.set(_savedMatrix)
                            }
                        }
                        _matrix.set(_preMatrix)
                        invalidate()
                        _down.x = event.x
                        _down.y = event.y
                        _savedMatrix.set(_matrix)
                    }
                }
            }
            MotionEvent.ACTION_UP -> {
                _mode = NONE
                springback()
            }
            MotionEvent.ACTION_POINTER_UP -> _mode = NONE
        }
        return true
    }

    private fun springback() {
        _preMatrix.set(_matrix)
        val x = FloatArray(4)
        val y = FloatArray(4)
        getFourPoint(x, y)
        if (x[1] - x[0] > _widthScreen) {
            if (x[0] > 0) {
                _preMatrix.postTranslate(-x[0], 0.0f)
                _matrix.set(_preMatrix)
                invalidate()
            } else if (x[1] < _widthScreen) {
                _preMatrix.postTranslate(_widthScreen - x[1], 0.0f)
                _matrix.set(_preMatrix)
                invalidate()
            }
        } else if (x[1] - x[0] < _widthScreen - 1.0f) {
            _preMatrix.postTranslate((_widthScreen - (x[1] - x[0])) / 2 - x[0], 0.0f)
            _matrix.set(_preMatrix)
            invalidate()
        }
        if (y[2] - y[0] > _heightScreen) {
            if (y[0] > 0) {
                _preMatrix.postTranslate(0.0f, -y[0])
                _matrix.set(_preMatrix)
                invalidate()
            } else if (y[2] < _heightScreen) {
                _preMatrix.postTranslate(0.0f, _heightScreen - y[2])
                _matrix.set(_preMatrix)
                invalidate()
            }
        } else if (y[2] - y[0] < _heightScreen - 1.0f) {
            _preMatrix.postTranslate(0.0f, (_heightScreen - (y[2] - y[0])) / 2 - y[0])
            _matrix.set(_preMatrix)
            invalidate()
        }
    }

    private fun canDrag(direction: Int): Boolean {
        val x = FloatArray(4)
        val y = FloatArray(4)
        getFourPoint(x, y)
        if ((x[0] > 0 || x[2] > 0 || x[1] < _widthScreen || x[3] < _widthScreen) && (y[0] > 0 || y[1] > 0 || y[2] < _heightScreen || y[3] < _heightScreen)) {
            return false
        }
        if (direction == DRAG_LEFT) {
            if (x[1] < _widthScreen || x[3] < _widthScreen) {
                return false
            }
        } else if (direction == DRAG_RIGHT) {
            if (x[0] > 0 || x[2] > 0) {
                return false
            }
        } else if (direction == DRAG_TOP) {
            if (y[2] < _heightScreen || y[3] < _heightScreen) {
                return false
            }
        } else if (direction == DRAG_DOWN) {
            if (y[0] > 0 || y[1] > 0) {
                return false
            }
        } else {
            return false
        }
        return true
    }

    private fun getFourPoint(x: FloatArray?, y: FloatArray?) {
        val f = FloatArray(9)
        _preMatrix.getValues(f)
        x!![0] = f[Matrix.MSCALE_X] * 0 + f[Matrix.MSKEW_X] * 0+ f[Matrix.MTRANS_X]
        y!![0] = f[Matrix.MSKEW_Y] * 0 + f[Matrix.MSCALE_Y] * 0+ f[Matrix.MTRANS_Y]
        x[1] = f[Matrix.MSCALE_X] * _touchImg!!.width + f[Matrix.MSKEW_X] * 0+ f[Matrix.MTRANS_X]
        y[1] = f[Matrix.MSKEW_Y] * _touchImg!!.width + f[Matrix.MSCALE_Y] * 0+ f[Matrix.MTRANS_Y]
        x[2] = f[Matrix.MSCALE_X] * 0 + f[Matrix.MSKEW_X] * _touchImg!!.height + f[Matrix.MTRANS_X]
        y[2] = f[Matrix.MSKEW_Y] * 0 + f[Matrix.MSCALE_Y] * _touchImg!!.height + f[Matrix.MTRANS_Y]
        x[3] = f[Matrix.MSCALE_X] * _touchImg!!.width + f[Matrix.MSKEW_X]* _touchImg!!.height + f[Matrix.MTRANS_X]
        y[3] = f[Matrix.MSKEW_Y] * _touchImg!!.width + f[Matrix.MSCALE_Y]* _touchImg!!.height + f[Matrix.MTRANS_Y]
    }

    private fun canZoom(): Boolean {
        val x = FloatArray(4)
        val y = FloatArray(4)
        getFourPoint(x, y)
        val width = Math.sqrt(((x[0] - x[1]) * (x[0] - x[1]) + (y[0] - y[1]) * (y[0] - y[1])).toDouble())
        val height = Math.sqrt(((x[0] - x[2]) * (x[0] - x[2]) + (y[0] - y[2]) * (y[0] - y[2])).toDouble())
        if (width < _touchImgWidth * _defaultScale - 1 || width > _touchImgWidth * MAX_SCALE + 1) {
            return false
        }
        if (width < _widthScreen && height < _heightScreen) {
            return false
        }
        return true
    }

    private fun changeSize(x: Float, y: Float) =
            if (_isBig) {
                val subX = (_widthScreen - _touchImgWidth * _defaultScale) / 2
                val subY = (_heightScreen - _touchImgHeight * _defaultScale) / 2
                _preMatrix.reset()
                _preMatrix.postScale(_defaultScale, _defaultScale)
                _preMatrix.postTranslate(subX, subY)
                _matrix.set(_preMatrix)
                invalidate()
                _isBig = false
            } else {
                val transX = (_widthScreen - _touchImgWidth * MAX_SCALE) / 2
                val transY = (_heightScreen - _touchImgHeight * MAX_SCALE) / 2
                _preMatrix.reset()
                _preMatrix.postScale(MAX_SCALE, MAX_SCALE)
                _preMatrix.postTranslate(transX, transY)
                _matrix.set(_preMatrix)
                invalidate()
                _isBig = true
            }

}