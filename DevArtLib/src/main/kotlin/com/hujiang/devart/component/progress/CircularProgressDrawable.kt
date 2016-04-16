package com.hujiang.devart.component.progress

import android.content.Context
import android.graphics.*
import android.graphics.drawable.Animatable
import android.graphics.drawable.Drawable
import android.os.PowerManager
import android.support.v4.view.animation.FastOutSlowInInterpolator
import android.view.animation.LinearInterpolator
import android.view.animation.Interpolator
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/15/16.
 */
class CircularProgressDrawable: Drawable, Animatable {

    interface OnEndListener {
        fun onEnd(drawable: CircularProgressDrawable?)
    }

    companion object {
        val STYLE_NORMAL = 0
        val STYLE_ROUNDED = 1
    }

    private val _bounds = RectF()
    private var _powerManager: PowerManager? = null
    private var _options: Options? = null
    private var _paint: Paint? = null
    private var _running = false
    private var _pbDelegate: ProgressBarDelegate? = null

    constructor(powerManager: PowerManager?, options: Options?) {
        _options = options
        _paint = Paint()
        _paint?.isAntiAlias = true
        _paint?.style = Paint.Style.STROKE
        _paint?.strokeWidth = options!!.borderWidth
        _paint?.strokeCap = if (options.style == STYLE_ROUNDED) Paint.Cap.ROUND else Paint.Cap.BUTT
        _paint?.color = options.colors!![0]
        _powerManager = powerManager
        initDelegate()
    }

    private fun initDelegate() {
        val powerSaveMode = ProgressBarUtils.isPowerSaveModeEnabled(_powerManager)
        if (powerSaveMode) {
            if (_pbDelegate == null || _pbDelegate !is PowerSaveModeDelegate) {
                _pbDelegate?.stop()
                _pbDelegate = PowerSaveModeDelegate(this)
            }
        } else {
            if (_pbDelegate == null || _pbDelegate is PowerSaveModeDelegate) {
                _pbDelegate?.stop()
                _pbDelegate = DefaultDelegate(this, _options!!)
            }
        }
    }

    override fun draw(canvas: Canvas?) {
        if (isRunning) {
            _pbDelegate?.draw(canvas, _paint)
        }
    }

    override fun setAlpha(alpha: Int) {
        _paint?.alpha = alpha
    }

    override fun setColorFilter(cf: ColorFilter?) {
        _paint?.colorFilter = cf
    }

    override fun getOpacity(): Int = PixelFormat.TRANSLUCENT

    override fun onBoundsChange(bounds: Rect?) {
        super.onBoundsChange(bounds)
        val border = _options!!.borderWidth
        _bounds.left = bounds!!.left + border / 2.0f + 0.5f
        _bounds.right = bounds.right - border / 2.0f - 0.5f
        _bounds.top = bounds.top + border / 2.0f + 0.5f
        _bounds.bottom = bounds.bottom - border / 2.0f - 0.5f
    }

    override fun start() {
        initDelegate()
        _pbDelegate?.start()
        _running = true
        invalidateSelf()
    }

    override fun stop() {
        _running = false
        _pbDelegate?.stop()
        invalidateSelf()
    }

    override fun isRunning(): Boolean = _running

    fun invalidate() {
        if (callback == null) {
            stop()
        }
        invalidateSelf()
    }

    fun getCurrentPaint(): Paint? = _paint

    fun getDrawableBounds(): RectF = _bounds

    fun progressiveStop(listener: CircularProgressDrawable.OnEndListener?) {
        _pbDelegate?.progressiveStop(listener)
    }

    fun progressiveStop() = progressiveStop(null)

    class Builder {
        companion object {
            private val DEFAULT_ROTATION_INTERPOLATOR = LinearInterpolator()
            private val DEFAULT_SWEEP_INTERPOLATOR = FastOutSlowInInterpolator()
        }

        private var _sweepInterpolator: Interpolator? = DEFAULT_SWEEP_INTERPOLATOR
        private var _angleInterpolator: Interpolator? = DEFAULT_ROTATION_INTERPOLATOR
        private var _borderWidth = 0.0f
        private var _colors: IntArray? = null
        private var _sweepSpeed = 0.0f
        private var _rotationSpeed = 0.0f
        private var _minSweepAngle = 0
        private var _maxSweepAngle = 0
        private var _style = 0
        private var _powerManager: PowerManager? = null

        constructor(context: Context): this(context, false)

        constructor(context: Context, editMode: Boolean) {
            initValues(context, editMode)
        }

        private fun initValues(context: Context, editMode: Boolean) {
            _borderWidth = context.resources.getDimension(R.dimen.cpbDefaultStrokeWidth)
            _sweepSpeed = 1.0f
            _rotationSpeed = 1.0f
            if (editMode) {
                _colors = intArrayOf(Color.BLUE)
                _minSweepAngle = 20
                _maxSweepAngle = 300
            } else {
                _colors = intArrayOf(context.resources.getColor(R.color.cpbDefaultColor))
                _minSweepAngle = context.resources.getInteger(R.integer.cpbDefaultMinSweepAngle)
                _maxSweepAngle = context.resources.getInteger(R.integer.cpbDefaultMaxSweepAngle)
            }
            _style = CircularProgressDrawable.STYLE_ROUNDED
            _powerManager = ProgressBarUtils.powerManager(context)
        }

        fun color(color: Int): Builder {
            _colors = intArrayOf(color)
            return this
        }

        fun colors(colors: IntArray?): Builder {
            ProgressBarUtils.checkColors(colors)
            _colors = colors
            return this
        }

        fun sweepSpeed(sweepSpeed: Float): Builder {
            ProgressBarUtils.checkSpeed(sweepSpeed)
            _sweepSpeed = sweepSpeed
            return this
        }

        fun rotationSpeed(rotationSpeed: Float): Builder {
            ProgressBarUtils.checkSpeed(rotationSpeed)
            _rotationSpeed = rotationSpeed
            return this
        }

        fun minSweepAngle(minSweepAngle: Int): Builder {
            ProgressBarUtils.checkAngle(minSweepAngle)
            _minSweepAngle = minSweepAngle
            return this
        }

        fun maxSweepAngle(maxSweepAngle: Int): Builder {
            ProgressBarUtils.checkAngle(maxSweepAngle)
            _maxSweepAngle = maxSweepAngle
            return this
        }

        fun strokeWidth(strokeWidth: Float): Builder {
            ProgressBarUtils.checkPositiveOrZero(strokeWidth, "StrokeWidth")
            _borderWidth = strokeWidth
            return this
        }

        fun style(style: Int): Builder {
            _style = style
            return this
        }

        fun sweepInterpolator(interpolator: Interpolator?): Builder {
            ProgressBarUtils.checkNotNull(interpolator, "Sweep interpolator")
            _sweepInterpolator = interpolator
            return this
        }

        fun angleInterpolator(interpolator: Interpolator?): Builder {
            ProgressBarUtils.checkNotNull(interpolator, "Angle interpolator")
            _angleInterpolator = interpolator
            return this
        }

        fun build(): CircularProgressDrawable = CircularProgressDrawable(_powerManager, Options(_angleInterpolator, _sweepInterpolator, _borderWidth, _colors, _sweepSpeed, _rotationSpeed, _minSweepAngle,_maxSweepAngle,_style))
    }


}