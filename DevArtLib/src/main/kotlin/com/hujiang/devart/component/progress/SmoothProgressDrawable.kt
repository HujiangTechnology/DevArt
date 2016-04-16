package com.hujiang.devart.component.progress

import android.content.Context
import android.graphics.*
import android.graphics.drawable.Animatable
import android.graphics.drawable.Drawable
import android.os.SystemClock
import android.view.animation.AccelerateInterpolator
import android.view.animation.Interpolator
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/15/16.
 */
class SmoothProgressDrawable: Drawable, Animatable {

    interface Callbacks {
        fun onStop()
        fun onStart()
    }

    companion object {
        private val FRAME_DURATION = (1000 / 60).toLong()
        private val OFFSET_PER_FRAME = 0.01f
    }

    private val _backgroundRect = Rect()
    private var _callbacks: Callbacks? = null
    private var _interpolator: Interpolator? = null
    private var _bounds: Rect? = null
    private var _paint: Paint? = null
    private var _colors: IntArray? = null
    private var _colorsIndex = 0
    private var _running = false
    private var _currentOffset = 0.0f
    private var _finishingOffset = 0.0f
    private var _separatorLength = 0
    private var _sectionsCount = 0
    private var _speed = 0.0f
    private var _progressiveStartSpeed = 0.0f
    private var _progressiveStopSpeed = 0.0f
    private var _reversed = false
    private var _newTurn = false
    private var _mirrorMode = false
    private var _maxOffset = 0.0f
    private var _finishing = false
    private var _progressiveStartActivated = false
    private var _startSection = 0
    private var _currentSections = 0
    private var _strokeWidth = 0.0f
    private var _backgroundDrawable: Drawable? = null
    private var _useGradients = false
    private var _linearGradientColors: IntArray? = null
    private var _linearGradientPositions: FloatArray? = null

    private fun getUpdater(): Runnable = _updater
    private val _updater = Runnable {
        if (isFinishing()) {
            _finishingOffset += (OFFSET_PER_FRAME * _progressiveStopSpeed)
            _currentOffset += (OFFSET_PER_FRAME * _progressiveStopSpeed)
            if (_finishingOffset >= 1.0f) {
                stop()
            }
        } else if (isStarting()) {
            _currentOffset += (OFFSET_PER_FRAME * _progressiveStartSpeed)
        } else {
            _currentOffset += (OFFSET_PER_FRAME * _speed)
        }
        if (_currentOffset >= _maxOffset) {
            _newTurn = true;
            _currentOffset -= _maxOffset
        }
        if (isRunning) {
            scheduleSelf(getUpdater(), SystemClock.uptimeMillis() + FRAME_DURATION)
        }
        invalidateSelf()
    }

    constructor(interpolator: Interpolator?, sectionsCount: Int, separatorLength: Int, colors: IntArray?, strokeWidth: Float, speed: Float, progressiveStartSpeed: Float, progressiveStopSpeed: Float, reversed: Boolean, mirrorMode: Boolean, callbacks: Callbacks?, progressiveStartActivated: Boolean, backgroundDrawable: Drawable?, useGradients: Boolean) {
        _running = false
        _interpolator = interpolator
        _sectionsCount = sectionsCount
        _startSection = 0
        _currentSections = _sectionsCount
        _separatorLength = separatorLength
        _speed = speed
        _progressiveStartSpeed = progressiveStartSpeed
        _progressiveStopSpeed = progressiveStopSpeed
        _reversed = reversed
        _colors = colors
        _colorsIndex = 0
        _mirrorMode = mirrorMode
        _finishing = false
        _backgroundDrawable = backgroundDrawable
        _strokeWidth = strokeWidth
        _maxOffset = 1f / _sectionsCount
        _paint = Paint()
        _paint?.strokeWidth = strokeWidth
        _paint?.style = Paint.Style.STROKE
        _paint?.isDither = false
        _paint?.isAntiAlias = false
        _progressiveStartActivated = progressiveStartActivated
        _callbacks = callbacks
        _useGradients = useGradients
        refreshLinearGradientOptions()
    }

    protected fun refreshLinearGradientOptions() {
        if (_useGradients) {
            _linearGradientColors = IntArray(_sectionsCount + 2)
            _linearGradientPositions = FloatArray(_sectionsCount + 2)
        } else {
            _paint?.shader = null
            _linearGradientColors = null
            _linearGradientPositions = null
        }
    }

    override fun draw(canvas: Canvas?) {
        _bounds = bounds
        canvas?.clipRect(_bounds)
        if (_newTurn) {
            _colorsIndex = decrementColor(_colorsIndex)
            _newTurn = false
            if (isFinishing()) {
                _startSection++
                if (_startSection > _sectionsCount) {
                    stop()
                    return
                }
            }
            if (_currentSections < _sectionsCount) {
                _currentSections++
            }
        }
        if (_useGradients) {
            drawGradient(canvas)
        }
        drawStrokes(canvas)
    }

    private fun drawStrokes(canvas: Canvas?) {
        if (_reversed) {
            canvas?.translate(_bounds!!.width().toFloat(), 0.0f)
            canvas?.scale(-1.0f, 1.0f)
        }
        var prevValue = 0.0f
        var boundsWidth = _bounds!!.width()
        if (_mirrorMode) {
            boundsWidth /= 2
        }
        val width = boundsWidth + _separatorLength + _sectionsCount
        val centerY = _bounds!!.centerY()
        val xSectionWidth = 1.0f / _sectionsCount
        var startX: Float
        var endX: Float
        var firstX = 0.0f
        var lastX = 0.0f
        var prev: Float
        var end: Float
        var spaceLength: Float
        var xOffset: Float
        var ratioSectionWidth: Float
        var sectionWidth: Float
        var drawLength: Float
        var currentIndexColor = _colorsIndex

        if (_startSection == _currentSections && _currentSections == _sectionsCount) {
            firstX = canvas!!.width.toFloat()
        }
        for (i in 0.._currentSections) {
            xOffset = xSectionWidth * i + _currentOffset
            prev = Math.max(0.0f, xOffset - xSectionWidth)
            ratioSectionWidth = Math.abs(_interpolator!!.getInterpolation(prev) - _interpolator!!.getInterpolation(Math.min(xOffset, 1.0f)))
            sectionWidth = (width * ratioSectionWidth)
            if (sectionWidth + prev < width) {
                spaceLength = Math.min(sectionWidth, _separatorLength.toFloat())
            } else {
                spaceLength = 0.0f
            }
            drawLength = if (sectionWidth > spaceLength) sectionWidth - spaceLength else 0.0f
            end = prevValue + drawLength
            if (end > prevValue && i >= _startSection) {
                val xFinishingOffset = _interpolator!!.getInterpolation(Math.min(_finishingOffset, 1.0f))
                startX = Math.max(xFinishingOffset * width, Math.min(boundsWidth.toFloat(), prevValue))
                endX = Math.min(boundsWidth.toFloat(), end)
                drawLine(canvas, boundsWidth, startX, centerY.toFloat(), endX, centerY.toFloat(), currentIndexColor)
                if (i == _startSection) {
                    firstX = startX - _separatorLength
                }
            }
            if (i == _currentSections) {
                lastX = prevValue + sectionWidth
            }
            prevValue = end + spaceLength
            currentIndexColor = incrementColor(currentIndexColor)
        }
        drawBackgroundIfNeeded(canvas, firstX, lastX)
    }

    private fun drawBackgroundIfNeeded(canvas: Canvas?, firstX: Float, lastX: Float) {
        if (_backgroundDrawable == null) {
            return
        }
        _backgroundRect.top = ((canvas!!.height - _strokeWidth) / 2).toInt()
        _backgroundRect.bottom = ((canvas.height + _strokeWidth) / 2).toInt()
        _backgroundRect.left = 0
        _backgroundRect.right = if (_mirrorMode) canvas.width / 2 else canvas.width
        _backgroundDrawable?.bounds = _backgroundRect
        if (!isRunning) {
            if (_mirrorMode) {
                canvas.save()
                canvas.translate(canvas.width * 1.0f / 2, 0.0f)
                drawBackground(canvas, 0.0f, _backgroundRect.width().toFloat())
                canvas.scale(-1.0f, 1.0f)
                drawBackground(canvas, 0.0f, _backgroundRect.width().toFloat())
                canvas.restore()
            } else {
                drawBackground(canvas, 0.0f, _backgroundRect.width().toFloat())
            }
            return
        }
        if (!isFinishing() && !isStarting()) {
            return
        }
        var nfirstX = firstX
        var nlastX = lastX
        if (nfirstX > nlastX) {
            val temp = nfirstX
            nfirstX = nlastX
            nlastX = temp
        }
        if (nfirstX > 0) {
            if (_mirrorMode) {
                canvas.save();
                canvas.translate(canvas.width * 1.0f / 2, 0.0f)
                if (_reversed) {
                    drawBackground(canvas, 0.0f, nfirstX)
                    canvas.scale(-1.0f, 1.0f);
                    drawBackground(canvas, 0.0f, nfirstX)
                } else {
                    drawBackground(canvas, canvas.width / 2 - nfirstX, (canvas.width / 2).toFloat())
                    canvas.scale(-1.0f, 1.0f)
                    drawBackground(canvas, canvas.width / 2 - nfirstX, (canvas.width / 2).toFloat())
                }
                canvas.restore()
            } else {
                drawBackground(canvas, 0.0f, nfirstX)
            }
        }
        if (nlastX <= canvas.width) {
            if (_mirrorMode) {
                canvas.save()
                canvas.translate(canvas.width * 1.0f / 2, 0.0f)
                if (_reversed) {
                    drawBackground(canvas, nlastX, (canvas.width / 2).toFloat())
                    canvas.scale(-1.0f, 1.0f)
                    drawBackground(canvas, nlastX, (canvas.width / 2).toFloat())
                } else {
                    drawBackground(canvas, 0.0f, canvas.width / 2 - nlastX)
                    canvas.scale(-1.0f, 1.0f)
                    drawBackground(canvas, 0.0f, canvas.width / 2 - nlastX)
                }
                canvas.restore()
            } else {
                drawBackground(canvas, nlastX, canvas.width.toFloat())
            }
        }
    }

    fun isStarting(): Boolean = _currentSections < _sectionsCount

    private fun drawBackground(canvas: Canvas?, fromX: Float, toX: Float) {
        val count = canvas!!.save()
        canvas.clipRect(fromX, ((canvas.height - _strokeWidth) / 2), toX, ((canvas.height + _strokeWidth) / 2))
        _backgroundDrawable?.draw(canvas)
        canvas.restoreToCount(count)
    }

    private fun drawLine(canvas: Canvas?, canvasWidth: Int, startX: Float, startY: Float, stopX: Float, stopY: Float, currentIndexColor: Int) {
        _paint?.color = _colors!![currentIndexColor]
        if (!_mirrorMode) {
            canvas?.drawLine(startX, startY, stopX, stopY, _paint)
        } else {
            if (_reversed) {
                canvas?.drawLine(canvasWidth + startX, startY, canvasWidth + stopX, stopY, _paint)
                canvas?.drawLine(canvasWidth - startX, startY, canvasWidth - stopX, stopY, _paint)
            } else {
                canvas?.drawLine(startX, startY, stopX, stopY, _paint)
                canvas?.drawLine(canvasWidth * 2 - startX, startY, canvasWidth * 2 - stopX, stopY, _paint)
            }
        }
    }

    private fun drawGradient(canvas: Canvas?) {
        val xSectionWidth = 1.0f / _sectionsCount
        var currentIndexColor = _colorsIndex
        _linearGradientPositions!![0] = 0.0f
        _linearGradientPositions!![_linearGradientPositions!!.size - 1] = 1.0f
        var firstColorIndex = currentIndexColor - 1
        if (firstColorIndex < 0) {
            firstColorIndex += _colors!!.size
        }
        _linearGradientColors!![0] = _colors!![firstColorIndex]
        for (i in 0.._sectionsCount - 1) {
            val position = _interpolator!!.getInterpolation(i * xSectionWidth + _currentOffset)
            _linearGradientPositions!![i + 1] = position
            _linearGradientColors!![i + 1] = _colors!![currentIndexColor]
            currentIndexColor = (currentIndexColor + 1) % _colors!!.size
        }
        _linearGradientColors!![_linearGradientColors!!.size - 1] = _colors!![currentIndexColor]
        val left = if (_reversed) (if (_mirrorMode) Math.abs(_bounds!!.left - _bounds!!.right) / 2 else _bounds!!.left) else _bounds!!.left
        val right = if (_mirrorMode) (if (_reversed) _bounds!!.left else Math.abs(_bounds!!.left - _bounds!!.right) / 2) else _bounds!!.right
        val top = _bounds!!.centerY() - _strokeWidth / 2
        val bottom = _bounds!!.centerY() + _strokeWidth / 2
        val linearGradient = LinearGradient(left.toFloat(), top, right.toFloat(), bottom, _linearGradientColors, _linearGradientPositions, if (_mirrorMode) Shader.TileMode.MIRROR else Shader.TileMode.CLAMP)
        _paint?.shader = linearGradient
    }

    fun isFinishing(): Boolean = _finishing

    private fun incrementColor(colorIndex: Int): Int {
        var ncolorIndex = colorIndex
        ++ncolorIndex
        if (ncolorIndex >= _colors!!.size) {
            ncolorIndex = 0
        }
        return ncolorIndex
    }

    private fun decrementColor(colorIndex: Int): Int {
        var ncolorIndex = colorIndex
        --ncolorIndex
        if (ncolorIndex < 0) {
            ncolorIndex = _colors!!.size - 1
        }
        return ncolorIndex
    }

    override fun setAlpha(alpha: Int) {
        _paint?.alpha = alpha
    }

    override fun getOpacity(): Int = PixelFormat.TRANSPARENT

    override fun setColorFilter(cf: ColorFilter?) {
        _paint?.colorFilter = cf
    }

    override fun isRunning(): Boolean = _running

    override fun scheduleSelf(what: Runnable?, `when`: Long) {
        _running = true
        super.scheduleSelf(what, `when`)
    }

    override fun start() {
        if (_progressiveStartActivated) {
            resetProgressiveStart(0)
        }
        if (isRunning) {
            return
        }
        _callbacks?.onStart()
        scheduleSelf(_updater, SystemClock.uptimeMillis() + FRAME_DURATION)
        invalidateSelf()
    }

    private fun resetProgressiveStart(index: Int) {
        checkColorIndex(index)
        _currentOffset = 0.0f
        _finishing = false
        _finishingOffset = 0.0f
        _startSection = 0
        _currentSections = 0
        _colorsIndex = index
    }

    private fun checkColorIndex(index: Int) {
        if (index < 0 || index >= _colors!!.size) {
            throw IllegalArgumentException("Index ${index} not valid")
        }
    }

    override fun stop() {
        if (!isRunning) {
            return
        }
        _callbacks?.onStop()
        _running = false
        unscheduleSelf(_updater)
    }

    fun setInterpolator(interpolator: Interpolator?) {
        if (interpolator == null) {
            throw IllegalArgumentException("Interpolator cannot be null")
        }
        _interpolator = interpolator
        invalidateSelf()
    }

    fun setColors(colors: IntArray?) {
        if (colors == null || colors.size == 0) {
            throw IllegalArgumentException("Colors cannot be null or empty")
        }
        _colorsIndex = 0
        _colors = colors
        refreshLinearGradientOptions()
        invalidateSelf()
    }

    fun setColor(color: Int) = setColors(intArrayOf(color))

    fun setSpeed(speed: Float) {
        if (speed < 0) {
            throw IllegalArgumentException("Speed must be >= 0")
        }
        _speed = speed
        invalidateSelf()
    }

    fun setProgressiveStartSpeed(speed: Float) {
        if (speed < 0) {
            throw IllegalArgumentException("SpeedProgressiveStart must be >= 0")
        }
        _progressiveStartSpeed = speed
        invalidateSelf()
    }

    fun setProgressiveStopSpeed(speed: Float) {
        if (speed < 0) {
            throw IllegalArgumentException("SpeedProgressiveStop must be >= 0")
        }
        _progressiveStopSpeed = speed
        invalidateSelf()
    }

    fun setSectionsCount(sectionsCount: Int) {
        if (sectionsCount <= 0) {
            throw IllegalArgumentException("SectionsCount must be > 0")
        }
        _sectionsCount = sectionsCount
        _maxOffset = 1f / _sectionsCount
        _currentOffset %= _maxOffset
        refreshLinearGradientOptions()
        invalidateSelf()
    }

    fun setSeparatorLength(separatorLength: Int) {
        if (separatorLength < 0) {
            throw IllegalArgumentException("SeparatorLength must be >= 0")
        }
        _separatorLength = separatorLength
        invalidateSelf()
    }

    fun setStrokeWidth(strokeWidth: Float) {
        if (strokeWidth < 0) {
            throw IllegalArgumentException("The strokeWidth must be >= 0")
        }
        _paint?.strokeWidth = strokeWidth
        invalidateSelf()
    }

    fun setReversed(reversed: Boolean) {
        if (_reversed == reversed) {
            return
        }
        _reversed = reversed
        invalidateSelf()
    }

    fun setMirrorMode(mirrorMode: Boolean) {
        if (_mirrorMode == mirrorMode) {
            return
        }
        _mirrorMode = mirrorMode
        invalidateSelf()
    }

    fun setBackgroundDrawable(backgroundDrawable: Drawable?) {
        if (_backgroundDrawable == backgroundDrawable) {
            return
        }
        _backgroundDrawable = backgroundDrawable
        invalidateSelf()
    }

    fun getBackgroundDrawable(): Drawable? = _backgroundDrawable

    fun getColors(): IntArray? = _colors

    fun getStrokeWidth(): Float = _strokeWidth

    fun setProgressiveStartActivated(progressiveStartActivated: Boolean) {
        _progressiveStartActivated = progressiveStartActivated
    }

    fun setUseGradients(useGradients: Boolean) {
        if (_useGradients == useGradients) {
            return
        }
        _useGradients = useGradients
        refreshLinearGradientOptions()
        invalidateSelf()
    }

    fun progressiveStart() = progressiveStart(0)

    fun progressiveStart(index: Int) {
        resetProgressiveStart(index)
        start()
    }

    fun progressiveStop() {
        _finishing = true
        _startSection = 0
    }

    fun setCallbacks(callbacks: Callbacks?) {
        _callbacks = callbacks
    }

    class Builder {
        private var _interpolator: Interpolator? = null
        private var _sectionsCount = 0
        private var _colors: IntArray? = null
        private var _speed = 0.0f
        private var _progressiveStartSpeed = 0.0f
        private var _progressiveStopSpeed = 0.0f
        private var _reversed = false
        private var _mirrorMode = false
        private var _strokeWidth = 0.0f
        private var _strokeSeparatorLength = 0
        private var _progressiveStartActivated = false
        private var _generateBackgroundUsingColors = false
        private var _gradients = false
        private var _backgroundDrawableWhenHidden: Drawable? = null
        private var _onProgressiveStopEndedListener: Callbacks? = null

        constructor(context: Context): this(context, false)

        constructor(context: Context, editMode: Boolean) {
            initValues(context, editMode)
        }

        fun build(): SmoothProgressDrawable {
            if (_generateBackgroundUsingColors) {
                _backgroundDrawableWhenHidden = ProgressBarUtils.generateDrawableWithColors(_colors, _strokeWidth)
            }
            val ret = SmoothProgressDrawable(_interpolator, _sectionsCount, _strokeSeparatorLength, _colors, _strokeWidth, _speed, _progressiveStartSpeed, _progressiveStopSpeed, _reversed, _mirrorMode, _onProgressiveStopEndedListener, _progressiveStartActivated, _backgroundDrawableWhenHidden, _gradients)
            return ret
        }

        private fun initValues(context: Context, editMode: Boolean) {
            val res = context.resources
            _interpolator = AccelerateInterpolator()
            if (!editMode) {
                _sectionsCount = res.getInteger(R.integer.spbDefaultSectionsCount)
                _speed = (res.getString(R.string.spbDefaultSpeed)).toFloat()
                _reversed = res.getBoolean(R.bool.spbDefaultReversed)
                _progressiveStartActivated = res.getBoolean(R.bool.spbDefaultProgressiveStartActivated)
                _colors = intArrayOf(res.getColor(R.color.spbDefaultColor))
                _strokeSeparatorLength = res.getDimensionPixelSize(R.dimen.spbDefaultStrokeSeparatorLength)
                _strokeWidth = res.getDimensionPixelOffset(R.dimen.spbDefaultStrokeWidth).toFloat()
            } else {
                _sectionsCount = 4
                _speed = 1.0f
                _reversed = false
                _progressiveStartActivated = false
                _colors = intArrayOf(0xff33b5e5.toInt())
                _strokeSeparatorLength = 4
                _strokeWidth = 4.0f
            }
            _progressiveStartSpeed = _speed
            _progressiveStopSpeed = _speed
            _gradients = false
        }

        fun interpolator(interpolator: Interpolator?):Builder {
            ProgressBarUtils.checkNotNull(interpolator, "Interpolator")
            _interpolator = interpolator
            return this
        }

        fun sectionsCount(sectionsCount: Int): Builder {
            ProgressBarUtils.checkPositive(sectionsCount, "Sections count")
            _sectionsCount = sectionsCount
            return this
        }

        fun separatorLength(separatorLength: Int): Builder {
            ProgressBarUtils.checkPositiveOrZero(separatorLength.toFloat(), "Separator length")
            _strokeSeparatorLength = separatorLength
            return this
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

        fun strokeWidth(width: Float): Builder {
            ProgressBarUtils.checkPositiveOrZero(width, "Width")
            _strokeWidth = width
            return this
        }

        fun speed(speed: Float): Builder {
            ProgressBarUtils.checkSpeed(speed)
            _speed = speed
            return this
        }

        fun progressiveStartSpeed(progressiveStartSpeed: Float): Builder {
            ProgressBarUtils.checkSpeed(progressiveStartSpeed)
            _progressiveStartSpeed = progressiveStartSpeed
            return this
        }

        fun progressiveStopSpeed(progressiveStopSpeed: Float): Builder {
            ProgressBarUtils.checkSpeed(progressiveStopSpeed)
            _progressiveStopSpeed = progressiveStopSpeed
            return this
        }

        fun reversed(reversed: Boolean): Builder {
            _reversed = reversed
            return this
        }

        fun mirrorMode(mirrorMode: Boolean): Builder {
            _mirrorMode = mirrorMode
            return this
        }

        fun progressiveStart(progressiveStartActivated: Boolean): Builder {
            _progressiveStartActivated = progressiveStartActivated
            return this
        }

        fun callbacks(onProgressiveStopEndedListener: Callbacks?): Builder {
            _onProgressiveStopEndedListener = onProgressiveStopEndedListener
            return this
        }

        fun backgroundDrawable(backgroundDrawableWhenHidden: Drawable?): Builder {
            _backgroundDrawableWhenHidden = backgroundDrawableWhenHidden
            return this
        }

        fun generateBackgroundUsingColors(): Builder {
            _generateBackgroundUsingColors = true
            return this
        }

        fun gradients(): Builder = gradients(true)

        fun gradients(useGradients: Boolean): Builder {
            _gradients = useGradients
            return this
        }
    }

}