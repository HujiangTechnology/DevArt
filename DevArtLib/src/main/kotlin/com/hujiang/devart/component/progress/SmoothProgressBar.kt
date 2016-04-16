package com.hujiang.devart.component.progress

import android.content.Context
import android.graphics.Canvas
import android.graphics.drawable.Drawable
import android.util.AttributeSet
import android.view.animation.*
import android.widget.ProgressBar
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/15/16.
 */
open class SmoothProgressBar: ProgressBar {

    companion object {
        private val INTERPOLATOR_ACCELERATE = 0
        private val INTERPOLATOR_LINEAR = 1
        private val INTERPOLATOR_ACCELERATEDECELERATE = 2
        private val INTERPOLATOR_DECELERATE = 3
    }

    constructor(context: Context): this(context, null)
    constructor(context: Context, attrs: AttributeSet?): this(context, attrs, R.attr.spbStyle)

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        if (isInEditMode) {
            indeterminateDrawable = SmoothProgressDrawable.Builder(context, true).build()
            return
        }
        val res = context.resources
        val a = context.obtainStyledAttributes(attrs, R.styleable.SmoothProgressBar, defStyle, 0)
        val color = a.getColor(R.styleable.SmoothProgressBar_spbColor, res.getColor(R.color.spbDefaultColor))
        val sectionsCount = a.getInteger(R.styleable.SmoothProgressBar_spbSectionsCount, res.getInteger(R.integer.spbDefaultSectionsCount))
        val separatorLength = a.getDimensionPixelSize(R.styleable.SmoothProgressBar_spbStrokeSeparatorLength, res.getDimensionPixelSize(R.dimen.spbDefaultStrokeSeparatorLength))
        val strokeWidth = a.getDimension(R.styleable.SmoothProgressBar_spbStrokeWidth, res.getDimension(R.dimen.spbDefaultStrokeWidth))
        val speed = a.getFloat(R.styleable.SmoothProgressBar_spbSpeed, (res.getString(R.string.spbDefaultSpeed)).toFloat())
        val speedProgressiveStart = a.getFloat(R.styleable.SmoothProgressBar_spbProgressiveStartSpeed, speed)
        val speedProgressiveStop = a.getFloat(R.styleable.SmoothProgressBar_spbProgressiveStopSpeed, speed)
        val iInterpolator = a.getInteger(R.styleable.SmoothProgressBar_spbInterpolator, -1)
        val reversed = a.getBoolean(R.styleable.SmoothProgressBar_spbReversed, res.getBoolean(R.bool.spbDefaultReversed))
        val mirrorMode = a.getBoolean(R.styleable.SmoothProgressBar_spbMirrorMode, res.getBoolean(R.bool.spbDefaultMirrorMode))
        val colorsId = a.getResourceId(R.styleable.SmoothProgressBar_spbColors, 0)
        val progressiveStartActivated = a.getBoolean(R.styleable.SmoothProgressBar_spbProgressiveStartActivated, res.getBoolean(R.bool.spbDefaultProgressiveStartActivated))
        val backgroundDrawable = a.getDrawable(R.styleable.SmoothProgressBar_spbBackground)
        val generateBackgroundWithColors = a.getBoolean(R.styleable.SmoothProgressBar_spbGenerateBackgroundWithColors, false)
        val gradients = a.getBoolean(R.styleable.SmoothProgressBar_spbGradients, false)
        a.recycle()
        var interpolator: Interpolator? = null
        if (iInterpolator == -1) {
            interpolator = this.interpolator
        }
        if (interpolator == null) {
            when (iInterpolator) {
                INTERPOLATOR_ACCELERATEDECELERATE -> interpolator = AccelerateDecelerateInterpolator()
                INTERPOLATOR_DECELERATE -> interpolator = DecelerateInterpolator()
                INTERPOLATOR_LINEAR -> interpolator = LinearInterpolator()
                else -> interpolator = AccelerateInterpolator()
            }
        }
        var colors: IntArray? = null
        if (colorsId != 0) {
            colors = res.getIntArray(colorsId)
        }
        val builder = SmoothProgressDrawable.Builder(context)
                .speed(speed)
                .progressiveStartSpeed(speedProgressiveStart)
                .progressiveStopSpeed(speedProgressiveStop)
                .interpolator(interpolator)
                .sectionsCount(sectionsCount)
                .separatorLength(separatorLength)
                .strokeWidth(strokeWidth)
                .reversed(reversed)
                .mirrorMode(mirrorMode)
                .progressiveStart(progressiveStartActivated)
                .gradients(gradients)
        if (backgroundDrawable != null) {
            builder.backgroundDrawable(backgroundDrawable)
        }
        if (generateBackgroundWithColors) {
            builder.generateBackgroundUsingColors()
        }
        if (colors != null && colors.size > 0) {
            builder.colors(colors)
        } else {
            builder.color(color)
        }
        val d = builder.build()
        indeterminateDrawable = d
    }


    fun applyStyle(styleResId: Int) {
        val a = context.obtainStyledAttributes(null, R.styleable.SmoothProgressBar, 0, styleResId)
        if (a.hasValue(R.styleable.SmoothProgressBar_spbColor)) {
            setSmoothProgressDrawableColor(a.getColor(R.styleable.SmoothProgressBar_spbColor, 0))
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbColors)) {
            val colorsId = a.getResourceId(R.styleable.SmoothProgressBar_spbColors, 0)
            if (colorsId != 0) {
                val colors = getResources().getIntArray(colorsId)
                if (colors != null && colors.size > 0)
                    setSmoothProgressDrawableColors(colors)
            }
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbSectionsCount)) {
            setSmoothProgressDrawableSectionsCount(a.getInteger(R.styleable.SmoothProgressBar_spbSectionsCount, 0))
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbStrokeSeparatorLength)) {
            setSmoothProgressDrawableSeparatorLength(a.getDimensionPixelSize(R.styleable.SmoothProgressBar_spbStrokeSeparatorLength, 0))
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbStrokeWidth)) {
            setSmoothProgressDrawableStrokeWidth(a.getDimension(R.styleable.SmoothProgressBar_spbStrokeWidth, 0.0f))
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbSpeed)) {
            setSmoothProgressDrawableSpeed(a.getFloat(R.styleable.SmoothProgressBar_spbSpeed, 0.0f))
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbProgressiveStartSpeed)) {
            setSmoothProgressDrawableProgressiveStartSpeed(a.getFloat(R.styleable.SmoothProgressBar_spbProgressiveStartSpeed, 0.0f))
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbProgressiveStopSpeed)) {
            setSmoothProgressDrawableProgressiveStopSpeed(a.getFloat(R.styleable.SmoothProgressBar_spbProgressiveStopSpeed, 0.0f))
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbReversed)) {
            setSmoothProgressDrawableReversed(a.getBoolean(R.styleable.SmoothProgressBar_spbReversed, false))
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbMirrorMode)) {
            setSmoothProgressDrawableMirrorMode(a.getBoolean(R.styleable.SmoothProgressBar_spbMirrorMode, false))
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbProgressiveStartActivated)) {
            setProgressiveStartActivated(a.getBoolean(R.styleable.SmoothProgressBar_spbProgressiveStartActivated, false))
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbProgressiveStartActivated)) {
            setProgressiveStartActivated(a.getBoolean(R.styleable.SmoothProgressBar_spbProgressiveStartActivated, false))
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbGradients)) {
            setSmoothProgressDrawableUseGradients(a.getBoolean(R.styleable.SmoothProgressBar_spbGradients, false))
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbGenerateBackgroundWithColors)) {
            if (a.getBoolean(R.styleable.SmoothProgressBar_spbGenerateBackgroundWithColors, false)) {
                setSmoothProgressDrawableBackgroundDrawable(ProgressBarUtils.generateDrawableWithColors(checkIndeterminateDrawable().getColors(), checkIndeterminateDrawable().getStrokeWidth()))
            }
        }
        if (a.hasValue(R.styleable.SmoothProgressBar_spbInterpolator)) {
            val iInterpolator = a.getInteger(R.styleable.SmoothProgressBar_spbInterpolator, -1)
            var interpolator: Interpolator? = null
            when (iInterpolator) {
                INTERPOLATOR_ACCELERATEDECELERATE -> interpolator = AccelerateDecelerateInterpolator()
                INTERPOLATOR_DECELERATE -> interpolator = DecelerateInterpolator()
                INTERPOLATOR_LINEAR -> interpolator = LinearInterpolator()
                INTERPOLATOR_ACCELERATE -> interpolator = AccelerateInterpolator()
            }
            if (interpolator != null) {
                setInterpolator(interpolator)
            }
        }
        a.recycle()
    }

    override fun onDraw(canvas: Canvas?) {
        super.onDraw(canvas)
        if (isIndeterminate && indeterminateDrawable is SmoothProgressDrawable && !(indeterminateDrawable as SmoothProgressDrawable).isRunning) {
            indeterminateDrawable.draw(canvas)
        }
    }

    private fun checkIndeterminateDrawable(): SmoothProgressDrawable {
        val ret = indeterminateDrawable
        if (ret == null || ret !is SmoothProgressDrawable) {
            throw RuntimeException("The drawable is not a SmoothProgressDrawable")
        }
        return ret
    }

    override fun setInterpolator(interpolator: Interpolator?) {
        super.setInterpolator(interpolator)
        val ret = indeterminateDrawable
        if (ret != null && ret is SmoothProgressDrawable) {
            ret.setInterpolator(interpolator)
        }
    }

    fun setSmoothProgressDrawableInterpolator(interpolator: Interpolator?) = checkIndeterminateDrawable().setInterpolator(interpolator)

    fun setSmoothProgressDrawableColors(colors: IntArray?) = checkIndeterminateDrawable().setColors(colors)

    fun setSmoothProgressDrawableColor(color: Int) = checkIndeterminateDrawable().setColor(color)

    fun setSmoothProgressDrawableSpeed(speed: Float) = checkIndeterminateDrawable().setSpeed(speed)

    fun setSmoothProgressDrawableProgressiveStartSpeed(speed: Float) = checkIndeterminateDrawable().setProgressiveStartSpeed(speed)

    fun setSmoothProgressDrawableProgressiveStopSpeed(speed: Float) = checkIndeterminateDrawable().setProgressiveStopSpeed(speed)

    fun setSmoothProgressDrawableSectionsCount(sectionsCount: Int) = checkIndeterminateDrawable().setSectionsCount(sectionsCount)

    fun setSmoothProgressDrawableSeparatorLength(separatorLength: Int) = checkIndeterminateDrawable().setSeparatorLength(separatorLength)

    fun setSmoothProgressDrawableStrokeWidth(strokeWidth: Float) = checkIndeterminateDrawable().setStrokeWidth(strokeWidth)

    fun setSmoothProgressDrawableReversed(reversed: Boolean) = checkIndeterminateDrawable().setReversed(reversed)

    fun setSmoothProgressDrawableMirrorMode(mirrorMode: Boolean) = checkIndeterminateDrawable().setMirrorMode(mirrorMode)

    fun setProgressiveStartActivated(progressiveStartActivated: Boolean) = checkIndeterminateDrawable().setProgressiveStartActivated(progressiveStartActivated)

    fun setSmoothProgressDrawableCallbacks(listener: SmoothProgressDrawable.Callbacks?) = checkIndeterminateDrawable().setCallbacks(listener)

    fun setSmoothProgressDrawableBackgroundDrawable(drawable: Drawable?) = checkIndeterminateDrawable().setBackgroundDrawable(drawable)

    fun setSmoothProgressDrawableUseGradients(useGradients: Boolean) = checkIndeterminateDrawable().setUseGradients(useGradients)

    fun progressiveStart() = checkIndeterminateDrawable().progressiveStart()

    fun progressiveStop() = checkIndeterminateDrawable().progressiveStop()
}