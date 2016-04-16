package com.hujiang.devart.component.progress

import android.content.Context
import android.graphics.drawable.Drawable
import android.util.AttributeSet
import android.widget.ProgressBar
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/15/16.
 */
class CircularProgressBar : ProgressBar {

    constructor(context: Context) : this(context, null)

    constructor(context: Context, attrs: AttributeSet?) : this(context, attrs, R.attr.cpbStyle)

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int) : super(context, attrs, defStyle) {
        if (isInEditMode) {
            indeterminateDrawable = CircularProgressDrawable.Builder(context, true).build()
            return
        }
        val res = context.resources
        val a = context.obtainStyledAttributes(attrs, R.styleable.CircularProgressBar, defStyle, 0)
        val color = a.getColor(R.styleable.CircularProgressBar_cpbColor, res.getColor(R.color.cpbDefaultColor))
        val strokeWidth = a.getDimension(R.styleable.CircularProgressBar_cpbStrokeWidth, res.getDimension(R.dimen.cpbDefaultStrokeWidth))
        val sweepSpeed = a.getFloat(R.styleable.CircularProgressBar_cpbSweepSpeed, (res.getString(R.string.cpbDefaultSweepSpeed)).toFloat())
        val rotationSpeed = a.getFloat(R.styleable.CircularProgressBar_cpbRotationSpeed, (res.getString(R.string.cpbDefaultRotationSpeed)).toFloat())
        val colorsId = a.getResourceId(R.styleable.CircularProgressBar_cpbColors, 0)
        val minSweepAngle = a.getInteger(R.styleable.CircularProgressBar_cpbMinSweepAngle, res.getInteger(R.integer.cpbDefaultMinSweepAngle))
        val maxSweepAngle = a.getInteger(R.styleable.CircularProgressBar_cpbMaxSweepAngle, res.getInteger(R.integer.cpbDefaultMaxSweepAngle))
        a.recycle()
        var colors: IntArray? = null
        if (colorsId != 0) {
            colors = res.getIntArray(colorsId)
        }
        var indeterminateDrawable: Drawable?
        val builder = CircularProgressDrawable.Builder(context)
                .sweepSpeed(sweepSpeed)
                .rotationSpeed(rotationSpeed)
                .strokeWidth(strokeWidth)
                .minSweepAngle(minSweepAngle)
                .maxSweepAngle(maxSweepAngle)
        if (colors != null && colors.size > 0) {
            builder.colors(colors)
        } else {
            builder.color(color)
        }
        indeterminateDrawable = builder.build()
        this.indeterminateDrawable = indeterminateDrawable
    }

    private fun checkIndeterminateDrawable(): CircularProgressDrawable {
        val ret = indeterminateDrawable
        if (ret == null || ret !is CircularProgressDrawable) {
            throw RuntimeException("The drawable is not a CircularProgressDrawable")
        }
        return ret
    }

    fun progressiveStop() = checkIndeterminateDrawable().progressiveStop()

    fun progressiveStop(listener: CircularProgressDrawable.OnEndListener?) = checkIndeterminateDrawable().progressiveStop(listener)

}