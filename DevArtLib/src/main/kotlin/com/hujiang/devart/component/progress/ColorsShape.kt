package com.hujiang.devart.component.progress

import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.drawable.shapes.Shape

/**
 * Created by rarnu on 4/15/16.
 */
class ColorsShape: Shape {

    private var _strokeWidth = 0.0f
    private var _colors: IntArray? = null

    constructor(strokeWidth: Float, colors: IntArray?) {
        _strokeWidth = strokeWidth
        _colors = colors
    }

    fun getStrokeWidth(): Float = _strokeWidth

    fun setStrokeWidth(strokeWidth: Float) {
        _strokeWidth = strokeWidth
    }

    fun getColors(): IntArray? = _colors

    fun setColors(colors: IntArray?) {
        _colors = colors
    }

    override fun draw(canvas: Canvas?, paint: Paint?) {
        val ratio = 1f / _colors!!.size
        var i = 0
        paint?.strokeWidth = _strokeWidth
        for (color in _colors!!) {
            paint?.color = color
            canvas?.drawLine(i * ratio * width, height / 2, (++i) * ratio * width, height / 2, paint)
        }
    }

}