package com.hujiang.devart.component.cropper

/**
 * Created by rarnu on 4/26/16.
 */
object MathUtil {

    fun calculateDistance(x1: Float, y1: Float, x2: Float, y2: Float): Float {
        val side1 = x2 - x1
        val side2 = y2 - y1
        return Math.sqrt((side1 * side1 + side2 * side2).toDouble()).toFloat()
    }

}