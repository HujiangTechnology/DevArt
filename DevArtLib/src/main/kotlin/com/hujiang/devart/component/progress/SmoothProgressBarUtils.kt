package com.hujiang.devart.component.progress

import android.graphics.drawable.Drawable
import android.graphics.drawable.ShapeDrawable

/**
 * Created by rarnu on 4/15/16.
 */
object SmoothProgressBarUtils {

    fun generateDrawableWithColors(colors: IntArray?, strokeWidth: Float): Drawable? {
        if (colors == null || colors.size == 0) {
            return null
        }
        return ShapeDrawable(ColorsShape(strokeWidth, colors))
    }

    fun checkSpeed(speed: Float) {
        if (speed <= 0.0f) {
            throw IllegalArgumentException("Speed must be >= 0")
        }
    }

    fun checkColors(colors: IntArray?) {
        if (colors == null || colors.size == 0) {
            throw IllegalArgumentException("You must provide at least 1 color")
        }
    }

    fun checkAngle(angle: Int) {
        if (angle < 0 || angle > 360) {
            throw IllegalArgumentException("Illegal angle ${angle}: must be >=0 and <= 360")
        }
    }

    fun checkPositiveOrZero(number: Float, name: String?) {
        if (number < 0) {
            throw IllegalArgumentException("${name} ${number} must be positive")
        }
    }

    fun checkPositive(number: Int, name: String?){
        if(number <= 0) {
            throw IllegalArgumentException("${name} must not be null")
        }
    }

    fun checkNotNull(o: Any?, name: String?) {
        if (o == null) {
            throw IllegalArgumentException("${name} must be not null")
        }
    }

}