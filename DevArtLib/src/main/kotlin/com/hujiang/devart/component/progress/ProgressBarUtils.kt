package com.hujiang.devart.component.progress

import android.animation.ValueAnimator
import android.content.Context
import android.graphics.drawable.Drawable
import android.graphics.drawable.ShapeDrawable
import android.os.Build
import android.os.PowerManager

/**
 * Created by rarnu on 4/15/16.
 */
object ProgressBarUtils {

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
            throw IllegalArgumentException("Illegal angle $angle: must be >=0 and <= 360")
        }
    }

    fun checkPositiveOrZero(number: Float, name: String?) {
        if (number < 0) {
            throw IllegalArgumentException("$name $number must be positive")
        }
    }

    fun checkPositive(number: Int, name: String?){
        if(number <= 0) {
            throw IllegalArgumentException("$name must not be null")
        }
    }

    fun checkNotNull(o: Any?, name: String?) {
        if (o == null) {
            throw IllegalArgumentException("$name must be not null")
        }
    }

    fun getAnimatedFraction(animator: ValueAnimator?): Float {
        var fraction = if (animator!!.duration > 0) animator.currentPlayTime * 1.0f / animator.duration else 0.0f
        fraction = Math.min(fraction, 1.0f)
        fraction = animator.interpolator.getInterpolation(fraction)
        return fraction
    }

    fun isPowerSaveModeEnabled(powerManager: PowerManager?): Boolean {
        if (Build.VERSION.SDK_INT < 21) {
            return false
        }
        try {
            // this method is already exists under API 21
            // for compile under API 19, here use reflect for calling...
            val mode = powerManager!!.javaClass.getDeclaredMethod("isPowerSaveMode")
            val b = mode.invoke(powerManager) as Boolean
            return b
        } catch (e: Exception) {
            return false
        }
    }

    fun powerManager(context: Context): PowerManager? = context.getSystemService(Context.POWER_SERVICE) as PowerManager?

}