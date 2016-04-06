package com.hujiang.devart.component.flip

import android.graphics.Canvas

/**
 * Created by rarnu on 4/6/16.
 */
interface OverFlipper {

    fun calculate(flipDistance: Float, minFlipDistance: Float, maxFlipDistance: Float): Float
    fun draw(c: Canvas?): Boolean
    fun overFlipEnded()
    fun getTotalOverFlip(): Float
}