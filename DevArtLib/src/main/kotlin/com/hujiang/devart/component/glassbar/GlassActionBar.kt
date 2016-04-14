package com.hujiang.devart.component.glassbar

/**
 * Created by rarnu on 4/14/16.
 */
object GlassActionBar {

    val DEFAULT_BLUR_RADIUS = 7
    val MIN_BLUR_RADIUS = 1
    val MAX_BLUR_RADIUS = 20

    val DEFAULT_DOWNSAMPLING = 5
    val MIN_DOWNSAMPLING = 1
    val MAX_DOWNSAMPLING = 6


    fun isValidBlurRadius(value: Int): Boolean = value >= MIN_BLUR_RADIUS && value <= MAX_BLUR_RADIUS

    fun isValidDownsampling(value: Int): Boolean = value >= MIN_DOWNSAMPLING && value <= MAX_DOWNSAMPLING

}