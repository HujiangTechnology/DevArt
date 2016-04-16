package com.hujiang.devart.component.progress

import android.view.animation.Interpolator

/**
 * Created by rarnu on 4/15/16.
 */
class Options {

    var angleInterpolator: Interpolator? = null
    var sweepInterpolator: Interpolator? = null
    var borderWidth = 0.0f
    var colors: IntArray? = null
    var sweepSpeed = 0.0f
    var rotationSpeed = 0.0f
    var minSweepAngle = 0
    var maxSweepAngle = 0
    var style = 0

    constructor(angleInterpolator: Interpolator?, sweepInterpolator: Interpolator?, borderWidth: Float, colors: IntArray?, sweepSpeed: Float, rotationSpeed: Float, minSweepAngle: Int, maxSweepAngle: Int, style: Int) {
        this.angleInterpolator = angleInterpolator
        this.sweepInterpolator = sweepInterpolator
        this.borderWidth = borderWidth
        this.colors = colors
        this.sweepSpeed = sweepSpeed
        this.rotationSpeed = rotationSpeed
        this.minSweepAngle = minSweepAngle
        this.maxSweepAngle = maxSweepAngle
        this.style = style
    }

}