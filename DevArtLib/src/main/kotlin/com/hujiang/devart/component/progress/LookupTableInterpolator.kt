package com.hujiang.devart.component.progress

import android.view.animation.Interpolator

/**
 * Created by rarnu on 4/15/16.
 */
abstract class LookupTableInterpolator: Interpolator {

    private var _values: FloatArray? = null
    private var _stepSize = 0.0f

    constructor(values: FloatArray?) {
        _values = values
        _stepSize = 1.0f / (_values!!.size - 1)
    }

    override fun getInterpolation(input: Float): Float {
        if (input >= 1.0f) {
            return 1.0f
        }
        if (input <= 0.0f) {
            return 0f
        }
        val position = Math.min((input * (_values!!.size - 1)).toInt(), _values!!.size - 2)
        val quantized = position * _stepSize
        val diff = input - quantized
        val weight = diff / _stepSize
        return _values!![position] + weight * (_values!![position + 1] - _values!![position])
    }

}