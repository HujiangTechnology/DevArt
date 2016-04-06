package com.hujiang.devart.component.flip

import android.graphics.Canvas

/**
 * Created by rarnu on 4/6/16.
 */
class RubberBandOverFlipper: OverFlipper {

    companion object {
        private val MAX_OVER_FLIP_DISTANCE = 70.0f
        private val EXPONENTIAL_DECREES = 0.85f
    }

    private var _totalOverFlip = 0.0f
    private var _currentOverFlip = 0.0f


    override fun calculate(flipDistance: Float, minFlipDistance: Float, maxFlipDistance: Float): Float {
        var deltaOverFlip: Float
        if(flipDistance < minFlipDistance) {
            deltaOverFlip = flipDistance - minFlipDistance - _currentOverFlip
        }else {
            deltaOverFlip = flipDistance - maxFlipDistance - _currentOverFlip
        }
        _totalOverFlip += deltaOverFlip
        val sign = Math.signum(_totalOverFlip)
        _currentOverFlip = (Math.pow(Math.abs(_totalOverFlip).toDouble(), EXPONENTIAL_DECREES.toDouble()) * sign).toFloat()
        if(_currentOverFlip < 0) {
            _currentOverFlip = Math.max(-MAX_OVER_FLIP_DISTANCE, _currentOverFlip)
        }else {
            _currentOverFlip = Math.min(MAX_OVER_FLIP_DISTANCE, _currentOverFlip)
        }
        return _currentOverFlip + (if (_currentOverFlip < 0) minFlipDistance else maxFlipDistance)
    }

    override fun draw(c: Canvas?): Boolean = false

    override fun overFlipEnded() {
        _totalOverFlip = 0.0f
        _currentOverFlip = 0.0f
    }

    override fun getTotalOverFlip(): Float = _totalOverFlip

}