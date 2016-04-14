package com.hujiang.devart.component.arcmenu

import android.view.animation.Animation
import android.view.animation.Transformation

/**
 * Created by rarnu on 4/14/16.
 */
class RotateAndTranslateAnimation: Animation {

    private var _fromXType = ABSOLUTE
    private var _toXType = ABSOLUTE
    private var _fromYType = ABSOLUTE
    private var _toYType = ABSOLUTE
    private var _fromXValue = 0.0f
    private var _toXValue = 0.0f
    private var _fromYValue = 0.0f
    private var _toYValue = 0.0f
    private var _fromXDelta = 0.0f
    private var _toXDelta = 0.0f
    private var _fromYDelta = 0.0f
    private var _toYDelta = 0.0f
    private var _fromDegrees = 0.0f
    private var _toDegrees = 0.0f
    private var _pivotXType = ABSOLUTE
    private var _pivotYType = ABSOLUTE
    private var _pivotXValue = 0.0f
    private var _pivotYValue = 0.0f
    private var _pivotX = 0.0f
    private var _pivotY = 0.0f

    constructor(fromXDelta: Float, toXDelta: Float, fromYDelta: Float, toYDelta: Float, fromDegrees: Float, toDegrees: Float) {
        _fromXValue = fromXDelta
        _toXValue = toXDelta
        _fromYValue = fromYDelta
        _toYValue = toYDelta
        _fromXType = ABSOLUTE
        _toXType = ABSOLUTE
        _fromYType = ABSOLUTE
        _toYType = ABSOLUTE
        _fromDegrees = fromDegrees
        _toDegrees = toDegrees
        _pivotXValue = 0.5f
        _pivotXType = RELATIVE_TO_SELF
        _pivotYValue = 0.5f
        _pivotYType = RELATIVE_TO_SELF
    }

    override fun applyTransformation(interpolatedTime: Float, t: Transformation?) {
        val degrees = _fromDegrees + ((_toDegrees - _fromDegrees) * interpolatedTime)
        if (_pivotX == 0.0f && _pivotY == 0.0f) {
            t?.matrix?.setRotate(degrees)
        } else {
            t?.matrix?.setRotate(degrees, _pivotX, _pivotY)
        }
        var dx = _fromXDelta
        var dy = _fromYDelta
        if (_fromXDelta != _toXDelta) {
            dx = _fromXDelta + ((_toXDelta - _fromXDelta) * interpolatedTime)
        }
        if (_fromYDelta != _toYDelta) {
            dy = _fromYDelta + ((_toYDelta - _fromYDelta) * interpolatedTime)
        }
        t?.matrix?.postTranslate(dx, dy)
    }

    override fun initialize(width: Int, height: Int, parentWidth: Int, parentHeight: Int) {
        super.initialize(width, height, parentWidth, parentHeight)
        _fromXDelta = resolveSize(_fromXType, _fromXValue, width, parentWidth)
        _toXDelta = resolveSize(_toXType, _toXValue, width, parentWidth)
        _fromYDelta = resolveSize(_fromYType, _fromYValue, height, parentHeight)
        _toYDelta = resolveSize(_toYType, _toYValue, height, parentHeight)
        _pivotX = resolveSize(_pivotXType, _pivotXValue, width, parentWidth)
        _pivotY = resolveSize(_pivotYType, _pivotYValue, height, parentHeight)
    }

}