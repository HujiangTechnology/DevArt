package com.hujiang.devart.component.gesturelock

/**
 * Created by rarnu on 4/5/16.
 */
class LockCircle {

    private var _ox = 0
    var ox: Int
        get() = _ox
        set(value) { _ox = value }
    private var _oy = 0
    var oy: Int
        get() = _oy
        set(value) { _oy = value }
    private var _r = 0.0f
    var r: Float
        get() = _r
        set(value) { _r = value }
    private var _num = 0
    var num: Int
        get() = _num
        set(value) { _num = value }
    private var _onTouch = false
    var onTouch: Boolean
        get() = _onTouch
        set(value) { _onTouch = value }

    fun isPointIn(x: Int, y: Int): Boolean {
        val distance = Math.sqrt(((x - _ox) * (x - _ox) + (y - _oy) * (y - _oy)).toDouble())
        return distance < _r
    }

}