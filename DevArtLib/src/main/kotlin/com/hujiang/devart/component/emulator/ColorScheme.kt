package com.hujiang.devart.component.emulator

/**
 * Created by rarnu on 4/10/16.
 */
class ColorScheme {

    private var _foreColor = 0
    private var _backColor = 0

    constructor(foreColor: Int, backColor: Int) {
        _foreColor = foreColor
        _backColor = backColor
    }

    constructor(scheme: IntArray?) {
        if (scheme!!.size != 2) {
            throw IllegalArgumentException()
        }
        _foreColor = scheme[0]
        _backColor = scheme[1]
    }

    fun getForeColor(): Int = _foreColor

    fun getBackColor(): Int = _backColor

}