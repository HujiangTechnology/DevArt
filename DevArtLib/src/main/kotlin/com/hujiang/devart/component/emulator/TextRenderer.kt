package com.hujiang.devart.component.emulator

import android.graphics.Canvas

/**
 * Created by rarnu on 4/10/16.
 */
interface TextRenderer {
    companion object {
        val MODE_OFF = 0
        val MODE_ON = 1
        val MODE_LOCKED = 2
        val MODE_MASK = 3
        val MODE_SHIFT_SHIFT = 0
        val MODE_ALT_SHIFT = 2
        val MODE_CTRL_SHIFT = 4
        val MODE_FN_SHIFT = 6
    }
    fun setReverseVideo(reverseVideo: Boolean)
    fun getCharacterWidth(): Float
    fun getCharacterHeight(): Int
    fun getTopMargin(): Int
    fun drawTextRun(canvas: Canvas?, x: Float, y: Float, lineOffset: Int, runWidth: Int, text: CharArray?, index: Int, count: Int, cursor: Boolean, textStyle: Int)
    fun drawCursor(canvas: Canvas?, x: Float, y: Float, lineOffset: Int, cursorMode: Int)
}
