package com.hujiang.devart.component.emulator

/**
 * Created by rarnu on 4/11/16.
 */
interface Screen {

    fun setLineWrap(row: Int)

    fun set(x: Int, y: Int, codePoint: Int, style: Int)

    fun set(x: Int, y: Int, b: Byte, style: Int)

    fun scroll(topMargin: Int, bottomMargin: Int, style: Int)

    fun blockCopy(sx: Int, sy: Int, w: Int, h: Int, dx: Int, dy: Int)

    fun blockSet(sx: Int, sy: Int, w: Int, h: Int, value: Int, style: Int)

    fun getTranscriptText(): String?

    fun getTranscriptText(colors: GrowableIntArray?): String?

    fun getSelectedText(x1: Int, y1: Int, x2: Int, y2: Int): String?

    fun getSelectedText(colors: GrowableIntArray?, x1: Int, y1: Int, x2: Int, y2: Int): String?

    fun getActiveRows(): Int

    fun fastResize(columns: Int, rows: Int, cursor: IntArray?): Boolean

    fun resize(columns: Int, rows: Int, style: Int)

}