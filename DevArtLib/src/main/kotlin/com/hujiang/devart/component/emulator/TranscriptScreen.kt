package com.hujiang.devart.component.emulator

import android.graphics.Canvas
import java.util.*

/**
 * Created by rarnu on 4/11/16.
 */
class TranscriptScreen: Screen {

    private var _columns = 0
    private var _totalRows = 0
    private var _screenRows = 0
    private var _data: UnicodeTranscript? = null

    constructor(columns: Int, totalRows: Int, screenRows: Int, scheme: ColorScheme?) {
        init(columns, totalRows, screenRows, TextStyle.kNormalTextStyle)
    }

    private fun init(columns: Int, totalRows: Int, screenRows: Int, style: Int) {
        _columns = columns
        _totalRows = totalRows
        _screenRows = screenRows
        _data = UnicodeTranscript(columns, totalRows, screenRows, style)
        _data?.blockSet(0, 0, _columns, _screenRows, ' '.toInt(), style)
    }

    fun setColorScheme(scheme: ColorScheme?) {
        _data?.setDefaultStyle(TextStyle.kNormalTextStyle)
    }

    fun finish() {
        _data = null
    }

    override fun setLineWrap(row: Int) {
        _data?.setLineWrap(row)
    }

    override fun set(x: Int, y: Int, codePoint: Int, style: Int) {
        _data?.setChar(x, y, codePoint, style)
    }

    override fun set(x: Int, y: Int, b: Byte, style: Int) {
        _data?.setChar(x, y, b.toInt(), style)
    }

    override fun scroll(topMargin: Int, bottomMargin: Int, style: Int) {
        _data?.scroll(topMargin, bottomMargin, style)
    }

    override fun blockCopy(sx: Int, sy: Int, w: Int, h: Int, dx: Int, dy: Int) {
        _data?.blockCopy(sx, sy, w, h, dx, dy)
    }

    override fun blockSet(sx: Int, sy: Int, w: Int, h: Int, value: Int, style: Int) {
        _data?.blockSet(sx, sy, w, h, value, style)
    }

    override fun getTranscriptText(): String? = internalGetTranscriptText(null, 0, -_data!!.getActiveTranscriptRows(), _columns, _screenRows)

    override fun getTranscriptText(colors: GrowableIntArray?): String? = internalGetTranscriptText(colors, 0, -_data!!.getActiveTranscriptRows(), _columns, _screenRows)

    override fun getSelectedText(x1: Int, y1: Int, x2: Int, y2: Int): String? = internalGetTranscriptText(null, x1, y1, x2, y2)

    override fun getSelectedText(colors: GrowableIntArray?, x1: Int, y1: Int, x2: Int, y2: Int): String? = internalGetTranscriptText(colors, x1, y1, x2, y2)

    override fun getActiveRows(): Int = _data!!.getActiveRows()

    fun getActiveTranscriptRows(): Int = _data!!.getActiveTranscriptRows()

    override fun fastResize(columns: Int, rows: Int, cursor: IntArray?): Boolean {
        if (_data == null) {
            return true
        }
        if (_data!!.resize(columns, rows, cursor)) {
            _columns = columns
            _screenRows = rows
            return true
        } else {
            return false
        }
    }

    override fun resize(columns: Int, rows: Int, style: Int) = init(columns, _totalRows, rows, style)

    fun drawText(row: Int, canvas: Canvas?, x: Float, y: Float, renderer: TextRenderer?, cx: Int, selx1: Int, selx2: Int, imeText: String?, cursorMode: Int) {
        var line: CharArray?
        var color: StyleRow?
        try {
            line = _data?.getLine(row)
            color = _data?.getLineColor(row)
        } catch (e: Exception) {
            return
        }
        val defaultStyle = _data!!.getDefaultStyle()
        if (line == null) {
            if (selx1 != selx2) {
                val blank = CharArray(selx2 - selx1)
                Arrays.fill(blank, ' ')
                renderer?.drawTextRun(canvas, x, y, selx1, selx2 - selx1, blank, 0, 1, true, defaultStyle)
            } else if (cx != -1) {
                renderer?.drawCursor(canvas, x, y, cx, cursorMode)
            }
            return
        }
        val columns = _columns
        var lastStyle = 0
        var lastCursorStyle = false
        var runWidth = 0
        var lastRunStart = -1
        var lastRunStartIndex = -1
        var forceFlushRun = false
        var column = 0
        var index = 0
        while (column < columns) {
            val style = color?.get(column)
            var cursorStyle = false
            var incr = 1
            var width: Int
            if (Character.isHighSurrogate(line[index])) {
                width = UnicodeTranscript.charWidth(line, index)
                incr++
            } else {
                width = UnicodeTranscript.charWidth(line[index].toInt())
            }
            if (column >= selx1 && column <= selx2) {
                cursorStyle = true
            }
            if (style != lastStyle || cursorStyle != lastCursorStyle || (width > 0 && forceFlushRun)) {
                if (lastRunStart >= 0) {
                    renderer?.drawTextRun(canvas, x, y, lastRunStart, runWidth, line, lastRunStartIndex, index - lastRunStartIndex, lastCursorStyle, lastStyle)
                }
                lastStyle = style!!
                lastCursorStyle = cursorStyle
                runWidth = 0
                lastRunStart = column
                lastRunStartIndex = index
                forceFlushRun = false
            }
            runWidth += width
            column += width
            index += incr
            if (width > 1) {
                forceFlushRun = true
            }
        }
        if (lastRunStart >= 0) {
            renderer?.drawTextRun(canvas, x, y, lastRunStart, runWidth, line, lastRunStartIndex, index - lastRunStartIndex, lastCursorStyle, lastStyle)
        }
        if (cx >= 0 && imeText!!.length > 0) {
            val imeLength = Math.min(columns, imeText.length)
            val imeOffset = imeText.length - imeLength
            val imePosition = Math.min(cx, columns - imeLength)
            renderer?.drawTextRun(canvas, x, y, imePosition, imeLength, imeText.toCharArray(), imeOffset, imeLength, true, TextStyle.encode(0x0f, 0x00, TextStyle.fxNormal))
        }
        if (cx >= 0) {
            renderer?.drawCursor(canvas, x, y, cx, cursorMode)
        }
    }

    private fun internalGetTranscriptText(colors: GrowableIntArray?, selX1: Int, selY1: Int, selX2: Int, selY2: Int): String? {
        val builder = StringBuilder()
        var data = _data
        val columns = _columns
        var line: CharArray? = null
        var rowColorBuffer: StyleRow? = null
        var nselY1 = selY1
        if (nselY1 < -data!!.getActiveTranscriptRows()) {
            nselY1 = -data!!.getActiveTranscriptRows()
        }
        var nselY2 = selY2
        if (nselY2 >= _screenRows) {
            nselY2 = _screenRows - 1
        }
        for (row in nselY1..nselY2) {
            var x1 = 0
            var x2: Int
            if (row == nselY1) {
                x1 = selX1
            }
            if (row == nselY2) {
                x2 = selX2 + 1
                if (x2 > columns) {
                    x2 = columns
                }
            } else {
                x2 = columns
            }
            line = data.getLine(row, x1, x2)
            if (colors != null) {
                rowColorBuffer = data.getLineColor(row, x1, x2)
            }
            if (line == null) {
                if (!data.getLineWrap(row) && row < nselY2 && row < _screenRows - 1) {
                    builder.append('\n')
                    colors?.append(0)
                }
                continue
            }
            var defaultColor = _data!!.getDefaultStyle()
            var lastPrintingChar = -1
            var lineLen = line.size
            var i = 0
            val width = x2 - x1
            var column = 0
            while (i < lineLen && column < width) {
                val c = line[i]
                if (c == 0.toChar()) {
                    break
                } else if (c != ' ' || ((rowColorBuffer != null) && (rowColorBuffer.get(column) != defaultColor))) {
                    lastPrintingChar = i
                }
                if (!Character.isLowSurrogate(c)) {
                    column += UnicodeTranscript.charWidth(line, i)
                }
                ++i
            }
            if (data.getLineWrap(row) && lastPrintingChar > -1 && x2 == columns) {
                lastPrintingChar = i - 1;
            }
            builder.append(line, 0, lastPrintingChar + 1)
            if (colors != null) {
                if (rowColorBuffer != null) {
                    column = 0
                    var j = 0
                    while (j <= lastPrintingChar) {
                        colors.append(rowColorBuffer.get(column))
                        column += UnicodeTranscript.charWidth(line, j)
                        if (Character.isHighSurrogate(line[j])) {
                            ++j
                        }
                        ++j
                    }
                } else {
                    var j = 0
                    while (j <= lastPrintingChar) {
                        colors.append(defaultColor)
                        val c = line[j]
                        if (Character.isHighSurrogate(c)) {
                            ++j
                        }
                        ++j
                    }
                }
            }
            if (!data.getLineWrap(row) && row < selY2 && row < _screenRows - 1) {
                builder.append('\n')
                colors?.append(0)
            }
        }
        return builder.toString()
    }
}