package com.hujiang.devart.component.emulator

import android.text.AndroidCharacter

/**
 * Created by rarnu on 4/11/16.
 */
class UnicodeTranscript {

    companion object {
        fun charWidth(codePoint: Int): Int {
            if (codePoint > 31 && codePoint < 127) {
                return 1
            }
            if (codePoint == 27) {
                return 1
            }
            when (Character.getType(codePoint)) {
                Character.CONTROL.toInt(), Character.FORMAT.toInt(), Character.NON_SPACING_MARK.toInt(), Character.ENCLOSING_MARK.toInt() -> return 0
            }

            if (Character.charCount(codePoint) == 1) {
                when (AndroidCharacter.getEastAsianWidth(codePoint.toChar())) {
                    AndroidCharacter.EAST_ASIAN_WIDTH_FULL_WIDTH, AndroidCharacter.EAST_ASIAN_WIDTH_WIDE -> return 2
                }
            } else {
                when ((codePoint shr 16) and 0xf) {
                    2, 3 -> return 2
                }
            }
            return 1
        }

        fun charWidth(cHigh: Char, cLow: Char): Int = charWidth(Character.toCodePoint(cHigh, cLow))

        fun charWidth(chars: CharArray?, index: Int): Int {
            val c = chars!![index]
            if (Character.isHighSurrogate(c)) {
                return charWidth(c, chars[index + 1])
            } else {
                return charWidth(c.toInt())
            }
        }
    }

    private var _lines: Array<Any?>? = null
    private var _color: Array<StyleRow?>? = null
    private var _lineWrap: BooleanArray? = null
    private var _totalRows = 0
    private var _screenRows = 0
    private var _columns = 0
    private var _activeTranscriptRows = 0
    private var _defaultStyle = 0
    private var _screenFirstRow = 0
    private var _tmpLine: CharArray? = null
    private var _tmpColor: StyleRow? = null

    constructor(columns: Int, totalRows: Int, screenRows: Int, defaultStyle: Int) {
        _columns = columns
        _totalRows = totalRows
        _screenRows = screenRows
        _lines = arrayOfNulls<Any?>(totalRows)
        _color = arrayOfNulls<StyleRow>(totalRows)
        _lineWrap = BooleanArray(totalRows)
        _tmpColor = StyleRow(defaultStyle, _columns)
        _defaultStyle = defaultStyle
    }

    fun getDefaultStyle(): Int = _defaultStyle

    fun setDefaultStyle(defaultStyle: Int) {
        _defaultStyle = defaultStyle
    }

    fun getActiveTranscriptRows(): Int = _activeTranscriptRows

    fun getActiveRows(): Int = _activeTranscriptRows + _screenRows

    private fun externalToInternalRow(extRow: Int): Int {
        if (extRow < -_activeTranscriptRows || extRow > _screenRows) {
            val errorMessage = "externalToInternalRow ${extRow} ${_screenRows} ${_activeTranscriptRows}"
            throw IllegalArgumentException(errorMessage)
        }
        if (extRow >= 0) {
            return (_screenFirstRow + extRow) % _totalRows
        } else {
            if (-extRow > _screenFirstRow) {
                return _totalRows + _screenFirstRow + extRow
            } else {
                return _screenFirstRow + extRow
            }
        }
    }

    fun setLineWrap(row: Int) {
        _lineWrap!![externalToInternalRow(row)] = true
    }

    fun getLineWrap(row: Int): Boolean = _lineWrap!![externalToInternalRow(row)]

    fun resize(newColumns: Int, newRows: Int, cursor: IntArray?): Boolean {
        if (newColumns != _columns || newRows > _totalRows) {
            return false
        }
        val screenRows = _screenRows
        val activeTranscriptRows = _activeTranscriptRows
        var shift = screenRows - newRows
        if (shift < -activeTranscriptRows) {
            val lines = _lines
            val color = _color
            val lineWrap = _lineWrap
            val screenFirstRow = _screenFirstRow
            val totalRows = _totalRows
            for (i in 0..activeTranscriptRows - shift - 1) {
                val index = (screenFirstRow + screenRows + i) % totalRows
                lines!![index] = null
                color!![index] = null
                lineWrap!![index] = false
            }
            shift = -activeTranscriptRows
        } else if (shift > 0 && cursor!![1] != screenRows - 1) {
            val lines = _lines
            for (i in screenRows - 1 downTo cursor[1] + 1) {
                val index = externalToInternalRow(i)
                if (lines!![index] == null) {
                    --shift
                    if (shift == 0) {
                        break
                    } else {
                        continue
                    }
                }
                var line: CharArray?
                if (lines[index] is CharArray) {
                    line = lines[index] as CharArray
                } else {
                    line = (lines[index] as FullUnicodeLine).getLine()
                }
                val len = line!!.size
                var idx: Int = -1
                for (j in 0..len - 1) {
                    if (line[j] == 0.toChar()) {
                        idx = len
                        break
                    } else if (line[j] != ' ') {
                        break
                    }
                }
                if (idx == len) {
                    --shift
                    if (shift == 0) {
                        break
                    } else {
                        continue
                    }
                } else {
                    break
                }
            }
        }
        if (shift > 0 || (shift < 0 && _screenFirstRow >= -shift)) {
            _screenFirstRow = (_screenFirstRow + shift) % _totalRows
        } else if (shift < 0) {
            _screenFirstRow += _totalRows + shift
        }

        if (_activeTranscriptRows + shift < 0) {
            _activeTranscriptRows = 0
        } else {
            _activeTranscriptRows += shift
        }
        cursor!![1] -= shift
        _screenRows = newRows
        return true
    }

    private fun blockCopyLines(src: Int, len: Int, shift: Int) {
        val totalRows = _totalRows
        var dst: Int
        if (src + shift >= 0) {
            dst = (src + shift) % totalRows
        } else {
            dst = totalRows + src + shift
        }
        if (src + len <= totalRows && dst + len <= totalRows) {
            System.arraycopy(_lines, src, _lines, dst, len)
            System.arraycopy(_color, src, _color, dst, len)
            System.arraycopy(_lineWrap, src, _lineWrap, dst, len)
            return
        }
        if (shift < 0) {
            for (i in 0..len - 1) {
                _lines!![(dst + i) % totalRows] = _lines!![(src + i) % totalRows]
                _color!![(dst + i) % totalRows] = _color!![(src + i) % totalRows]
                _lineWrap!![(dst + i) % totalRows] = _lineWrap!![(src + i) % totalRows]
            }
        } else {
            for (i in len - 1 downTo 0) {
                _lines!![(dst + i) % totalRows] = _lines!![(src + i) % totalRows]
                _color!![(dst + i) % totalRows] = _color!![(src + i) % totalRows]
                _lineWrap!![(dst + i) % totalRows] = _lineWrap!![(src + i) % totalRows]
            }
        }
    }

    fun scroll(topMargin: Int, bottomMargin: Int, style: Int) {
        if (topMargin > bottomMargin - 1) {
            throw IllegalArgumentException()
        }
        if (topMargin < 0) {
            throw IllegalArgumentException()
        }
        if (bottomMargin > _screenRows) {
            throw IllegalArgumentException()
        }
        val screenRows = _screenRows
        val totalRows = _totalRows

        if (topMargin == 0 && bottomMargin == screenRows) {
            _screenFirstRow = (_screenFirstRow + 1) % totalRows
            if (_activeTranscriptRows < totalRows - screenRows) {
                ++_activeTranscriptRows
            }
            val blankRow = externalToInternalRow(bottomMargin - 1)
            _lines!![blankRow] = null
            _color!![blankRow] = StyleRow(style, _columns)
            _lineWrap!![blankRow] = false
            return
        }
        val screenFirstRow = _screenFirstRow
        val topMarginInt = externalToInternalRow(topMargin)
        val bottomMarginInt = externalToInternalRow(bottomMargin)
        val lines = _lines
        val color = _color
        val lineWrap = _lineWrap
        val scrollLine = lines!![topMarginInt]
        val scrollColor = color!![topMarginInt]
        val scrollLineWrap = lineWrap!![topMarginInt]
        blockCopyLines(screenFirstRow, topMargin, 1)
        blockCopyLines(bottomMarginInt, screenRows - bottomMargin, 1)
        lines[screenFirstRow] = scrollLine
        color[screenFirstRow] = scrollColor
        lineWrap[screenFirstRow] = scrollLineWrap
        _screenFirstRow = (screenFirstRow + 1) % totalRows
        if (_activeTranscriptRows < totalRows - screenRows) {
            ++_activeTranscriptRows
        }
        val blankRow = externalToInternalRow(bottomMargin - 1)
        lines[blankRow] = null
        color[blankRow] = StyleRow(style, _columns)
        lineWrap[blankRow] = false
    }

    fun blockCopy(sx: Int, sy: Int, w: Int, h: Int, dx: Int, dy: Int) {
        if (sx < 0 || sx + w > _columns || sy < 0 || sy + h > _screenRows || dx < 0 || dx + w > _columns || dy < 0 || dy + h > _screenRows) {
            throw IllegalArgumentException()
        }
        val lines = _lines
        val color = _color
        if (sy > dy) {
            for (y in 0..h - 1) {
                val srcRow = externalToInternalRow(sy + y)
                val dstRow = externalToInternalRow(dy + y)
                if (lines!![srcRow] is CharArray && lines[dstRow] is CharArray) {
                    System.arraycopy(lines[srcRow], sx, lines[dstRow], dx, w)
                } else {
                    val extDstRow = dy + y
                    val tmp = getLine(sy + y, sx, sx + w)
                    if (tmp == null) {
                        blockSet(dx, extDstRow, w, 1, ' '.toInt(), _defaultStyle)
                        continue
                    }
                    var cHigh = 0.toChar()
                    var x = 0
                    val columns = _columns
                    for (i in 0..tmp.size - 1) {
                        if (tmp[i] == 0.toChar() || dx + x >= columns) {
                            break
                        }
                        if (Character.isHighSurrogate(tmp[i])) {
                            cHigh = tmp[i]
                            continue
                        } else if (Character.isLowSurrogate(tmp[i])) {
                            val codePoint = Character.toCodePoint(cHigh, tmp[i])
                            setChar(dx + x, extDstRow, codePoint)
                            x += charWidth(codePoint)
                        } else {
                            setChar(dx + x, extDstRow, tmp[i].toInt())
                            x += charWidth(tmp[i].toInt())
                        }
                    }
                }
                color!![srcRow]?.copy(sx, color[dstRow], dx, w)
            }
        } else {
            for (y in 0..h - 1) {
                val y2 = h - (y + 1)
                val srcRow = externalToInternalRow(sy + y2)
                val dstRow = externalToInternalRow(dy + y2)
                if (lines!![srcRow] is CharArray && lines[dstRow] is CharArray) {
                    System.arraycopy(lines[srcRow], sx, lines[dstRow], dx, w)
                } else {
                    val extDstRow = dy + y2
                    val tmp = getLine(sy + y2, sx, sx + w)
                    if (tmp == null) {
                        blockSet(dx, extDstRow, w, 1, ' '.toInt(), _defaultStyle)
                        continue
                    }
                    var cHigh = 0.toChar()
                    var x = 0
                    val columns = _columns
                    for (i in 0..tmp.size - 1) {
                        if (tmp[i] == 0.toChar() || dx + x >= columns) {
                            break
                        }
                        if (Character.isHighSurrogate(tmp[i])) {
                            cHigh = tmp[i]
                            continue
                        } else if (Character.isLowSurrogate(tmp[i])) {
                            val codePoint = Character.toCodePoint(cHigh, tmp[i])
                            setChar(dx + x, extDstRow, codePoint)
                            x += charWidth(codePoint)
                        } else {
                            setChar(dx + x, extDstRow, tmp[i].toInt())
                            x += charWidth(tmp[i].toInt())
                        }
                    }
                }
                color!![srcRow]?.copy(sx, color[dstRow], dx, w)
            }
        }
    }

    fun blockSet(sx: Int, sy: Int, w: Int, h: Int, value: Int, style: Int) {
        if (sx < 0 || sx + w > _columns || sy < 0 || sy + h > _screenRows) {
            throw IllegalArgumentException()
        }
        for (y in 0..h - 1) {
            for (x in 0..w - 1) {
                setChar(sx + x, sy + y, value, style)
            }
        }
    }

    fun getLine(row: Int, x1: Int, x2: Int): CharArray? {
        if (row < -_activeTranscriptRows || row > _screenRows - 1) {
            throw IllegalArgumentException()
        }
        val columns = _columns
        var nrow = externalToInternalRow(row)
        if (_lines!![nrow] == null) {
            return null
        }
        if (_lines!![nrow] is CharArray) {
            if (x1 == 0 && x2 == columns) {
                return _lines!![row] as CharArray
            } else {
                if (_tmpLine == null || _tmpLine!!.size < columns + 1) {
                    _tmpLine = CharArray(columns + 1)
                }
                val length = x2 - x1;
                System.arraycopy(_lines!![nrow], x1, _tmpLine, 0, length)
                _tmpLine!![length] = 0.toChar()
                return _tmpLine
            }
        }
        val line = _lines!![nrow] as FullUnicodeLine
        val rawLine = line.getLine()
        var nx1 = line.findStartOfColumn(x1)
        var nx2 = x2
        if (nx2 < columns) {
            nx2 = line.findStartOfColumn(nx2)
        } else {
            nx2 = line.getSpaceUsed()
        }
        val length = nx2 - nx1
        if (_tmpLine == null || _tmpLine!!.size < length + 1) {
            _tmpLine = CharArray(length + 1)
        }
        System.arraycopy(rawLine, x1, _tmpLine, 0, length)
        _tmpLine!![length] = 0.toChar()
        return _tmpLine
    }

    fun getLine(row: Int): CharArray? = getLine(row, 0, _columns)

    fun getLineColor(row: Int, x1: Int, x2: Int): StyleRow? {
        if (row < -_activeTranscriptRows || row > _screenRows - 1) {
            throw IllegalArgumentException()
        }
        var nrow = externalToInternalRow(row)
        val color = _color!![nrow]
        val tmp = _tmpColor
        if (color != null) {
            if (x1 == 0 && x2 == _columns) {
                return color
            }
            color.copy(x1, tmp, 0, x2 - x1)
            return tmp
        } else {
            return null
        }
    }

    fun getLineColor(row: Int): StyleRow? = getLineColor(row, 0, _columns)

    fun getChar(row: Int, column: Int): Boolean = getChar(row, column, 0)

    fun getChar(row: Int, column: Int, charIndex: Int): Boolean = getChar(row, column, charIndex, CharArray(1), 0)

    fun getChar(row: Int, column: Int, charIndex: Int, out: CharArray?, offset: Int): Boolean {
        if (row < -_activeTranscriptRows || row > _screenRows - 1) {
            throw IllegalArgumentException()
        }
        var nrow = externalToInternalRow(row)
        if (_lines!![nrow] is CharArray) {
            val line = _lines!![nrow] as CharArray
            out!![offset] = line[column]
            return false
        }
        val line = _lines!![row] as FullUnicodeLine?
        return line!!.getChar(column, charIndex, out, offset)
    }

    private fun isBasicChar(codePoint: Int): Boolean = !(charWidth(codePoint) != 1 || Character.charCount(codePoint) != 1)

    private fun allocateBasicLine(row: Int, columns: Int): CharArray? {
        val line = CharArray(columns)
        for (i in 0..columns - 1) {
            line[i] = ' '
        }
        _lines!![row] = line
        if (_color!![row] == null) {
            _color!![row] = StyleRow(0, columns)
        }
        return line
    }

    private fun allocateFullLine(row: Int, columns: Int): FullUnicodeLine? {
        val line = FullUnicodeLine(columns)
        _lines!![row] = line
        if (_color!![row] == null) {
            _color!![row] = StyleRow(0, columns)
        }
        return line
    }

    fun setChar(column: Int, row: Int, codePoint: Int, style: Int): Boolean {
        if (!setChar(column, row, codePoint)) {
            return false;
        }
        var nrow = externalToInternalRow(row)
        _color!![nrow]?.set(column, style)
        return true
    }

    fun setChar(column: Int, row: Int, codePoint: Int): Boolean {
        if (row >= _screenRows || column >= _columns) {
            throw IllegalArgumentException()
        }
        var nrow =externalToInternalRow(row)
        var basicMode = -1
        if (_lines!![row] == null) {
            if (isBasicChar(codePoint)) {
                allocateBasicLine(row, _columns)
                basicMode = 1
            } else {
                allocateFullLine(row, _columns)
                basicMode = 0
            }
        }
        if (_lines!![row] is CharArray) {
            val line = _lines!![row] as CharArray
            if (basicMode == -1) {
                if (isBasicChar(codePoint)) {
                    basicMode = 1
                } else {
                    basicMode = 0
                }
            }
            if (basicMode == 1) {
                line[column] = codePoint.toChar()
                return true
            }
            _lines!![row] = FullUnicodeLine(line)
        }
        val line = _lines!![row] as FullUnicodeLine?
        line?.setChar(column, codePoint)
        return true
    }
}