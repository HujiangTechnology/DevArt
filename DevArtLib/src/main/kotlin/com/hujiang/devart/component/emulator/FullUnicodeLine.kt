package com.hujiang.devart.component.emulator

/**
 * Created by rarnu on 4/11/16.
 */
class FullUnicodeLine {

    companion object {
        private val SPARE_CAPACITY_FACTOR = 1.5f
    }

    private var _text: CharArray? = null
    private var _offset: ShortArray? = null
    private var _columns = 0

    constructor(columns: Int) {
        commonConstructor(columns)
        val text =_text
        for (i in 0..columns - 1) {
            text!![i] = ' '
        }
        _offset!![0] = columns.toShort()
    }

    constructor(basicLine: CharArray?) {
        commonConstructor(basicLine!!.size)
        System.arraycopy(basicLine, 0, _text, 0, _columns)
        _offset!![0] = basicLine.size.toShort()
    }

    private fun commonConstructor(columns: Int) {
        _columns = columns
        _offset = ShortArray(columns)
        _text = CharArray((SPARE_CAPACITY_FACTOR * columns).toInt())
    }

    fun getSpaceUsed(): Int = _offset!![0].toInt()

    fun getLine(): CharArray? = _text

    fun findStartOfColumn(column: Int): Int {
        if (column == 0) {
            return 0
        } else {
            return column + _offset!![column]
        }
    }

    fun getChar(column: Int, charIndex: Int, out: CharArray?, offset: Int): Boolean {
        val pos = findStartOfColumn(column)
        var length: Int
        if (column + 1 < _columns) {
            length = findStartOfColumn(column + 1) - pos
        } else {
            length = getSpaceUsed() - pos
        }
        if (charIndex >= length) {
            throw IllegalArgumentException()
        }
        out!![offset] = _text!![pos + charIndex]
        return (charIndex + 1 < length)
    }

    fun setChar(column: Int, codePoint: Int) {
        var ncolumn = column
        val columns = _columns
        if (ncolumn < 0 || ncolumn >= columns) {
            throw IllegalArgumentException()
        }
        var text = _text
        val offset = _offset
        var spaceUsed = offset!![0].toInt()
        val pos = findStartOfColumn(ncolumn)
        val charWidth = UnicodeTranscript.charWidth(codePoint)
        val oldCharWidth = UnicodeTranscript.charWidth(text, pos)
        var oldLen: Int
        if (ncolumn + oldCharWidth < columns) {
            oldLen = findStartOfColumn(ncolumn + oldCharWidth) - pos
        } else {
            oldLen = spaceUsed - pos
        }
        var newLen = Character.charCount(codePoint)
        if (charWidth == 0) {
            newLen += oldLen
        }
        var shift = newLen - oldLen
        if (shift > 0) {
            if (spaceUsed + shift > text!!.size) {
                val newText = CharArray(text.size + columns)
                System.arraycopy(text, 0, newText, 0, pos)
                System.arraycopy(text, pos + oldLen, newText, pos + newLen, spaceUsed - pos - oldLen)
                _text = newText
                text = newText
            } else {
                System.arraycopy(text, pos + oldLen, text, pos + newLen, spaceUsed - pos - oldLen)
            }
        }
        if (charWidth > 0) {
            Character.toChars(codePoint, text, pos)
        } else {
            Character.toChars(codePoint, text, pos + oldLen)
        }
        if (shift < 0) {
            System.arraycopy(text, pos + oldLen, text, pos + newLen, spaceUsed - pos - oldLen)
        }
        if (shift != 0) {
            spaceUsed += shift
            offset[0] = spaceUsed.toShort()
        }
        if (oldCharWidth == 2 && charWidth == 1) {
            val nextPos = pos + newLen
            if (spaceUsed + 1 > text!!.size) {
                val newText = CharArray(text.size + columns)
                System.arraycopy(text, 0, newText, 0, nextPos)
                System.arraycopy(text, nextPos, newText, nextPos + 1, spaceUsed - nextPos)
                _text = newText
                text = newText
            } else {
                System.arraycopy(text, nextPos, text, nextPos + 1, spaceUsed - nextPos)
            }
            text[nextPos] = ' '
            ++offset[0]
            if (ncolumn == 0) {
                offset[1] = (newLen - 1).toShort()
            } else if (ncolumn + 1 < columns) {
                offset[ncolumn + 1] = (offset[ncolumn] + newLen - 1).toShort()
            }
            ++ncolumn
            ++shift
        } else if (oldCharWidth == 1 && charWidth == 2) {
            if (ncolumn == columns - 1) {
                text!![pos] = ' '
                offset[0] = (pos + 1).toShort()
                shift = 0
            } else if (ncolumn == columns - 2) {
                offset[ncolumn + 1] = (offset[ncolumn] - 1).toShort()
                offset[0] = (pos + newLen).toShort()
                shift = 0
            } else {
                val nextPos = pos + newLen
                val nextWidth = UnicodeTranscript.charWidth(text, nextPos)
                var nextLen: Int
                if (ncolumn + nextWidth + 1 < columns) {
                    nextLen = findStartOfColumn(ncolumn + nextWidth + 1) + shift - nextPos
                } else {
                    nextLen = spaceUsed - nextPos
                }
                if (nextWidth == 2) {
                    text!![nextPos] = ' '
                    if (nextLen > 1) {
                        System.arraycopy(text, nextPos + nextLen, text, nextPos + 1, spaceUsed - nextPos - nextLen)
                        shift -= nextLen - 1
                        offset[0] = (offset[0] - (nextLen - 1)).toShort()
                    }
                } else {
                    System.arraycopy(text, nextPos + nextLen, text, nextPos, spaceUsed - nextPos - nextLen)
                    shift -= nextLen
                    offset[0] = (offset[0] - nextLen).toShort()
                }
                if (ncolumn == 0) {
                    offset[1] = -1
                } else {
                    offset[ncolumn + 1] = (offset[ncolumn] - 1).toShort()
                }
                ++ncolumn
            }
        }
        if (shift != 0) {
            for (i in ncolumn + 1..columns - 1) {
                offset[i] = (offset[i] + shift).toShort()
            }
        }
    }
}