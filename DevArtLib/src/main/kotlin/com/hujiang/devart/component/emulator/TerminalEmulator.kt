package com.hujiang.devart.component.emulator

import java.io.UnsupportedEncodingException
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.Charset
import java.nio.charset.CharsetDecoder
import java.nio.charset.CodingErrorAction

/**
 * Created by rarnu on 4/11/16.
 */
class TerminalEmulator {

    companion object {
        private val MAX_ESCAPE_PARAMETERS = 16
        private val MAX_OSC_STRING_LENGTH = 512
        private val ESC_NONE = 0
        private val ESC = 1
        private val ESC_POUND = 2
        private val ESC_SELECT_LEFT_PAREN = 3
        private val ESC_SELECT_RIGHT_PAREN = 4
        private val ESC_LEFT_SQUARE_BRACKET = 5
        private val ESC_LEFT_SQUARE_BRACKET_QUESTION_MARK = 6
        private val ESC_PERCENT = 7
        private val ESC_RIGHT_SQUARE_BRACKET = 8
        private val ESC_RIGHT_SQUARE_BRACKET_ESC = 9
        private val K_132_COLUMN_MODE_MASK = 1 shl 3
        private val K_REVERSE_VIDEO_MASK = 1 shl 5
        private val K_ORIGIN_MODE_MASK = 1 shl 6
        private val K_WRAPAROUND_MODE_MASK = 1 shl 7
        private val K_SHOW_CURSOR_MASK = 1 shl 25
        private val K_DECSC_DECRC_MASK = K_ORIGIN_MODE_MASK or K_WRAPAROUND_MODE_MASK
        private val CHAR_SET_UK = 0
        private val CHAR_SET_ASCII = 1
        private val CHAR_SET_SPECIAL_GRAPHICS = 2
        private val CHAR_SET_ALT_STANDARD = 3
        private val CHAR_SET_ALT_SPECIAL_GRAPICS = 4
        private val _specialGraphicsCharMap = CharArray(128)
        private val UNICODE_REPLACEMENT_CHAR = 0xfffd
        private val DEFAULT_TO_AUTOWRAP_ENABLED = true

        init {
            for (i in 0..128 - 1) {
                _specialGraphicsCharMap[i] = i.toChar()
            }
            _specialGraphicsCharMap['_'.toInt()] = ' '
            _specialGraphicsCharMap['b'.toInt()] = 0x2409.toChar()
            _specialGraphicsCharMap['c'.toInt()] = 0x240C.toChar()
            _specialGraphicsCharMap['d'.toInt()] = 0x240D.toChar()
            _specialGraphicsCharMap['e'.toInt()] = 0x240A.toChar()
            _specialGraphicsCharMap['h'.toInt()] = 0x2424.toChar()
            _specialGraphicsCharMap['i'.toInt()] = 0x240B.toChar()
            _specialGraphicsCharMap['}'.toInt()] = 0x00A3.toChar()
            _specialGraphicsCharMap['f'.toInt()] = 0x00B0.toChar()
            _specialGraphicsCharMap['`'.toInt()] = 0x2B25.toChar()
            _specialGraphicsCharMap['~'.toInt()] = 0x2022.toChar()
            _specialGraphicsCharMap['y'.toInt()] = 0x2264.toChar()
            _specialGraphicsCharMap['|'.toInt()] = 0x2260.toChar()
            _specialGraphicsCharMap['z'.toInt()] = 0x2265.toChar()
            _specialGraphicsCharMap['g'.toInt()] = 0x00B1.toChar()
            _specialGraphicsCharMap['{'.toInt()] = 0x03C0.toChar()
            _specialGraphicsCharMap['.'.toInt()] = 0x25BC.toChar()
            _specialGraphicsCharMap[','.toInt()] = 0x25C0.toChar()
            _specialGraphicsCharMap['+'.toInt()] = 0x25B6.toChar()
            _specialGraphicsCharMap['-'.toInt()] = 0x25B2.toChar()
            _specialGraphicsCharMap['h'.toInt()] = '#'
            _specialGraphicsCharMap['a'.toInt()] = 0x2592.toChar()
            _specialGraphicsCharMap['0'.toInt()] = 0x2588.toChar()
            _specialGraphicsCharMap['q'.toInt()] = 0x2500.toChar()
            _specialGraphicsCharMap['x'.toInt()] = 0x2502.toChar()
            _specialGraphicsCharMap['m'.toInt()] = 0x2514.toChar()
            _specialGraphicsCharMap['j'.toInt()] = 0x2518.toChar()
            _specialGraphicsCharMap['l'.toInt()] = 0x250C.toChar()
            _specialGraphicsCharMap['k'.toInt()] = 0x2510.toChar()
            _specialGraphicsCharMap['w'.toInt()] = 0x252C.toChar()
            _specialGraphicsCharMap['u'.toInt()] = 0x2524.toChar()
            _specialGraphicsCharMap['t'.toInt()] = 0x251C.toChar()
            _specialGraphicsCharMap['v'.toInt()] = 0x2534.toChar()
            _specialGraphicsCharMap['n'.toInt()] = 0x253C.toChar()
            _specialGraphicsCharMap['o'.toInt()] = 0x23BA.toChar()
            _specialGraphicsCharMap['p'.toInt()] = 0x23BB.toChar()
            _specialGraphicsCharMap['r'.toInt()] = 0x23BC.toChar()
            _specialGraphicsCharMap['s'.toInt()] = 0x23BD.toChar()
        }
    }

    private var _cursorRow = 0
    private var _cursorCol = 0
    private var _rows = 0
    private var _columns = 0
    private var _screen: Screen? = null
    private var _session: TermSession? = null
    private var _argIndex = 0
    private var _args = IntArray(MAX_ESCAPE_PARAMETERS)
    private var _oSCArg = ByteArray(MAX_OSC_STRING_LENGTH)
    private var _oSCArgLength = 0
    private var _oSCArgTokenizerIndex = 0
    private var _continueSequence = false
    private var _escapeState = 0
    private var _savedCursorRow = 0
    private var _savedCursorCol = 0
    private var _savedEffect = 0
    private var _savedDecFlags_DECSC_DECRC = 0
    private var _decFlags = 0
    private var _savedDecFlags = 0
    private var _insertMode = false
    private var _tabStop: BooleanArray? = null
    private var _topMargin = 0
    private var _bottomMargin = 0
    private var _aboutToAutoWrap = false
    private var _lastEmittedCharWidth = 0
    private var _justWrapped = false
    private var _foreColor = 0
    private var _defaultForeColor = 0
    private var _backColor = 0
    private var _defaultBackColor = 0
    private var _effect = 0
    private var _mbKeypadApplicationMode = false
    private var _alternateCharSet = false
    private var _charSet = IntArray(2)
    private var _useAlternateCharSet = false
    private var _scrollCounter = 0
    private var _defaultUTF8Mode = false
    private var _UTF8Mode = false
    private var _UTF8EscapeUsed = false
    private var _UTF8ToFollow = 0
    private var _UTF8ByteBuffer: ByteBuffer? = null
    private var _inputCharBuffer: CharBuffer? = null
    private var _UTF8Decoder: CharsetDecoder? = null
    private var _UTF8ModeNotify: UpdateCallback? = null


    constructor(session: TermSession?, screen: Screen?, columns: Int, rows: Int, scheme: ColorScheme?) {
        _session = session
        _screen = screen
        _rows = rows
        _columns = columns
        _tabStop = BooleanArray(_columns)
        setColorScheme(scheme)
        _UTF8ByteBuffer = ByteBuffer.allocate(4)
        _inputCharBuffer = CharBuffer.allocate(2)
        _UTF8Decoder = Charset.forName("UTF-8").newDecoder()
        _UTF8Decoder?.onMalformedInput(CodingErrorAction.REPLACE)
        _UTF8Decoder?.onUnmappableCharacter(CodingErrorAction.REPLACE)
        reset()
    }

    fun reset() {
        _cursorRow = 0
        _cursorCol = 0
        _argIndex = 0
        _continueSequence = false
        _escapeState = ESC_NONE
        _savedCursorRow = 0
        _savedCursorCol = 0
        _savedEffect = 0
        _savedDecFlags_DECSC_DECRC = 0
        _decFlags = 0
        if (DEFAULT_TO_AUTOWRAP_ENABLED) {
            _decFlags = _decFlags or K_WRAPAROUND_MODE_MASK
        }
        _decFlags = _decFlags or K_SHOW_CURSOR_MASK
        _savedDecFlags = 0
        _insertMode = false
        _topMargin = 0
        _bottomMargin = _rows
        _aboutToAutoWrap = false
        _foreColor = _defaultForeColor
        _backColor = _defaultBackColor
        _mbKeypadApplicationMode = false
        _alternateCharSet = false
        _charSet[0] = CHAR_SET_ASCII
        _charSet[1] = CHAR_SET_SPECIAL_GRAPHICS
        computeEffectiveCharSet()
        setDefaultTabStops()
        blockClear(0, 0, _columns, _rows)
        setUTF8Mode(_defaultUTF8Mode)
        _UTF8EscapeUsed = false
        _UTF8ToFollow = 0
        _UTF8ByteBuffer?.clear()
        _inputCharBuffer?.clear()
    }

    fun setUTF8Mode(utf8Mode: Boolean) {
        if (utf8Mode && !_UTF8Mode) {
            _UTF8ToFollow = 0
            _UTF8ByteBuffer?.clear()
            _inputCharBuffer?.clear()
        }
        _UTF8Mode = utf8Mode
        _UTF8ModeNotify?.onUpdate()
    }

    fun append(buffer: ByteArray?, base: Int, length: Int) {
        for (i in 0..length - 1) {
            val b = buffer!![base + i]
            try {
                process(b)
            } catch (e: Exception) {

            }
        }
    }

    private fun process(b: Byte) {
        process(b, true)
    }

    private fun process(b: Byte, doUTF8: Boolean) {
        if (doUTF8 && _UTF8Mode && handleUTF8Sequence(b)) {
            return
        }
        if ((b.toInt() and 0x80) == 0x80 && (b.toInt() and 0x7f) <= 0x1f) {
            process(27.toByte(), false)
            process(((b.toInt() and 0x7f) + 0x40).toByte(), false)
            return
        }
        when (b.toInt()) {
            0 -> {}
            7 -> if (_escapeState == ESC_RIGHT_SQUARE_BRACKET) { doEscRightSquareBracket(b) }
            8 -> setCursorCol(Math.max(0, _cursorCol - 1))
            9 -> setCursorCol(nextTabStop(_cursorCol))
            13 -> setCursorCol(0)
            10, 11, 12 -> doLinefeed()
            14 -> setAltCharSet(true)
            15 -> setAltCharSet(false)
            24, 26 -> if (_escapeState != ESC_NONE) {
                _escapeState = ESC_NONE
                emit(127.toByte())
            }
            27 -> if (_escapeState != ESC_RIGHT_SQUARE_BRACKET) {
                startEscapeSequence(ESC)
            } else {
                doEscRightSquareBracket(b)
            }
            else -> {
                _continueSequence = false
                when (_escapeState) {
                    ESC_NONE -> if (b >= 32) { emit(b) }
                    ESC -> doEsc(b)
                    ESC_POUND -> doEscPound(b)
                    ESC_SELECT_LEFT_PAREN -> doEscSelectLeftParen(b)
                    ESC_SELECT_RIGHT_PAREN -> doEscSelectRightParen(b)
                    ESC_LEFT_SQUARE_BRACKET -> doEscLeftSquareBracket(b)
                    ESC_LEFT_SQUARE_BRACKET_QUESTION_MARK -> doEscLSBQuest(b)
                    ESC_PERCENT -> doEscPercent(b)
                    ESC_RIGHT_SQUARE_BRACKET -> doEscRightSquareBracket(b)
                    ESC_RIGHT_SQUARE_BRACKET_ESC -> doEscRightSquareBracketEsc(b)
                    else -> unknownSequence(b)
                }
                if (!_continueSequence) {
                    _escapeState = ESC_NONE
                }
            }
        }
    }

    private fun getDecFlagsMask(argument: Int): Int {
        if (argument >= 1 && argument <= 32) {
            return (1 shl argument)
        }
        return 0
    }

    private fun doEscLSBQuest(b: Byte) {
        val mask = getDecFlagsMask(getArg0(0))
        val oldFlags = _decFlags
        when (b) {
            'h'.toByte() -> _decFlags = _decFlags or mask
            'l'.toByte() -> _decFlags = _decFlags and mask.inv()
            'r'.toByte() -> _decFlags = (_decFlags and mask.inv()) or (_savedDecFlags and mask)
            's'.toByte() -> _savedDecFlags = (_savedDecFlags and mask.inv()) or (_decFlags and mask)
            else -> parseArg(b)
        }
        val newlySetFlags = oldFlags.inv() and _decFlags
        val changedFlags = oldFlags xor _decFlags
        if ((changedFlags and K_132_COLUMN_MODE_MASK) != 0) {
            blockClear(0, 0, _columns, _rows)
            setCursorRowCol(0, 0)
        }
        if ((newlySetFlags and K_ORIGIN_MODE_MASK) != 0) {
            setCursorPosition(0, 0)
        }
    }

    private fun setCursorPosition(x: Int, y: Int) {
        var effectiveTopMargin = 0
        var effectiveBottomMargin = _rows
        if ((_decFlags and K_ORIGIN_MODE_MASK) != 0) {
            effectiveTopMargin = _topMargin
            effectiveBottomMargin = _bottomMargin
        }
        val newRow = Math.max(effectiveTopMargin, Math.min(effectiveTopMargin + y, effectiveBottomMargin - 1))
        val newCol = Math.max(0, Math.min(x, _columns - 1))
        setCursorRowCol(newRow, newCol)
    }

    private fun setCursorRowCol(row: Int, col: Int) {
        _cursorRow = Math.min(row, _rows - 1)
        _cursorCol = Math.min(col, _columns - 1)
        _aboutToAutoWrap = false
    }

    private fun parseArg(b: Byte) {
        if (b >= '0'.toByte() && b <= '9'.toByte()) {
            if (_argIndex < _args.size) {
                val oldValue = _args[_argIndex]
                val thisDigit = b - '0'.toByte()
                var value: Int
                if (oldValue >= 0) {
                    value = oldValue * 10 + thisDigit
                } else {
                    value = thisDigit
                }
                _args[_argIndex] = value
            }
            continueSequence()
        } else if (b == ';'.toByte()) {
            if (_argIndex < _args.size) {
                _argIndex++;
            }
            continueSequence()
        } else {
            unknownSequence(b)
        }
    }

    private fun continueSequence() {
        _continueSequence = true
    }

    private fun continueSequence(state: Int) {
        _escapeState = state
        _continueSequence = true
    }

    private fun getArg0(defaultValue: Int): Int = getArg(0, defaultValue, true)

    private fun getArg1(defaultValue: Int): Int = getArg(1, defaultValue, true)

    private fun getArg(index: Int, defaultValue: Int, treatZeroAsDefault: Boolean): Int {
        var result = _args[index]
        if (result < 0 || (result == 0 && treatZeroAsDefault)) {
            result = defaultValue
        }
        return result
    }

    private fun setHorizontalVerticalPosition() = setCursorPosition(getArg1(1) - 1, getArg0(1) - 1)

    private fun doEscLeftSquareBracket(b: Byte) {
        when (b) {
            '@'.toByte() -> {
                val charsAfterCursor = _columns - _cursorCol
                val charsToInsert = Math.min(getArg0(1), charsAfterCursor)
                val charsToMove = charsAfterCursor - charsToInsert
                _screen?.blockCopy(_cursorCol, _cursorRow, charsToMove, 1, _cursorCol + charsToInsert, _cursorRow)
                blockClear(_cursorCol, _cursorRow, charsToInsert)
            }
            'A'.toByte() -> setCursorRow(Math.max(_topMargin, _cursorRow - getArg0(1)))
            'B'.toByte() -> setCursorRow(Math.min(_bottomMargin - 1, _cursorRow + getArg0(1)))
            'C'.toByte() -> setCursorCol(Math.min(_columns - 1, _cursorCol + getArg0(1)))
            'D'.toByte() -> setCursorCol(Math.max(0, _cursorCol - getArg0(1)))
            'G'.toByte() -> setCursorCol(Math.min(Math.max(1, getArg0(1)), _columns) - 1)
            'H'.toByte() -> setHorizontalVerticalPosition()
            'J'.toByte() -> {
                when (getArg0(0)) {
                    0 -> {
                        blockClear(_cursorCol, _cursorRow, _columns - _cursorCol);
                        blockClear(0, _cursorRow + 1, _columns, _rows - (_cursorRow + 1))
                    }
                    1 -> {
                        blockClear(0, 0, _columns, _cursorRow)
                        blockClear(0, _cursorRow, _cursorCol + 1)
                    }
                    2 -> blockClear(0, 0, _columns, _rows)
                    else -> unknownSequence(b)
                }
            }
            'K'.toByte() -> {
                when (getArg0(0)) {
                    0 -> blockClear(_cursorCol, _cursorRow, _columns - _cursorCol)
                    1 -> blockClear(0, _cursorRow, _cursorCol + 1)
                    2 -> blockClear(0, _cursorRow, _columns)
                    else -> unknownSequence(b)
                }
            }
            'L'.toByte() -> {
                val linesAfterCursor = _bottomMargin - _cursorRow
                val linesToInsert = Math.min(getArg0(1), linesAfterCursor)
                val linesToMove = linesAfterCursor - linesToInsert
                _screen?.blockCopy(0, _cursorRow, _columns, linesToMove, 0, _cursorRow + linesToInsert)
                blockClear(0, _cursorRow, _columns, linesToInsert)
            }
            'M'.toByte() -> {
                val linesAfterCursor = _bottomMargin - _cursorRow
                val linesToDelete = Math.min(getArg0(1), linesAfterCursor)
                val linesToMove = linesAfterCursor - linesToDelete
                _screen?.blockCopy(0, _cursorRow + linesToDelete, _columns, linesToMove, 0, _cursorRow)
                blockClear(0, _cursorRow + linesToMove, _columns, linesToDelete)
            }
            'P'.toByte() -> {
                val charsAfterCursor = _columns - _cursorCol
                val charsToDelete = Math.min(getArg0(1), charsAfterCursor)
                val charsToMove = charsAfterCursor - charsToDelete
                _screen?.blockCopy(_cursorCol + charsToDelete, _cursorRow, charsToMove, 1, _cursorCol, _cursorRow)
                blockClear(_cursorCol + charsToMove, _cursorRow, charsToDelete)
            }
            'T'.toByte() -> unimplementedSequence(b)
            'X'.toByte() -> blockClear(_cursorCol, _cursorRow, getArg0(0))
            'Z'.toByte() -> setCursorCol(prevTabStop(_cursorCol))
            '?'.toByte() -> continueSequence(ESC_LEFT_SQUARE_BRACKET_QUESTION_MARK)
            'c'.toByte() -> sendDeviceAttributes()
            'd'.toByte() -> setCursorRow(Math.min(Math.max(1, getArg0(1)), _rows) - 1)
            'f'.toByte() -> setHorizontalVerticalPosition()
            'g'.toByte() -> {
                when (getArg0(0)) {
                    0 -> _tabStop!![_cursorCol] = false
                    3 -> {
                        for (i in 0.._columns - 1) {
                            _tabStop!![i] = false
                        }
                    }
                }
            }
            'h'.toByte() -> doSetMode(true)
            'l'.toByte() -> doSetMode(false)
            'm'.toByte() -> selectGraphicRendition()
            'r'.toByte() -> {
                val top = Math.max(0, Math.min(getArg0(1) - 1, _rows - 2));
                val bottom = Math.max(top + 2, Math.min(getArg1(_rows), _rows))
                _topMargin = top
                _bottomMargin = bottom
                setCursorRowCol(_topMargin, 0)
            }
            else -> parseArg(b)
        }
    }

    private fun selectGraphicRendition() {

        var i = 0
        while (i <= _argIndex) {
            var code = _args[i]
            if (code < 0) {
                if (_argIndex > 0) {
                    continue
                } else {
                    code = 0
                }
            }
            if (code == 0) {
                _foreColor = _defaultForeColor
                _backColor = _defaultBackColor
                _effect = TextStyle.fxNormal
            } else if (code == 1) {
                _effect = _effect or TextStyle.fxBold
            } else if (code == 3) {
                _effect = _effect or TextStyle.fxItalic
            } else if (code == 4) {
                _effect = _effect or TextStyle.fxUnderline
            } else if (code == 5) {
                _effect = _effect or TextStyle.fxBlink
            } else if (code == 7) {
                _effect = _effect or TextStyle.fxInverse
            } else if (code == 8) {
                _effect =_effect or TextStyle.fxInvisible
            } else if (code == 10) {
                setAltCharSet(false)
            } else if (code == 11) {
                setAltCharSet(true)
            } else if (code == 22) {
                _effect = _effect and TextStyle.fxBold.inv()
            } else if (code == 23) {
                _effect = _effect and TextStyle.fxItalic.inv()
            } else if (code == 24) {
                _effect = _effect and TextStyle.fxUnderline.inv()
            } else if (code == 25) {
                _effect = _effect and TextStyle.fxBlink.inv()
            } else if (code == 27) {
                _effect = _effect and TextStyle.fxInverse.inv()
            } else if (code == 28) {
                _effect = _effect and TextStyle.fxInvisible.inv()
            } else if (code >= 30 && code <= 37) {
                _foreColor = code - 30
            } else if (code == 38 && i + 2 <= _argIndex && _args[i + 1] == 5) {
                val color = _args[i + 2]
                if (checkColor(color)) {
                    _foreColor = color
                }
                i += 2
            } else if (code == 39) {
                _foreColor = _defaultForeColor;
            } else if (code >= 40 && code <= 47) {
                _backColor = code - 40
            } else if (code == 48 && i + 2 <= _argIndex && _args[i + 1] == 5) {
                _backColor = _args[i + 2]
                val color = _args[i + 2]
                if (checkColor(color)) {
                    _backColor = color
                }
                i += 2
            } else if (code == 49) {
                _backColor = _defaultBackColor
            } else if (code >= 90 && code <= 97) {
                _foreColor = code - 90 + 8
            } else if (code >= 100 && code <= 107) {
                _backColor = code - 100 + 8
            }
            ++i
        }
    }

    private fun checkColor(color: Int): Boolean = isValidColor(color)

    private fun isValidColor(color: Int): Boolean = color >= 0 && color < TextStyle.ciColorLength

    private fun doSetMode(newValue: Boolean) {
        val modeBit = getArg0(0)
        when (modeBit) {
            4 -> _insertMode = newValue
            else -> unknownParameter(modeBit)
        }
    }

    private fun unknownParameter(parameter: Int) {
        logError()
    }

    private fun sendDeviceAttributes() {
        val attributes = byteArrayOf(27.toByte(), '['.toByte(), '?'.toByte(), '1'.toByte(), ';'.toByte(), '2'.toByte(), 'c'.toByte())
        _session?.write(attributes, 0, attributes.size)
    }

    private fun prevTabStop(cursorCol: Int): Int {
        for (i in cursorCol - 1 downTo 0) {
            if (_tabStop!![i]) {
                return i
            }
        }
        return 0
    }

    private fun unimplementedSequence(b: Byte) {
        logError()
        finishSequence()
    }

    private fun logError() {
        finishSequence()
    }

    private fun finishSequence() {
        _escapeState = ESC_NONE
    }

    private fun doEscSelectLeftParen(b: Byte) {
        doSelectCharSet(0, b)
    }

    private fun doEscSelectRightParen(b: Byte) {
        doSelectCharSet(1, b)
    }

    private fun doSelectCharSet(charSetIndex: Int, b: Byte) {
        var charSet: Int
        when (b) {
            'A'.toByte() -> charSet = CHAR_SET_UK
            'B'.toByte() -> charSet = CHAR_SET_ASCII
            '0'.toByte() -> charSet = CHAR_SET_SPECIAL_GRAPHICS
            '1'.toByte() -> charSet = CHAR_SET_ALT_STANDARD
            '2'.toByte() -> charSet = CHAR_SET_ALT_SPECIAL_GRAPICS
            else -> {
                unknownSequence(b)
                return
            }
        }
        _charSet[charSetIndex] = charSet
        computeEffectiveCharSet()
    }

    private fun unknownSequence(b: Byte) {
        logError()
        finishSequence()
    }

    private fun doEscPound(b: Byte) {
        when (b) {
            '8'.toByte() -> _screen?.blockSet(0, 0, _columns, _rows, 'E'.toInt(), getStyle())
            else -> unknownSequence(b)
        }
    }

    private fun doEsc(b: Byte) {
        when (b) {
            '#'.toByte() -> continueSequence(ESC_POUND)
            '('.toByte() -> continueSequence(ESC_SELECT_LEFT_PAREN)
            ')'.toByte() -> continueSequence(ESC_SELECT_RIGHT_PAREN)
            '7'.toByte() -> {
                _savedCursorRow = _cursorRow
                _savedCursorCol = _cursorCol
                _savedEffect = _effect
                _savedDecFlags_DECSC_DECRC = _decFlags and K_DECSC_DECRC_MASK
            }
            '8'.toByte() -> {
                setCursorRowCol(_savedCursorRow, _savedCursorCol)
                _effect = _savedEffect
                _decFlags = (_decFlags and K_DECSC_DECRC_MASK.inv()) or _savedDecFlags_DECSC_DECRC
            }
            'D'.toByte() -> doLinefeed()
            'E'.toByte() -> {
                setCursorCol(0)
                doLinefeed()
            }
            'F'.toByte() -> setCursorRowCol(0, _bottomMargin - 1)
            'H'.toByte() -> _tabStop!![_cursorCol] = true
            'M'.toByte() -> if (_cursorRow <= _topMargin) {
                _screen?.blockCopy(0, _topMargin, _columns, _bottomMargin - (_topMargin + 1), 0, _topMargin + 1)
                blockClear(0, _topMargin, _columns)
            } else {
                _cursorRow--
            }
            'N'.toByte() -> unimplementedSequence(b)
            '0'.toByte() -> unimplementedSequence(b)
            'P'.toByte() -> unimplementedSequence(b)
            'Z'.toByte() -> sendDeviceAttributes()
            '['.toByte() -> continueSequence(ESC_LEFT_SQUARE_BRACKET)
            '='.toByte() -> _mbKeypadApplicationMode = true
            ']'.toByte() -> {
                startCollectingOSCArgs()
                continueSequence(ESC_RIGHT_SQUARE_BRACKET)
            }
            '>'.toByte() -> _mbKeypadApplicationMode = false
            else -> unknownSequence(b)
        }
    }

    private fun startCollectingOSCArgs() {
        _oSCArgLength = 0
    }

    private fun startEscapeSequence(escapeState: Int) {
        _escapeState = escapeState
        _argIndex = 0
        for (j in 0..MAX_ESCAPE_PARAMETERS - 1) {
            _args[j] = -1
        }
    }

    private fun doEscPercent(b: Byte) {
        when (b) {
            '@'.toByte() -> {
                setUTF8Mode(false)
                _UTF8EscapeUsed = true
            }
            'G'.toByte() -> {
                setUTF8Mode(true)
                _UTF8EscapeUsed = true
            }
        }
    }

    private fun setAltCharSet(alternateCharSet: Boolean) {
        _alternateCharSet = alternateCharSet
        computeEffectiveCharSet()
    }

    private fun nextTabStop(cursorCol: Int): Int {
        for (i in cursorCol + 1.._columns - 1) {
            if (_tabStop!![i]) {
                return i
            }
        }
        return _columns - 1
    }

    private fun doEscRightSquareBracket(b: Byte) {
        when (b.toInt()) {
            0x7 -> doOSC()
            0x1b -> continueSequence(ESC_RIGHT_SQUARE_BRACKET_ESC)
            else -> collectOSCArgs(b)
        }
    }

    private fun collectOSCArgs(b: Byte) {
        if (_oSCArgLength < MAX_OSC_STRING_LENGTH) {
            _oSCArg[_oSCArgLength++] = b
            continueSequence()
        } else {
            unknownSequence(b)
        }
    }

    private fun doEscRightSquareBracketEsc(b: Byte) {
        when (b) {
            '\\'.toByte() -> doOSC()
            else -> {
                collectOSCArgs(0x1b)
                collectOSCArgs(b)
                continueSequence(ESC_RIGHT_SQUARE_BRACKET)
            }
        }
    }

    private fun nextOSCInt(delimiter: Int): Int {
        var value = -1
        while (_oSCArgTokenizerIndex < _oSCArgLength) {
            val b = _oSCArg[_oSCArgTokenizerIndex++]
            if (b.toInt() == delimiter) {
                break
            } else if (b >= '0'.toByte() && b <= '9'.toByte()) {
                if (value < 0) {
                    value = 0
                }
                value = value * 10 + b - '0'.toByte()
            } else {
                unknownSequence(b)
            }
        }
        return value
    }

    private fun startTokenizingOSC() {
        _oSCArgTokenizerIndex = 0
    }

    private fun doOSC() {
        startTokenizingOSC()
        val ps = nextOSCInt(';'.toInt())
        when (ps) {
            0, 1, 2 -> changeTitle(ps, nextOSCString(-1))
            else -> unknownParameter(ps)
        }
        finishSequence()
    }

    private fun nextOSCString(delimiter: Int): String? {
        val start = _oSCArgTokenizerIndex
        var end = start
        while (_oSCArgTokenizerIndex < _oSCArgLength) {
            val b = _oSCArg[_oSCArgTokenizerIndex++]
            if (b.toInt() == delimiter) {
                break
            }
            end++
        }
        if (start == end) {
            return ""
        }
        try {
            return String(_oSCArg, start, end - start, Charset.forName("UTF-8"))
        } catch (e: UnsupportedEncodingException) {
            return String(_oSCArg, start, end - start)
        }
    }

    private fun changeTitle(parameter: Int, title: String?) {
        if (parameter == 0 || parameter == 2) {
            _session?.setTitle(title)
        }
    }

    private fun handleUTF8Sequence(b: Byte): Boolean {
        if (_UTF8ToFollow == 0 && (b.toInt() and 0x80) == 0) {
            return false
        }
        if (_UTF8ToFollow > 0) {
            if ((b.toInt() and 0xc0) != 0x80) {
                _UTF8ToFollow = 0
                _UTF8ByteBuffer?.clear()
                emit(UNICODE_REPLACEMENT_CHAR)
                return true
            }
            _UTF8ByteBuffer?.put(b)
            if (--_UTF8ToFollow == 0) {
                val byteBuf = _UTF8ByteBuffer
                val charBuf = _inputCharBuffer
                val decoder = _UTF8Decoder
                byteBuf?.rewind()
                decoder?.reset()
                decoder?.decode(byteBuf, charBuf, true)
                decoder?.flush(charBuf)
                val chars = charBuf!!.array()
                if (chars[0] >= 0x80.toChar() && chars[0] <= 0x9f.toChar()) {
                    process(chars[0].toByte(), false)
                } else {
                    emit(chars)
                }
                byteBuf?.clear()
                charBuf.clear()
            }
        } else {
            if ((b.toInt() and 0xe0) == 0xc0) {
                _UTF8ToFollow = 1
            } else if ((b.toInt() and 0xf0) == 0xe0) {
                _UTF8ToFollow = 2
            } else if ((b.toInt() and 0xf8) == 0xf0) {
                _UTF8ToFollow = 3
            } else {
                emit(UNICODE_REPLACEMENT_CHAR)
                return true
            }
            _UTF8ByteBuffer?.put(b)
        }
        return true
    }

    fun getUTF8Mode(): Boolean = _UTF8Mode

    fun setUTF8ModeUpdateCallback(utf8ModeNotify: UpdateCallback?) {
        _UTF8ModeNotify = utf8ModeNotify
    }

    fun setDefaultUTF8Mode(defaultToUTF8Mode: Boolean) {
        _defaultUTF8Mode = defaultToUTF8Mode
        if (!_UTF8EscapeUsed) {
            setUTF8Mode(defaultToUTF8Mode)
        }
    }

    private fun blockClear(sx: Int, sy: Int, w: Int) {
        blockClear(sx, sy, w, 1)
    }

    private fun blockClear(sx: Int, sy: Int, w: Int, h: Int) {
        _screen?.blockSet(sx, sy, w, h, ' '.toInt(), getStyle())
    }

    private fun computeEffectiveCharSet() {
        val charSet = _charSet[if (_alternateCharSet) 1 else 0]
        _useAlternateCharSet = charSet == CHAR_SET_SPECIAL_GRAPHICS
    }

    private fun setDefaultTabStops() {
        for (i in 0.._columns - 1) {
            _tabStop!![i] = (i and 7) == 0 && i != 0
        }
    }

    fun setColorScheme(scheme: ColorScheme?) {
        _defaultForeColor = TextStyle.ciForeground
        _defaultBackColor = TextStyle.ciBackground
    }

    fun updateSize(columns: Int, rows: Int) {
        if (_rows == rows && _columns == columns) {
            return
        }
        if (columns <= 0) {
            throw IllegalArgumentException("rows:${columns}")
        }
        if (rows <= 0) {
            throw IllegalArgumentException("rows:${rows}")
        }
        val cursor = intArrayOf(_cursorCol, _cursorRow)
        val fastResize = _screen!!.fastResize(columns, rows, cursor)
        var cursorColor: GrowableIntArray? = null
        var charAtCursor: String? = null
        var colors: GrowableIntArray? = null
        var transcriptText: String? = null
        if (!fastResize) {
            cursorColor = GrowableIntArray(1)
            charAtCursor = _screen?.getSelectedText(cursorColor, _cursorCol, _cursorRow, _cursorCol, _cursorRow)
            _screen?.set(_cursorCol, _cursorRow, 27, 0)
            colors = GrowableIntArray(1024)
            transcriptText = _screen?.getTranscriptText(colors)
            _screen?.resize(columns, rows, getStyle())
        }
        if (_rows != rows) {
            _rows = rows
            _topMargin = 0
            _bottomMargin = _rows
        }
        if (_columns != columns) {
            val oldColumns = _columns
            _columns = columns
            val oldTabStop = _tabStop
            _tabStop = BooleanArray(_columns)
            val toTransfer = Math.min(oldColumns, columns)
            System.arraycopy(oldTabStop, 0, _tabStop, 0, toTransfer)
        }
        if (fastResize) {
            if (cursor[0] >= 0 && cursor[1] >= 0) {
                _cursorCol = cursor[0]
                _cursorRow = cursor[1]
            } else {
                _cursorCol = 0
                _cursorRow = 0
            }
            return
        }
        _cursorRow = 0
        _cursorCol = 0
        _aboutToAutoWrap = false
        var newCursorRow = -1
        var newCursorCol = -1
        var newCursorTranscriptPos = -1
        var end = transcriptText!!.length - 1
        while ((end >= 0) && transcriptText[end] == '\n') {
            end--
        }
        var c: Char
        var cLow: Char
        var colorOffset = 0
        var i = 0
        while (i <= end) {
            c = transcriptText[i]
            val style = colors!!.at(i - colorOffset)
            if (Character.isHighSurrogate(c)) {
                cLow = transcriptText[++i]
                emit(Character.toCodePoint(c, cLow), style)
                ++colorOffset
            } else if (c == '\n') {
                setCursorCol(0)
                doLinefeed()
            } else if (c == 27.toChar()) {
                newCursorRow = _cursorRow
                newCursorCol = _cursorCol
                newCursorTranscriptPos = _screen!!.getActiveRows()
                if (charAtCursor != null && charAtCursor.length > 0) {
                    val encodedCursorColor = cursorColor!!.at(0)
                    emit(charAtCursor.toCharArray(), 0, charAtCursor.length, encodedCursorColor)
                }
            } else {
                emit(c.toInt(), style)
            }
            ++i
        }
        if (newCursorRow != -1 && newCursorCol != -1) {
            _cursorRow = newCursorRow
            _cursorCol = newCursorCol
            val scrollCount = _screen!!.getActiveRows() - newCursorTranscriptPos
            if (scrollCount > 0 && scrollCount <= newCursorRow) {
                _cursorRow -= scrollCount
            } else if (scrollCount > newCursorRow) {
                _cursorRow = 0
                _cursorCol = 0
            }
        }
    }

    private fun doLinefeed() {
        var newCursorRow = _cursorRow + 1
        if (newCursorRow >= _bottomMargin) {
            scroll()
            newCursorRow = _bottomMargin - 1
        }
        setCursorRow(newCursorRow)
    }

    private fun setCursorRow(row: Int) {
        _cursorRow = row
        _aboutToAutoWrap = false
    }

    private fun setCursorCol(col: Int) {
        _cursorCol = col
        _aboutToAutoWrap = false
    }

    private fun getStyle(): Int = TextStyle.encode(getForeColor(), getBackColor(), getEffect())

    private fun getForeColor(): Int = _foreColor

    private fun getBackColor(): Int = _backColor

    private fun getEffect(): Int = _effect

    private fun scroll() {
        _scrollCounter++
        _screen?.scroll(_topMargin, _bottomMargin, getStyle())
    }

    private fun autoWrapEnabled(): Boolean = (_decFlags and K_WRAPAROUND_MODE_MASK) != 0

    private fun emit(c: Int, style: Int) {
        val autoWrap = autoWrapEnabled()
        val width = UnicodeTranscript.charWidth(c)
        if (autoWrap) {
            if (_cursorCol == _columns - 1 && (_aboutToAutoWrap || width == 2)) {
                _screen?.setLineWrap(_cursorRow)
                _cursorCol = 0
                _justWrapped = true
                if (_cursorRow + 1 < _bottomMargin) {
                    _cursorRow++
                } else {
                    scroll()
                }
            }
        }
        // XXX: if (_insertMode & width != 0) {
        if (_insertMode && width != 0) {
            val destCol = _cursorCol + width
            if (destCol < _columns) {
                _screen?.blockCopy(_cursorCol, _cursorRow, _columns - destCol, 1, destCol, _cursorRow)
            }
        }
        if (width == 0) {
            if (_justWrapped) {
                _screen?.set(_columns - _lastEmittedCharWidth, _cursorRow - 1, c, style)
            } else {
                _screen?.set(_cursorCol - _lastEmittedCharWidth, _cursorRow, c, style)
            }
        } else {
            _screen?.set(_cursorCol, _cursorRow, c, style)
            _justWrapped = false
        }
        if (autoWrap) {
            _aboutToAutoWrap = (_cursorCol == _columns - 1)
        }
        _cursorCol = Math.min(_cursorCol + width, _columns - 1)
        if (width > 0) {
            _lastEmittedCharWidth = width
        }
    }

    private fun emit(c: Int) {
        emit(c, getStyle())
    }

    private fun emit(b: Byte) {
        if (_useAlternateCharSet && b < 128) {
            emit(_specialGraphicsCharMap[b.toInt()].toInt())
        } else {
            emit(b.toInt())
        }
    }

    private fun emit(c: CharArray?) {
        if (Character.isHighSurrogate(c!![0])) {
            emit(Character.toCodePoint(c[0], c[1]))
        } else {
            emit(c[0].toInt())
        }
    }

    private fun emit(c: CharArray?, offset: Int, length: Int, style: Int) {
        var i = offset
        while (i < length) {
            if (c!![i] == 0.toChar()) {
                break
            }
            if (Character.isHighSurrogate(c[i])) {
                emit(Character.toCodePoint(c[i], c[i + 1]), style)
                ++i
            } else {
                emit(c[i].toInt(), style)
            }
            ++i
        }
    }

    fun getCursorRow(): Int = _cursorRow

    fun getCursorCol(): Int = _cursorCol

    fun getReverseVideo(): Boolean = (_decFlags and K_REVERSE_VIDEO_MASK) != 0

    fun getShowCursor(): Boolean = (_decFlags and K_SHOW_CURSOR_MASK) != 0

    fun getKeypadApplicationMode(): Boolean = _mbKeypadApplicationMode

    fun getScrollCounter(): Int = _scrollCounter

    fun clearScrollCounter() {
        _scrollCounter = 0
    }

    fun getSelectedText(x1: Int, y1: Int, x2: Int, y2: Int): String? = _screen?.getSelectedText(x1, y1, x2, y2)
}