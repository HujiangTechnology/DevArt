package com.hujiang.devart.component.emulator

import android.content.Context
import android.graphics.Canvas
import android.graphics.Paint
import android.os.Build
import android.os.Bundle
import android.os.Handler
import android.text.TextUtils
import android.util.AttributeSet
import android.util.DisplayMetrics
import android.view.GestureDetector
import android.view.KeyEvent
import android.view.MotionEvent
import android.view.View
import android.view.inputmethod.*
import android.widget.Scroller
import java.io.IOException

/**
 * Created by rarnu on 4/11/16.
 */
class EmulatorView: View, GestureDetector.OnGestureListener {

    companion object {
        private val CURSOR_BLINK_PERIOD = 1000
        private val SELECT_TEXT_OFFSET_Y = -40
        private val sTrapAltAndMeta = Build.MODEL.contains("Transformer TF101")
    }

    private val _handler = Handler()
    private var _knownSize = false
    private var _deferInit = false
    private var _visibleWidth = 0
    private var _visibleHeight = 0
    private var _termSession: TermSession? = null
    private var _transcriptScreen: TranscriptScreen? = null
    private var _characterWidth = 0.0f
    private var _characterHeight = 0
    private var _topOfScreenMargin = 0
    private var _textRenderer: TextRenderer? = null
    private var _textSize = 10
    private var _cursorBlink = 0
    private var _colorScheme = BaseTextRenderer.defaultColorScheme
    private var _foregroundPaint: Paint? = null
    private var _backgroundPaint: Paint? = null
    private var _useCookedIme = false
    private var _emulator: TerminalEmulator? = null
    private var _rows = 0
    private var _columns = 0
    private var _visibleColumns = 0
    private var _topRow = 0
    private var _leftColumn = 0
    private var _cursorVisible = true
    private var _isSelectingText = false
    private var _backKeySendsCharacter = false
    private var _controlKeyCode = 0
    private var _fnKeyCode = 0
    private var _isControlKeySent = false
    private var _isFnKeySent = false
    private var _density = 0.0f
    private var _scaledDensity = 0.0f
    private var _selXAnchor = -1
    private var _selYAnchor = -1
    private var _selX1 = -1
    private var _selY1 = -1
    private var _selX2 = -1
    private var _selY2 = -1
    private var _blinkCursor = object : Runnable {
        override fun run() {
            if (_cursorBlink != 0) {
                _cursorVisible = !_cursorVisible
                _handler.postDelayed(this, CURSOR_BLINK_PERIOD.toLong())
            } else {
                _cursorVisible = true
            }
            invalidate()
        }
    }
    private var _gestureDetector: GestureDetector? = null
    private var _extGestureListener: GestureDetector.OnGestureListener? = null
    private var _scroller: Scroller? = null
    private var _flingRunner = object : Runnable {
        override fun run() {
            if (_scroller!!.isFinished) {
                return
            }
            val more = _scroller!!.computeScrollOffset()
            val newTopRow = _scroller!!.currY
            if (newTopRow != _topRow) {
                _topRow = newTopRow
                invalidate()
            }
            if (more) {
                post(this)
            }
        }
    }
    private var _scrollRemainder = 0.0f
    private var _keyListener: TermKeyListener? = null
    private var _imeBuffer = ""
    private var _updateNotify = object : UpdateCallback {
        override fun onUpdate() {
            if (_isSelectingText) {
                val rowShift = _emulator!!.getScrollCounter()
                _selY1 -= rowShift
                _selY2 -= rowShift
                _selYAnchor -= rowShift
            }
            _emulator?.clearScrollCounter()
            ensureCursorVisible()
            invalidate()
        }
    }

    constructor(context: Context, session: TermSession, metrics: DisplayMetrics?): super(context) {
        attachSession(session)
        setDensity(metrics)
        commonConstructor(context)
    }

    fun setDensity(metrics: DisplayMetrics?) {
        if (_density == 0.0f) {
            _textSize = (_textSize * metrics!!.density).toInt()
        }
        _density = metrics!!.density
        _scaledDensity = metrics.scaledDensity
    }

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        commonConstructor(context)
    }

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        commonConstructor(context)
    }

    private fun commonConstructor(context: Context) {
        _scroller = Scroller(context)
    }

    private fun ensureCursorVisible() {
        _topRow = 0
        if (_visibleColumns > 0) {
            val cx = _emulator!!.getCursorCol()
            val visibleCursorX = _emulator!!.getCursorCol() - _leftColumn
            if (visibleCursorX < 0) {
                _leftColumn = cx
            } else if (visibleCursorX >= _visibleColumns) {
                _leftColumn = (cx - _visibleColumns) + 1
            }
        }
    }

    fun attachSession(session: TermSession?) {
        _textRenderer = null
        _foregroundPaint = Paint()
        _backgroundPaint = Paint()
        _topRow = 0
        _leftColumn = 0
        _gestureDetector = GestureDetector(context, this)
        isVerticalScrollBarEnabled = true
        isFocusable = true
        isFocusableInTouchMode = true
        _termSession = session
        _keyListener = TermKeyListener(session)
        if (_deferInit) {
            _deferInit = false
            _knownSize = true
            initialize()
        }
    }

    private fun initialize() {
        val session = _termSession
        updateText()
        _transcriptScreen = session?.getTranscriptScreen()
        _emulator = session?.getEmulator()
        session?.setUpdateCallback(_updateNotify)
        requestFocus()
    }

    private fun updateText() {
        val scheme = _colorScheme
        _textRenderer = PaintRenderer(_textSize, scheme)
        _foregroundPaint?.color = scheme.getForeColor()
        _backgroundPaint?.color = scheme.getBackColor()
        _characterWidth = _textRenderer!!.getCharacterWidth()
        _characterHeight = _textRenderer!!.getCharacterHeight()
        updateSize(true)
    }

    private fun updateSize(w: Int, h: Int) {
        _columns = Math.max(1, (w * 1.0f / _characterWidth).toInt())
        _visibleColumns = (_visibleWidth * 1.0f / _characterWidth).toInt()
        _topOfScreenMargin = _textRenderer!!.getTopMargin()
        _rows = Math.max(1, (h - _topOfScreenMargin) / _characterHeight)
        _termSession?.updateSize(_columns, _rows)
        _topRow = 0
        _leftColumn = 0
        invalidate()
    }

    fun updateSize(force: Boolean) {
        if (_knownSize) {
            val w = width
            val h = height
            if (force || w != _visibleWidth || h != _visibleHeight) {
                _visibleWidth = w
                _visibleHeight = h
                updateSize(_visibleWidth, _visibleHeight)
            }
        }
    }

    fun onResume() {
        updateSize(false)
        if (_cursorBlink != 0) {
            _handler.postDelayed(_blinkCursor, CURSOR_BLINK_PERIOD.toLong())
        }
    }

    fun onPause() {
        if (_cursorBlink != 0) {
            _handler.removeCallbacks(_blinkCursor)
        }
    }

    fun setColorScheme(scheme: ColorScheme?) {
        if (scheme == null) {
            _colorScheme = BaseTextRenderer.defaultColorScheme
        } else {
            _colorScheme = scheme
        }
        updateText()
    }

    override fun onCheckIsTextEditor(): Boolean = true

    override fun onCreateInputConnection(outAttrs: EditorInfo?): InputConnection? {
        outAttrs?.inputType = if (_useCookedIme) EditorInfo.TYPE_CLASS_TEXT else EditorInfo.TYPE_NULL
        return object: BaseInputConnection(this, true) {
            private var _cursor = 0
            private var _composingTextStart = 0
            private var _composingTextEnd = 0
            private var _selectedTextStart = 0
            private var _selectedTextEnd = 0

            private fun sendText(text: CharSequence?) {
                val n = text!!.length
                var c: Char
                try {
                    var i = 0
                    while (i < n) {
                        c = text[i]
                        if (Character.isHighSurrogate(c)) {
                            var codePoint: Int
                            if (++i < n) {
                                codePoint = Character.toCodePoint(c, text[i])
                            } else {
                                codePoint = '\ufffd'.toInt()
                            }
                            mapAndSend(codePoint)
                        } else {
                            mapAndSend(c.toInt())
                        }
                        ++i
                    }
                } catch (e: IOException) {
                }
            }
            private fun mapAndSend(c: Int) {
                val result = _keyListener!!.mapControlChar(c)
                if (result < TermKeyListener.KEYCODE_OFFSET) {
                    _termSession?.write(result)
                } else {
                    _keyListener?.handleKeyCode(result - TermKeyListener.KEYCODE_OFFSET, getKeypadApplicationMode())
                }
                clearSpecialKeyStatus()
            }
            override fun beginBatchEdit(): Boolean {
                setImeBuffer("")
                _cursor = 0
                _composingTextStart = 0
                _composingTextEnd = 0
                return true
            }
            override fun clearMetaKeyStates(arg0: Int): Boolean = false

            override fun commitCompletion(arg0: CompletionInfo?): Boolean = false

            override fun endBatchEdit(): Boolean = true

            override fun finishComposingText(): Boolean {
                sendText(_imeBuffer)
                setImeBuffer("")
                _composingTextStart = 0
                _composingTextEnd = 0
                _cursor = 0
                return true
            }

            override fun getCursorCapsMode(reqModes: Int): Int {
                var mode = 0
                if ((reqModes and TextUtils.CAP_MODE_CHARACTERS) != 0) {
                    mode = mode or TextUtils.CAP_MODE_CHARACTERS
                }
                return mode
            }

            override fun getExtractedText(arg0: ExtractedTextRequest?, arg1: Int):  ExtractedText? = null

            override fun getTextAfterCursor(n: Int, flags: Int): CharSequence? {
                val len = Math.min(n, _imeBuffer.length - _cursor)
                if (len <= 0 || _cursor < 0 || _cursor >= _imeBuffer.length) {
                    return ""
                }
                return _imeBuffer.substring(_cursor, _cursor + len)
            }

            override fun getTextBeforeCursor(n: Int, flags: Int): CharSequence? {
                val len = Math.min(n, _cursor)
                if (len <= 0 || _cursor < 0 || _cursor >= _imeBuffer.length) {
                    return ""
                }
                return _imeBuffer.substring(_cursor - len, _cursor)
            }

            override fun performContextMenuAction(arg0: Int): Boolean = true

            override fun performPrivateCommand(arg0: String?, arg1: Bundle?): Boolean = true

            override fun reportFullscreenMode(arg0: Boolean): Boolean = true

            override fun commitCorrection(correctionInfo: CorrectionInfo?): Boolean = true

            override fun commitText(text: CharSequence?, newCursorPosition: Int): Boolean {
                clearComposingText()
                sendText(text)
                setImeBuffer("")
                _cursor = 0
                return true
            }

            private fun clearComposingText() {
                val len = _imeBuffer.length
                if (_composingTextStart > len || _composingTextEnd > len) {
                    _composingTextEnd = 0
                    _composingTextStart = 0
                    return
                }
                setImeBuffer(_imeBuffer.substring(0, _composingTextStart) + _imeBuffer.substring(_composingTextEnd))
                if (_cursor < _composingTextStart) {
                } else if (_cursor < _composingTextEnd) {
                    _cursor = _composingTextStart
                } else {
                    _cursor -= _composingTextEnd - _composingTextStart
                }
                _composingTextEnd = 0
                _composingTextStart = 0
            }

            override fun deleteSurroundingText(leftLength: Int, rightLength: Int): Boolean {
                if (leftLength > 0) {
                    for (i in 0..leftLength - 1) {
                        sendKeyEvent(KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL))
                    }
                } else if ((leftLength == 0) && (rightLength == 0)) {
                    sendKeyEvent(KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL))
                }
                return true
            }

            override fun performEditorAction(actionCode: Int): Boolean {
                if (actionCode == EditorInfo.IME_ACTION_UNSPECIFIED) {
                    sendText("\r")
                }
                return true
            }

            override fun sendKeyEvent(event: KeyEvent?): Boolean {
                dispatchKeyEvent(event)
                return true
            }

            override fun setComposingText(text: CharSequence?, newCursorPosition: Int): Boolean {
                val len = _imeBuffer.length
                if (_composingTextStart > len || _composingTextEnd > len) {
                    return false
                }
                setImeBuffer(_imeBuffer.substring(0, _composingTextStart) + text + _imeBuffer.substring(_composingTextEnd))
                _composingTextEnd = _composingTextStart + text!!.length
                _cursor = if (newCursorPosition > 0) _composingTextEnd + newCursorPosition - 1 else _composingTextStart - newCursorPosition
                return true
            }

            override fun setSelection(start: Int, end: Int): Boolean {
                val length = _imeBuffer.length
                if (start == end && start > 0 && start < length) {
                    _selectedTextStart = 0
                    _selectedTextEnd = 0
                    _cursor = start
                } else if (start < end && start > 0 && end < length) {
                    _selectedTextStart = start
                    _selectedTextEnd = end
                    _cursor = start
                }
                return true
            }

            override fun setComposingRegion(start: Int, end: Int): Boolean {
                if (start < end && start > 0 && end < _imeBuffer.length) {
                    clearComposingText()
                    _composingTextStart = start
                    _composingTextEnd = end
                }
                return true
            }

            override fun getSelectedText(flags: Int): CharSequence? {
                val len = _imeBuffer.length
                if (_selectedTextEnd >= len || _selectedTextStart > _selectedTextEnd) {
                    return ""
                }
                return _imeBuffer.substring(_selectedTextStart, _selectedTextEnd + 1)
            }
        }
    }

    private fun setImeBuffer(buffer: String) {
        if (buffer != _imeBuffer) {
            invalidate()
        }
        _imeBuffer = buffer
    }

    fun getKeypadApplicationMode(): Boolean = _emulator!!.getKeypadApplicationMode()

    private fun clearSpecialKeyStatus() {
        if (_isControlKeySent) {
            _isControlKeySent = false
            _keyListener?.handleControlKey(false)
            invalidate()
        }
        if (_isFnKeySent) {
            _isFnKeySent = false
            _keyListener?.handleFnKey(false)
            invalidate()
        }
    }

    override fun computeVerticalScrollRange(): Int = _transcriptScreen!!.getActiveRows()

    override fun computeVerticalScrollExtent(): Int = _rows

    override fun computeVerticalScrollOffset(): Int = _transcriptScreen!!.getActiveRows() + _topRow - _rows


    override fun onTouchEvent(ev: MotionEvent?): Boolean {
        if (_isSelectingText) {
            return onTouchEventWhileSelectingText(ev)
        } else {
            return _gestureDetector!!.onTouchEvent(ev)
        }
    }

    private fun onTouchEventWhileSelectingText(ev: MotionEvent?): Boolean {
        val action = ev!!.action
        val cx = (ev.x / _characterWidth).toInt()
        val cy = Math.max(0, ((ev.y + SELECT_TEXT_OFFSET_Y * _scaledDensity) / _characterHeight).toInt() + _topRow)
        when (action) {
            MotionEvent.ACTION_DOWN -> {
                _selXAnchor = cx
                _selYAnchor = cy
                _selX1 = cx
                _selY1 = cy
                _selX2 = _selX1
                _selY2 = _selY1
            }
            MotionEvent.ACTION_MOVE, MotionEvent.ACTION_UP -> {
                val minx = Math.min(_selXAnchor, cx)
                val maxx = Math.max(_selXAnchor, cx)
                val miny = Math.min(_selYAnchor, cy)
                val maxy = Math.max(_selYAnchor, cy)
                _selX1 = minx
                _selY1 = miny
                _selX2 = maxx
                _selY2 = maxy
                if (action == MotionEvent.ACTION_UP) {
                    toggleSelectingText()
                }
                invalidate()
            }
            else -> {
                toggleSelectingText()
                invalidate()
            }
        }
        return true
    }

    fun toggleSelectingText() {
        _isSelectingText = !_isSelectingText
        isVerticalScrollBarEnabled = !_isSelectingText
        if (!_isSelectingText) {
            _selX1 = -1
            _selY1 = -1
            _selX2 = -1
            _selY2 = -1
        }
    }

    override fun onDraw(canvas: Canvas?) {
        updateSize(false)
        if (_emulator == null) {
            return
        }
        val w = width
        val h = height
        val reverseVideo = _emulator!!.getReverseVideo()
        _textRenderer?.setReverseVideo(reverseVideo)
        val backgroundPaint = if (reverseVideo) _foregroundPaint else _backgroundPaint
        canvas?.drawRect(0.0f, 0.0f, w.toFloat(), h.toFloat(), backgroundPaint)
        val x = -_leftColumn * _characterWidth
        var y = _characterHeight + _topOfScreenMargin
        val endLine = _topRow + _rows
        val cx = _emulator!!.getCursorCol()
        val cy = _emulator!!.getCursorRow()
        val cursorVisible = _cursorVisible && _emulator!!.getShowCursor()
        var effectiveImeBuffer = _imeBuffer
        val combiningAccent = _keyListener!!.getCombiningAccent()
        if (combiningAccent != 0) {
            effectiveImeBuffer += combiningAccent.toChar().toString()
        }
        val cursorStyle = _keyListener!!.getCursorMode()
        for (i in _topRow..endLine - 1) {
            var cursorX = -1
            if (i == cy && cursorVisible) {
                cursorX = cx
            }
            var selx1 = -1
            var selx2 = -1
            if (i >= _selY1 && i <= _selY2) {
                if (i == _selY1) {
                    selx1 = _selX1
                }
                if (i == _selY2) {
                    selx2 = _selX2
                } else {
                    selx2 = _columns
                }
            }
            _transcriptScreen?.drawText(i, canvas, x, y.toFloat(), _textRenderer, cursorX, selx1, selx2, effectiveImeBuffer, cursorStyle)
            y += _characterHeight
        }
    }

    override fun onKeyDown(keyCode: Int, event: KeyEvent?): Boolean {
        if (handleControlKey(keyCode, true)) {
            return true
        } else if (handleFnKey(keyCode, true)) {
            return true
        } else if (isSystemKey(keyCode, event)) {
            if (!isInterceptedSystemKey(keyCode)) {
                return super.onKeyDown(keyCode, event)
            }
        }
        try {
            val oldCombiningAccent = _keyListener!!.getCombiningAccent()
            val oldCursorMode = _keyListener!!.getCursorMode()
            _keyListener?.keyDown(keyCode, event!!, getKeypadApplicationMode(), TermKeyListener.isEventFromToggleDevice(event))
            if (_keyListener?.getCombiningAccent() != oldCombiningAccent || _keyListener!!.getCursorMode() != oldCursorMode) {
                invalidate()
            }
        } catch (e: IOException) {
        }
        return true
    }

    override fun onKeyUp(keyCode: Int, event: KeyEvent?): Boolean {
        if (handleControlKey(keyCode, false)) {
            return true
        } else if (handleFnKey(keyCode, false)) {
            return true
        } else if (isSystemKey(keyCode, event)) {
            if (!isInterceptedSystemKey(keyCode)) {
                return super.onKeyUp(keyCode, event)
            }
        }
        _keyListener?.keyUp(keyCode, event!!)
        clearSpecialKeyStatus()
        return true
    }

    override fun onKeyPreIme(keyCode: Int, event: KeyEvent?): Boolean {
        if (sTrapAltAndMeta) {
            val altEsc = _keyListener!!.getAltSendsEsc()
            val altOn = (event!!.metaState and KeyEvent.META_ALT_ON) != 0
            val metaOn = (event.metaState and KeyEvent.META_META_ON) != 0
            val altPressed = (keyCode == KeyEvent.KEYCODE_ALT_LEFT) || (keyCode == KeyEvent.KEYCODE_ALT_RIGHT)
            val altActive = _keyListener!!.isAltActive()
            if (altEsc && (altOn || altPressed || altActive || metaOn)) {
                if (event.action == KeyEvent.ACTION_DOWN) {
                    return onKeyDown(keyCode, event)
                } else {
                    return onKeyUp(keyCode, event)
                }
            }
        }
        if (handleHardwareControlKey(keyCode, event)) {
            return true
        }
        if (_keyListener!!.isCtrlActive()) {
            if (event!!.action == KeyEvent.ACTION_DOWN) {
                return onKeyDown(keyCode, event)
            } else {
                return onKeyUp(keyCode, event)
            }
        }
        return super.onKeyPreIme(keyCode, event)
    }

    private fun handleHardwareControlKey(keyCode: Int, event: KeyEvent?): Boolean {
        if (keyCode == TermKeyListener.KEYCODE_CTRL_LEFT || keyCode == TermKeyListener.KEYCODE_CTRL_RIGHT) {
            val down = event!!.action == KeyEvent.ACTION_DOWN
            _keyListener?.handleHardwareControlKey(down)
            invalidate()
            return true
        }
        return false
    }

    private fun isInterceptedSystemKey(keyCode: Int): Boolean = keyCode == KeyEvent.KEYCODE_BACK && _backKeySendsCharacter

    private fun isSystemKey(@Suppress("UNUSED_PARAMETER") keyCode: Int, event: KeyEvent?): Boolean = event!!.isSystem

    private fun handleControlKey(keyCode: Int, down: Boolean): Boolean {
        if (keyCode == _controlKeyCode) {
            _keyListener?.handleControlKey(down)
            invalidate()
            return true
        }
        return false
    }

    private fun handleFnKey(keyCode: Int, down: Boolean): Boolean {
        if (keyCode == _fnKeyCode) {
            _keyListener?.handleFnKey(down)
            invalidate()
            return true
        }
        return false
    }

    override fun onSizeChanged(w: Int, h: Int, oldw: Int, oldh: Int) {
        if (_termSession == null) {
            _deferInit = true
            return
        }
        if (!_knownSize) {
            _knownSize = true
            initialize()
        } else {
            updateSize(false)
        }
    }

    override fun onSingleTapUp(e: MotionEvent?): Boolean {
        if (_extGestureListener != null && _extGestureListener!!.onSingleTapUp(e)) {
            return true
        }
        requestFocus()
        return true
    }

    override fun onDown(e: MotionEvent?): Boolean {
        if (_extGestureListener != null && _extGestureListener!!.onDown(e)) {
            return true
        }
        _scrollRemainder = 0.0f
        return true
    }

    override fun onFling(e1: MotionEvent?, e2: MotionEvent?, velocityX: Float, velocityY: Float): Boolean {
        if (_extGestureListener != null && _extGestureListener!!.onFling(e1, e2, velocityX, velocityY)) {
            return true
        }
        val SCALE = 0.25f
        _scroller?.fling(0, _topRow, -(velocityX * SCALE).toInt(), -(velocityY * SCALE).toInt(), 0, 0, -_transcriptScreen!!.getActiveTranscriptRows(), 0)
        _scrollRemainder = 0.0f
        post(_flingRunner)
        return true
    }

    override fun onScroll(e1: MotionEvent?, e2: MotionEvent?, distanceX: Float, distanceY: Float): Boolean {
        if (_extGestureListener != null && _extGestureListener!!.onScroll(e1, e2, distanceX, distanceY)) {
            return true
        }
        var ndistanceY = distanceY + _scrollRemainder
        val deltaRows = (ndistanceY / _characterHeight).toInt()
        _scrollRemainder = ndistanceY - deltaRows * _characterHeight
        _topRow = Math.min(0, Math.max(-(_transcriptScreen!!.getActiveTranscriptRows()), _topRow + deltaRows))
        invalidate()
        return true
    }

    override fun onShowPress(e: MotionEvent?) {
        _extGestureListener?.onShowPress(e)
    }

    override fun onLongPress(e: MotionEvent?) {
        showContextMenu()
    }

    fun setExtGestureListener(listener: GestureDetector.OnGestureListener?) {
        _extGestureListener = listener
    }

    fun getTermSession(): TermSession? = _termSession

    fun getVisibleWidth(): Int = _visibleWidth

    fun getVisibleHeight(): Int = _visibleHeight

    fun page(delta: Int) {
        _topRow = Math.min(0, Math.max(-(_transcriptScreen!!.getActiveTranscriptRows()), _topRow + _rows * delta))
        invalidate()
    }

    fun pageHorizontal(deltaColumns: Int) {
        _leftColumn = Math.max(0, Math.min(_leftColumn + deltaColumns, _columns - _visibleColumns))
        invalidate()
    }

    fun setTextSize(fontSize: Int) {
        _textSize = (fontSize * _density).toInt()
        updateText()
    }

    fun setCursorStyle(blink: Int) {
        if (blink != 0 && _cursorBlink == 0) {
            _handler.postDelayed(_blinkCursor, CURSOR_BLINK_PERIOD.toLong())
        } else if (blink == 0 && _cursorBlink != 0) {
            _handler.removeCallbacks(_blinkCursor)
        }
        _cursorBlink = blink
    }

    fun setUseCookedIME(useCookedIME: Boolean) {
        _useCookedIme = useCookedIME
    }

    @Suppress("UNUSED_PARAMETER")
    fun onSingleTapConfirmed(e: MotionEvent?) { }

    @Suppress("UNUSED_PARAMETER")
    fun onJumpTapDown(e1: MotionEvent?, e2: MotionEvent?): Boolean {
        _topRow = 0
        invalidate()
        return true
    }

    @Suppress("UNUSED_PARAMETER")
    fun onJumpTapUp(e1: MotionEvent?, e2: MotionEvent?): Boolean {
        _topRow = -_transcriptScreen!!.getActiveTranscriptRows()
        invalidate()
        return true
    }

    fun getSelectingText(): Boolean = _isSelectingText

    fun getSelectedText(): String? = _emulator?.getSelectedText(_selX1, _selY1, _selX2, _selY2)

    fun sendControlKey() {
        _isControlKeySent = true
        _keyListener?.handleControlKey(true)
        invalidate()
    }

    fun sendFnKey() {
        _isFnKeySent = true
        _keyListener?.handleFnKey(true)
        invalidate()
    }

    fun setBackKeyCharacter(keyCode: Int) {
        _keyListener?.setBackKeyCharacter(keyCode)
        _backKeySendsCharacter = keyCode != 0
    }

    fun setAltSendsEsc(flag: Boolean) = _keyListener?.setAltSendsEsc(flag)

    fun setControlKeyCode(keyCode: Int) {
        _controlKeyCode = keyCode
    }

    fun setFnKeyCode(keyCode: Int) {
        _fnKeyCode = keyCode
    }

    fun setTermType(termType: String) = _keyListener?.setTermType(termType)

}