package com.hujiang.devart.component.emulator

import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.Typeface
import android.util.FloatMath

/**
 * Created by rarnu on 4/11/16.
 */
class PaintRenderer: BaseTextRenderer {

    companion object {
        private val EXAMPLE_CHAR = charArrayOf('X')
    }
    private var _textPaint: Paint? = null
    private var _charWidth = 0.0f
    private var _charHeight = 0
    private var _charAscent = 0
    private var _charDescent = 0

    constructor(fontSize: Int, scheme: ColorScheme): super(scheme) {
        _textPaint = Paint()
        _textPaint?.typeface = Typeface.MONOSPACE
        _textPaint?.isAntiAlias = true
        _textPaint?.textSize = fontSize.toFloat()
        _charHeight = FloatMath.ceil(_textPaint!!.fontSpacing).toInt()
        _charAscent = FloatMath.ceil(_textPaint!!.ascent()).toInt()
        _charDescent = _charHeight + _charAscent
        _charWidth = _textPaint!!.measureText(EXAMPLE_CHAR, 0, 1)
    }


    override fun getCharacterWidth(): Float = _charWidth

    override fun getCharacterHeight(): Int = _charHeight

    override fun getTopMargin(): Int = _charDescent

    override fun drawTextRun(canvas: Canvas?, x: Float, y: Float, lineOffset: Int, runWidth: Int, text: CharArray?, index: Int, count: Int, cursor: Boolean, textStyle: Int) {
        var foreColor = TextStyle.decodeForeColor(textStyle)
        var backColor = TextStyle.decodeBackColor(textStyle)
        val effect = TextStyle.decodeEffect(textStyle)
        val inverse = _reverseVideo xor ((effect and (TextStyle.fxInverse or TextStyle.fxItalic)) != 0)
        if (inverse) {
            val temp = foreColor
            foreColor = backColor
            backColor = temp
        }
        if (cursor) {
            backColor = TextStyle.ciCursor
        }
        _textPaint?.color = _palette!![backColor]
        val left = x + lineOffset * _charWidth
        canvas?.drawRect(left, y + _charAscent - _charDescent, left + runWidth * _charWidth, y, _textPaint)
        val invisible = (effect and TextStyle.fxInvisible) != 0
        if (!invisible) {
            val bold = (effect and (TextStyle.fxBold or TextStyle.fxBlink)) != 0
            val underline = (effect and TextStyle.fxUnderline) != 0
            if (bold) {
                _textPaint?.isFakeBoldText = true
            }
            if (underline) {
                _textPaint?.isUnderlineText = true
            }
            if (foreColor < 8 && bold) {
                _textPaint?.color = _palette!![foreColor + 8]
            } else {
                _textPaint?.color = _palette!![foreColor]
            }
            canvas?.drawText(text, index, count, left, y - _charDescent, _textPaint)
            if (bold) {
                _textPaint?.isFakeBoldText = false
            }
            if (underline) {
                _textPaint?.isUnderlineText = false
            }
        }
    }

    override fun drawCursor(canvas: Canvas?, x: Float, y: Float, lineOffset: Int, cursorMode: Int) {
        val left = x + lineOffset * _charWidth
        drawCursorImp(canvas, left, y, _charWidth, _charHeight.toFloat(), cursorMode)
    }
}