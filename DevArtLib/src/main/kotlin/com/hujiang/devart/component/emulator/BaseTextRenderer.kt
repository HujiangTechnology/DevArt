package com.hujiang.devart.component.emulator

import android.graphics.*
import org.apache.http.impl.auth.BasicScheme

/**
 * Created by rarnu on 4/10/16.
 */
abstract class BaseTextRenderer: TextRenderer {

    companion object {
        val defaultColorScheme = ColorScheme(0xffcccccc.toInt(), 0xff000000.toInt())
        val sXterm256Paint = intArrayOf(0xff000000.toInt(), 0xffcd0000.toInt(),
            0xff00cd00.toInt(), 0xffcdcd00.toInt(), 0xff0000ee.toInt(), 0xffcd00cd.toInt(), 0xff00cdcd.toInt(),
            0xffe5e5e5.toInt(), 0xff7f7f7f.toInt(), 0xffff0000.toInt(), 0xff00ff00.toInt(), 0xffffff00.toInt(),
            0xff5c5cff.toInt(), 0xffff00ff.toInt(), 0xff00ffff.toInt(), 0xffffffff.toInt(), 0xff000000.toInt(),
            0xff00005f.toInt(), 0xff000087.toInt(), 0xff0000af.toInt(), 0xff0000d7.toInt(), 0xff0000ff.toInt(),
            0xff005f00.toInt(), 0xff005f5f.toInt(), 0xff005f87.toInt(), 0xff005faf.toInt(), 0xff005fd7.toInt(),
            0xff005fff.toInt(), 0xff008700.toInt(), 0xff00875f.toInt(), 0xff008787.toInt(), 0xff0087af.toInt(),
            0xff0087d7.toInt(), 0xff0087ff.toInt(), 0xff00af00.toInt(), 0xff00af5f.toInt(), 0xff00af87.toInt(),
            0xff00afaf.toInt(), 0xff00afd7.toInt(), 0xff00afff.toInt(), 0xff00d700.toInt(), 0xff00d75f.toInt(),
            0xff00d787.toInt(), 0xff00d7af.toInt(), 0xff00d7d7.toInt(), 0xff00d7ff.toInt(), 0xff00ff00.toInt(),
            0xff00ff5f.toInt(), 0xff00ff87.toInt(), 0xff00ffaf.toInt(), 0xff00ffd7.toInt(), 0xff00ffff.toInt(),
            0xff5f0000.toInt(), 0xff5f005f.toInt(), 0xff5f0087.toInt(), 0xff5f00af.toInt(), 0xff5f00d7.toInt(),
            0xff5f00ff.toInt(), 0xff5f5f00.toInt(), 0xff5f5f5f.toInt(), 0xff5f5f87.toInt(), 0xff5f5faf.toInt(),
            0xff5f5fd7.toInt(), 0xff5f5fff.toInt(), 0xff5f8700.toInt(), 0xff5f875f.toInt(), 0xff5f8787.toInt(),
            0xff5f87af.toInt(), 0xff5f87d7.toInt(), 0xff5f87ff.toInt(), 0xff5faf00.toInt(), 0xff5faf5f.toInt(),
            0xff5faf87.toInt(), 0xff5fafaf.toInt(), 0xff5fafd7.toInt(), 0xff5fafff.toInt(), 0xff5fd700.toInt(),
            0xff5fd75f.toInt(), 0xff5fd787.toInt(), 0xff5fd7af.toInt(), 0xff5fd7d7.toInt(), 0xff5fd7ff.toInt(),
            0xff5fff00.toInt(), 0xff5fff5f.toInt(), 0xff5fff87.toInt(), 0xff5fffaf.toInt(), 0xff5fffd7.toInt(),
            0xff5fffff.toInt(), 0xff870000.toInt(), 0xff87005f.toInt(), 0xff870087.toInt(), 0xff8700af.toInt(),
            0xff8700d7.toInt(), 0xff8700ff.toInt(), 0xff875f00.toInt(), 0xff875f5f.toInt(), 0xff875f87.toInt(),
            0xff875faf.toInt(), 0xff875fd7.toInt(), 0xff875fff.toInt(), 0xff878700.toInt(), 0xff87875f.toInt(),
            0xff878787.toInt(), 0xff8787af.toInt(), 0xff8787d7.toInt(), 0xff8787ff.toInt(), 0xff87af00.toInt(),
            0xff87af5f.toInt(), 0xff87af87.toInt(), 0xff87afaf.toInt(), 0xff87afd7.toInt(), 0xff87afff.toInt(),
            0xff87d700.toInt(), 0xff87d75f.toInt(), 0xff87d787.toInt(), 0xff87d7af.toInt(), 0xff87d7d7.toInt(),
            0xff87d7ff.toInt(), 0xff87ff00.toInt(), 0xff87ff5f.toInt(), 0xff87ff87.toInt(), 0xff87ffaf.toInt(),
            0xff87ffd7.toInt(), 0xff87ffff.toInt(), 0xffaf0000.toInt(), 0xffaf005f.toInt(), 0xffaf0087.toInt(),
            0xffaf00af.toInt(), 0xffaf00d7.toInt(), 0xffaf00ff.toInt(), 0xffaf5f00.toInt(), 0xffaf5f5f.toInt(),
            0xffaf5f87.toInt(), 0xffaf5faf.toInt(), 0xffaf5fd7.toInt(), 0xffaf5fff.toInt(), 0xffaf8700.toInt(),
            0xffaf875f.toInt(), 0xffaf8787.toInt(), 0xffaf87af.toInt(), 0xffaf87d7.toInt(), 0xffaf87ff.toInt(),
            0xffafaf00.toInt(), 0xffafaf5f.toInt(), 0xffafaf87.toInt(), 0xffafafaf.toInt(), 0xffafafd7.toInt(),
            0xffafafff.toInt(), 0xffafd700.toInt(), 0xffafd75f.toInt(), 0xffafd787.toInt(), 0xffafd7af.toInt(),
            0xffafd7d7.toInt(), 0xffafd7ff.toInt(), 0xffafff00.toInt(), 0xffafff5f.toInt(), 0xffafff87.toInt(),
            0xffafffaf.toInt(), 0xffafffd7.toInt(), 0xffafffff.toInt(), 0xffd70000.toInt(), 0xffd7005f.toInt(),
            0xffd70087.toInt(), 0xffd700af.toInt(), 0xffd700d7.toInt(), 0xffd700ff.toInt(), 0xffd75f00.toInt(),
            0xffd75f5f.toInt(), 0xffd75f87.toInt(), 0xffd75faf.toInt(), 0xffd75fd7.toInt(), 0xffd75fff.toInt(),
            0xffd78700.toInt(), 0xffd7875f.toInt(), 0xffd78787.toInt(), 0xffd787af.toInt(), 0xffd787d7.toInt(),
            0xffd787ff.toInt(), 0xffd7af00.toInt(), 0xffd7af5f.toInt(), 0xffd7af87.toInt(), 0xffd7afaf.toInt(),
            0xffd7afd7.toInt(), 0xffd7afff.toInt(), 0xffd7d700.toInt(), 0xffd7d75f.toInt(), 0xffd7d787.toInt(),
            0xffd7d7af.toInt(), 0xffd7d7d7.toInt(), 0xffd7d7ff.toInt(), 0xffd7ff00.toInt(), 0xffd7ff5f.toInt(),
            0xffd7ff87.toInt(), 0xffd7ffaf.toInt(), 0xffd7ffd7.toInt(), 0xffd7ffff.toInt(), 0xffff0000.toInt(),
            0xffff005f.toInt(), 0xffff0087.toInt(), 0xffff00af.toInt(), 0xffff00d7.toInt(), 0xffff00ff.toInt(),
            0xffff5f00.toInt(), 0xffff5f5f.toInt(), 0xffff5f87.toInt(), 0xffff5faf.toInt(), 0xffff5fd7.toInt(),
            0xffff5fff.toInt(), 0xffff8700.toInt(), 0xffff875f.toInt(), 0xffff8787.toInt(), 0xffff87af.toInt(),
            0xffff87d7.toInt(), 0xffff87ff.toInt(), 0xffffaf00.toInt(), 0xffffaf5f.toInt(), 0xffffaf87.toInt(),
            0xffffafaf.toInt(), 0xffffafd7.toInt(), 0xffffafff.toInt(), 0xffffd700.toInt(), 0xffffd75f.toInt(),
            0xffffd787.toInt(), 0xffffd7af.toInt(), 0xffffd7d7.toInt(), 0xffffd7ff.toInt(), 0xffffff00.toInt(),
            0xffffff5f.toInt(), 0xffffff87.toInt(), 0xffffffaf.toInt(), 0xffffffd7.toInt(), 0xffffffff.toInt(),
            0xff080808.toInt(), 0xff121212.toInt(), 0xff1c1c1c.toInt(), 0xff262626.toInt(), 0xff303030.toInt(),
            0xff3a3a3a.toInt(), 0xff444444.toInt(), 0xff4e4e4e.toInt(), 0xff585858.toInt(), 0xff626262.toInt(),
            0xff6c6c6c.toInt(), 0xff767676.toInt(), 0xff808080.toInt(), 0xff8a8a8a.toInt(), 0xff949494.toInt(),
            0xff9e9e9e.toInt(), 0xffa8a8a8.toInt(), 0xffb2b2b2.toInt(), 0xffbcbcbc.toInt(), 0xffc6c6c6.toInt(),
            0xffd0d0d0.toInt(), 0xffdadada.toInt(), 0xffe4e4e4.toInt(), 0xffeeeeee.toInt())
        private val sCursorColor = 0xff808080.toInt()
        private val mScaleType = Matrix.ScaleToFit.FILL

        private fun cloneDefaultColors(): IntArray? {
            val length = sXterm256Paint.size
            val clone = IntArray(TextStyle.ciColorLength)
            System.arraycopy(sXterm256Paint, 0, clone, 0, length)
            return clone
        }
    }

    private var _cursorPaint: Paint? = null
    private var _cursorStrokePaint: Paint? = null
    private var _shiftCursor: Path? = null
    private var _altCursor: Path? = null
    private var _ctrlCursor: Path? = null
    private var _fnCursor: Path? = null
    protected var _reverseVideo = false
    protected var _palette: IntArray? = null
    private var _tempSrc: RectF? = null
    private var _tempDst: RectF? = null
    private var _scaleMatrix: Matrix? = null
    private var _lastCharWidth = 0.0f
    private var _lastCharHeight = 0.0f

    @Suppress("DEPRECATION")
    constructor(scheme: ColorScheme?) {
        var nscheme = scheme
        if (nscheme == null) {
            nscheme = defaultColorScheme
        }
        setDefaultColors(nscheme.getForeColor(), nscheme.getBackColor())
        _cursorPaint = Paint()
        _cursorPaint?.color = sCursorColor
        _cursorPaint?.xfermode = PixelXorXfermode(sCursorColor.inv())
        _cursorPaint?.isAntiAlias = true
        _cursorStrokePaint = Paint(_cursorPaint)
        _cursorStrokePaint?.strokeWidth = 0.1f
        _cursorStrokePaint?.style = Paint.Style.STROKE
        _shiftCursor = Path()
        _shiftCursor?.lineTo(0.5f, 0.33f)
        _shiftCursor?.lineTo(1.0f, 0.0f)
        _altCursor = Path()
        _altCursor?.moveTo(0.0f, 1.0f)
        _altCursor?.lineTo(0.5f, 0.66f)
        _altCursor?.lineTo(1.0f, 1.0f)
        _ctrlCursor = Path()
        _ctrlCursor?.moveTo(0.0f, 0.25f)
        _ctrlCursor?.lineTo(1.0f, 0.5f)
        _ctrlCursor?.lineTo(0.0f, 0.75f)
        _fnCursor = Path()
        _fnCursor?.moveTo(1.0f, 0.25f)
        _fnCursor?.lineTo(0.0f, 0.5f)
        _fnCursor?.lineTo(1.0f, 0.75f)
        _tempSrc = RectF()
        _tempSrc?.set(0.0f, 0.0f, 1.0f, 1.0f)
        _tempDst = RectF()
        _scaleMatrix = Matrix()
    }

    override fun setReverseVideo(reverseVideo: Boolean) {
        _reverseVideo = reverseVideo
    }

    private fun setDefaultColors(forePaintColor: Int, backPaintColor: Int) {
        _palette = cloneDefaultColors()
        _palette!![TextStyle.ciForeground] = forePaintColor
        _palette!![TextStyle.ciBackground] = backPaintColor
        _palette!![TextStyle.ciCursor] = sCursorColor
    }

    protected fun drawCursorImp(canvas: Canvas?, x: Float, y: Float, charWidth: Float, charHeight: Float, cursorMode: Int) {
        if (charWidth != _lastCharWidth || charHeight != _lastCharHeight) {
            _lastCharWidth = charWidth
            _lastCharHeight = charHeight
            _tempDst?.set(0.0f, 0.0f, charWidth, charHeight)
            _scaleMatrix?.setRectToRect(_tempSrc, _tempDst, mScaleType)
        }
        canvas?.save()
        canvas?.translate(x, y - charHeight)
        canvas?.clipRect(0.0f, 0.0f, charWidth, charHeight)
        canvas?.drawPaint(_cursorPaint)
        if (cursorMode != 0) {
            canvas?.concat(_scaleMatrix)
            drawCursorHelper(canvas, _shiftCursor, cursorMode, TextRenderer.MODE_SHIFT_SHIFT)
            drawCursorHelper(canvas, _altCursor, cursorMode, TextRenderer.MODE_ALT_SHIFT)
            drawCursorHelper(canvas, _ctrlCursor, cursorMode, TextRenderer.MODE_CTRL_SHIFT)
            drawCursorHelper(canvas, _fnCursor, cursorMode, TextRenderer.MODE_FN_SHIFT)
        }
        canvas?.restore()
    }

    private fun drawCursorHelper(canvas: Canvas?, path: Path?, mode: Int, shift: Int) {
        when ((mode shr shift) and TextRenderer.MODE_MASK) {
            TextRenderer.MODE_ON -> canvas?.drawPath(path, _cursorStrokePaint)
            TextRenderer.MODE_LOCKED -> canvas?.drawPath(path, _cursorPaint)
        }
    }
}