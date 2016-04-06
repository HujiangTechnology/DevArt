package com.hujiang.devart.component.flip

import android.graphics.Canvas
import android.widget.EdgeEffect

/**
 * Created by rarnu on 4/6/16.
 */
class GlowOverFlipper: OverFlipper {

    private var _topEdgeEffect: EdgeEffect? = null
    private var _bottomEdgeEffect: EdgeEffect? = null
    private var _flipView: FlipView? = null
    private var _totalOverFlip = 0.0f

    constructor(v: FlipView?) {
        _flipView = v
        _topEdgeEffect = EdgeEffect(v?.context)
        _bottomEdgeEffect = EdgeEffect(v?.context)
    }

    override fun calculate(flipDistance: Float, minFlipDistance: Float, maxFlipDistance: Float): Float {
        val deltaOverFlip = flipDistance - (if (flipDistance < 0) minFlipDistance else maxFlipDistance)
        _totalOverFlip += deltaOverFlip
        if (deltaOverFlip > 0) {
            _bottomEdgeEffect?.onPull(deltaOverFlip / (if (_flipView!!.isFlippingVertically()) _flipView!!.height else _flipView!!.width))
        } else if (deltaOverFlip < 0) {
            _topEdgeEffect?.onPull(-deltaOverFlip / (if (_flipView!!.isFlippingVertically()) _flipView!!.height else _flipView!!.width))
        }
        return if (flipDistance < 0) minFlipDistance else maxFlipDistance
    }

    override fun draw(c: Canvas?): Boolean = drawTopEdgeEffect(c) or drawBottomEdgeEffect(c)

    private fun drawTopEdgeEffect(canvas: Canvas?): Boolean {
        var needsMoreDrawing = false
        if (!_topEdgeEffect!!.isFinished) {
            canvas?.save()
            if (_flipView!!.isFlippingVertically()) {
                _topEdgeEffect?.setSize(_flipView!!.width, _flipView!!.height)
                canvas?.rotate(0.0f)
            } else {
                _topEdgeEffect?.setSize(_flipView!!.height, _flipView!!.width)
                canvas?.rotate(270.0f)
                canvas?.translate(-_flipView!!.height.toFloat(), 0.0f)
            }
            needsMoreDrawing = _topEdgeEffect!!.draw(canvas)
            canvas?.restore()
        }
        return needsMoreDrawing
    }

    private fun drawBottomEdgeEffect(canvas: Canvas?): Boolean {
        var needsMoreDrawing = false
        if (!_bottomEdgeEffect!!.isFinished) {
            canvas?.save()
            if (_flipView!!.isFlippingVertically()) {
                _bottomEdgeEffect?.setSize(_flipView!!.width, _flipView!!.height)
                canvas?.rotate(180.0f)
                canvas?.translate(-_flipView!!.width.toFloat(), -_flipView!!.height.toFloat())
            } else {
                _bottomEdgeEffect?.setSize(_flipView!!.height, _flipView!!.width)
                canvas?.rotate(90.0f)
                canvas?.translate(0.0f, -_flipView!!.width.toFloat())
            }
            needsMoreDrawing = _bottomEdgeEffect!!.draw(canvas)
            canvas?.restore()
        }
        return needsMoreDrawing
    }

    override fun overFlipEnded() {
        _topEdgeEffect?.onRelease()
        _bottomEdgeEffect?.onRelease()
        _totalOverFlip = 0.0f
    }

    override fun getTotalOverFlip(): Float = _totalOverFlip
}