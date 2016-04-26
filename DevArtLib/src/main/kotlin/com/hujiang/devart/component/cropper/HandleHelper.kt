package com.hujiang.devart.component.cropper

import android.graphics.RectF

/**
 * Created by rarnu on 4/26/16.
 */
abstract class HandleHelper {

    companion object {
        private val UNFIXED_ASPECT_RATIO_CONSTANT = 1.0f
    }


    private var _horizontalEdge: Edge? = null
    private var _verticalEdge: Edge? = null
    private var _activeEdges: EdgePair? = null

    constructor(horizontalEdge: Edge?, verticalEdge: Edge?) {
        _horizontalEdge = horizontalEdge
        _verticalEdge = verticalEdge
        _activeEdges = EdgePair(_horizontalEdge, _verticalEdge)
    }

    open fun updateCropWindow(x: Float, y: Float, imageRect: RectF, snapRadius: Float) {
        val activeEdges = getActiveEdges()
        val primaryEdge = activeEdges?.primary
        val secondaryEdge = activeEdges?.secondary
        primaryEdge?.adjustCoordinate(x, y, imageRect, snapRadius, UNFIXED_ASPECT_RATIO_CONSTANT)
        secondaryEdge?.adjustCoordinate(x, y, imageRect, snapRadius, UNFIXED_ASPECT_RATIO_CONSTANT)
    }

    fun getActiveEdges(): EdgePair? = _activeEdges

    abstract fun updateCropWindow(x: Float, y: Float, targetAspectRatio: Float, imageRect: RectF, snapRadius: Float)

    fun getActiveEdges(x: Float, y: Float, targetAspectRatio: Float): EdgePair? {
        val potentialAspectRatio = getAspectRatio(x, y)
        if (potentialAspectRatio > targetAspectRatio) {
            _activeEdges?.primary = _verticalEdge
            _activeEdges?.secondary = _horizontalEdge
        } else {
            _activeEdges?.primary = _horizontalEdge
            _activeEdges?.secondary = _verticalEdge
        }
        return _activeEdges
    }

    private fun getAspectRatio(x: Float, y: Float): Float {
        val left = if (_verticalEdge == Edge.LEFT) x else Edge.LEFT.getCoordinate()
        val top = if (_horizontalEdge == Edge.TOP) y else Edge.TOP.getCoordinate()
        val right = if (_verticalEdge == Edge.RIGHT) x else Edge.RIGHT.getCoordinate()
        val bottom = if  (_horizontalEdge == Edge.BOTTOM) y else Edge.BOTTOM.getCoordinate()
        return AspectRatioUtil.calculateAspectRatio(left, top, right, bottom)
    }
}