package com.hujiang.devart.component.cropper

import android.graphics.RectF

/**
 * Created by rarnu on 4/26/16.
 */
class VerticalHandleHelper: HandleHelper {

    private var _edge: Edge? = null

    constructor(edge: Edge?): super(null, edge) {
        _edge =  edge
    }

    override fun updateCropWindow(x: Float, y: Float, targetAspectRatio: Float, imageRect: RectF, snapRadius: Float) {
        _edge?.adjustCoordinate(x, y, imageRect, snapRadius, targetAspectRatio)
        var top = Edge.TOP.getCoordinate()
        var bottom = Edge.BOTTOM.getCoordinate()
        val targetHeight = AspectRatioUtil.calculateHeight(Edge.getWidth(), targetAspectRatio)
        val difference = targetHeight - Edge.getHeight()
        val halfDifference = difference / 2
        top -= halfDifference
        bottom += halfDifference
        Edge.TOP.setCoordinate(top)
        Edge.BOTTOM.setCoordinate(bottom)
        if (Edge.TOP.isOutsideMargin(imageRect, snapRadius) && !_edge!!.isNewRectangleOutOfBounds(Edge.TOP, imageRect, targetAspectRatio)) {
            val offset = Edge.TOP.snapToRect(imageRect)
            Edge.BOTTOM.offset(-offset)
            _edge?.adjustCoordinate(targetAspectRatio)
        }
        if (Edge.BOTTOM.isOutsideMargin(imageRect, snapRadius) && !_edge!!.isNewRectangleOutOfBounds(Edge.BOTTOM, imageRect, targetAspectRatio)) {
            val offset = Edge.BOTTOM.snapToRect(imageRect)
            Edge.TOP.offset(-offset)
            _edge?.adjustCoordinate(targetAspectRatio)
        }
    }
}