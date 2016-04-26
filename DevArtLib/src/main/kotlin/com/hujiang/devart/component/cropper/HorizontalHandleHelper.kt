package com.hujiang.devart.component.cropper

import android.graphics.RectF

/**
 * Created by rarnu on 4/26/16.
 */
class HorizontalHandleHelper: HandleHelper {

    private var _edge: Edge? = null

    constructor(edge: Edge?):super(edge, null) {
        _edge = edge
    }

    override fun updateCropWindow(x: Float, y: Float, targetAspectRatio: Float, imageRect: RectF, snapRadius: Float) {
        _edge?.adjustCoordinate(x, y, imageRect, snapRadius, targetAspectRatio)
        var left = Edge.LEFT.getCoordinate()
        var right = Edge.RIGHT.getCoordinate()
        val targetWidth = AspectRatioUtil.calculateWidth(Edge.getHeight(), targetAspectRatio)
        val difference = targetWidth - Edge.getWidth()
        val halfDifference = difference / 2
        left -= halfDifference
        right += halfDifference
        Edge.LEFT.setCoordinate(left)
        Edge.RIGHT.setCoordinate(right)
        if (Edge.LEFT.isOutsideMargin(imageRect, snapRadius) && !_edge!!.isNewRectangleOutOfBounds(Edge.LEFT, imageRect, targetAspectRatio)) {
            val offset = Edge.LEFT.snapToRect(imageRect)
            Edge.RIGHT.offset(-offset)
            _edge?.adjustCoordinate(targetAspectRatio)
        }
        if (Edge.RIGHT.isOutsideMargin(imageRect, snapRadius) && !_edge!!.isNewRectangleOutOfBounds(Edge.RIGHT, imageRect, targetAspectRatio)) {
            val offset = Edge.RIGHT.snapToRect(imageRect)
            Edge.LEFT.offset(-offset)
            _edge?.adjustCoordinate(targetAspectRatio)
        }
    }

}