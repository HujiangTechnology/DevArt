package com.hujiang.devart.component.cropper

import android.graphics.RectF

/**
 * Created by rarnu on 4/26/16.
 */
class CenterHandleHelper: HandleHelper {

    constructor(): super(null, null)

    override fun updateCropWindow(x: Float, y: Float, targetAspectRatio: Float, imageRect: RectF, snapRadius: Float) {
        updateCropWindow(x, y, imageRect, snapRadius)
    }

    override fun updateCropWindow(x: Float, y: Float, imageRect: RectF, snapRadius: Float) {
        val left = Edge.LEFT.getCoordinate()
        val top = Edge.TOP.getCoordinate()
        val right = Edge.RIGHT.getCoordinate()
        val bottom = Edge.BOTTOM.getCoordinate()
        val currentCenterX = (left + right) / 2
        val currentCenterY = (top + bottom) / 2
        val offsetX = x - currentCenterX
        val offsetY = y - currentCenterY
        Edge.LEFT.offset(offsetX)
        Edge.TOP.offset(offsetY)
        Edge.RIGHT.offset(offsetX)
        Edge.BOTTOM.offset(offsetY)
        if (Edge.LEFT.isOutsideMargin(imageRect, snapRadius)) {
            val offset = Edge.LEFT.snapToRect(imageRect)
            Edge.RIGHT.offset(offset)
        } else if (Edge.RIGHT.isOutsideMargin(imageRect, snapRadius)) {
            val offset = Edge.RIGHT.snapToRect(imageRect)
            Edge.LEFT.offset(offset)
        }
        if (Edge.TOP.isOutsideMargin(imageRect, snapRadius)) {
            val offset = Edge.TOP.snapToRect(imageRect)
            Edge.BOTTOM.offset(offset)
        } else if (Edge.BOTTOM.isOutsideMargin(imageRect, snapRadius)) {
            val offset = Edge.BOTTOM.snapToRect(imageRect)
            Edge.TOP.offset(offset)
        }
    }

}