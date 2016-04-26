package com.hujiang.devart.component.cropper

import android.graphics.RectF

/**
 * Created by rarnu on 4/26/16.
 */
class CornerHandleHelper(horizontalEdge: Edge?, verticalEdge: Edge?) : HandleHelper(horizontalEdge, verticalEdge) {


    override fun updateCropWindow(x: Float, y: Float, targetAspectRatio: Float, imageRect: RectF, snapRadius: Float) {
        val activeEdges = getActiveEdges(x, y, targetAspectRatio)
        val primaryEdge = activeEdges?.primary
        val secondaryEdge = activeEdges?.secondary
        primaryEdge?.adjustCoordinate(x, y, imageRect, snapRadius, targetAspectRatio)
        secondaryEdge?.adjustCoordinate(targetAspectRatio)
        if (secondaryEdge!!.isOutsideMargin(imageRect, snapRadius)) {
            secondaryEdge.snapToRect(imageRect)
            primaryEdge?.adjustCoordinate(targetAspectRatio)
        }
    }
}