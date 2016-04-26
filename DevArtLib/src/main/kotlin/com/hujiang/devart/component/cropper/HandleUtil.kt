package com.hujiang.devart.component.cropper

import android.graphics.PointF

/**
 * Created by rarnu on 4/26/16.
 */
object HandleUtil {

    fun getPressedHandle(x: Float, y: Float, left: Float, top: Float, right: Float, bottom: Float, targetRadius: Float): Handle? {

        var closestHandle: Handle? = null
        var closestDistance = Float.POSITIVE_INFINITY
        val distanceToTopLeft = MathUtil.calculateDistance(x, y, left, top)
        if (distanceToTopLeft < closestDistance) {
            closestDistance = distanceToTopLeft
            closestHandle = Handle.TOP_LEFT
        }
        val distanceToTopRight = MathUtil.calculateDistance(x, y, right, top)
        if (distanceToTopRight < closestDistance) {
            closestDistance = distanceToTopRight
            closestHandle = Handle.TOP_RIGHT
        }
        val distanceToBottomLeft = MathUtil.calculateDistance(x, y, left, bottom)
        if (distanceToBottomLeft < closestDistance) {
            closestDistance = distanceToBottomLeft
            closestHandle = Handle.BOTTOM_LEFT
        }
        val distanceToBottomRight = MathUtil.calculateDistance(x, y, right, bottom)
        if (distanceToBottomRight < closestDistance) {
            closestDistance = distanceToBottomRight
            closestHandle = Handle.BOTTOM_RIGHT
        }
        if (closestDistance <= targetRadius) {
            return closestHandle
        }
        if (HandleUtil.isInHorizontalTargetZone(x, y, left, right, top, targetRadius)) {
            return Handle.TOP
        } else if (HandleUtil.isInHorizontalTargetZone(x, y, left, right, bottom, targetRadius)) {
            return Handle.BOTTOM
        } else if (HandleUtil.isInVerticalTargetZone(x, y, left, top, bottom, targetRadius)) {
            return Handle.LEFT
        } else if (HandleUtil.isInVerticalTargetZone(x, y, right, top, bottom, targetRadius)) {
            return Handle.RIGHT
        }
        if (isWithinBounds(x, y, left, top, right, bottom)) {
            return Handle.CENTER
        }
        return null
    }

    fun getOffset(handle: Handle?, x: Float, y: Float, left: Float, top: Float, right: Float, bottom: Float, touchOffsetOutput: PointF) {
        var touchOffsetX = 0.0f
        var touchOffsetY = 0.0f
        when (handle) {
            Handle.TOP_LEFT -> {
                touchOffsetX = left - x
                touchOffsetY = top - y
            }
            Handle.TOP_RIGHT -> {
                touchOffsetX = right - x
                touchOffsetY = top - y
            }
            Handle.BOTTOM_LEFT -> {
                touchOffsetX = left - x
                touchOffsetY = bottom - y
            }
            Handle.BOTTOM_RIGHT -> {
                touchOffsetX = right - x
                touchOffsetY = bottom - y
            }
            Handle.LEFT -> {
                touchOffsetX = left - x
                touchOffsetY = 0.0f
            }
            Handle.TOP -> {
                touchOffsetX = 0.0f
                touchOffsetY = top - y
            }
            Handle.RIGHT -> {
                touchOffsetX = right - x
                touchOffsetY = 0.0f
            }
            Handle.BOTTOM -> {
                touchOffsetX = 0.0f
                touchOffsetY = bottom - y
            }
            Handle.CENTER -> {
                val centerX = (right + left) / 2
                val centerY = (top + bottom) / 2
                touchOffsetX = centerX - x
                touchOffsetY = centerY - y
            }
        }
        touchOffsetOutput.x = touchOffsetX
        touchOffsetOutput.y = touchOffsetY
    }

    private fun isInHorizontalTargetZone(x: Float, y: Float, handleXStart: Float, handleXEnd: Float, handleY: Float, targetRadius: Float): Boolean = x > handleXStart && x < handleXEnd && Math.abs(y - handleY) <= targetRadius

    private fun isInVerticalTargetZone(x: Float, y: Float, handleX: Float, handleYStart: Float, handleYEnd: Float, targetRadius: Float): Boolean = Math.abs(x - handleX) <= targetRadius && y > handleYStart && y < handleYEnd

    private fun isWithinBounds(x: Float, y: Float, left: Float, top: Float, right: Float, bottom: Float): Boolean = x >= left && x <= right && y >= top && y <= bottom




}