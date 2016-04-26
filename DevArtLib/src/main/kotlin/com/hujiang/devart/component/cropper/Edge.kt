package com.hujiang.devart.component.cropper

import android.graphics.RectF

/**
 * Created by rarnu on 4/26/16.
 */
enum class Edge {

    LEFT, TOP, RIGHT, BOTTOM;

    companion object {
        val MIN_CROP_LENGTH_PX = 40

        fun getWidth(): Float = Edge.RIGHT.getCoordinate() - Edge.LEFT.getCoordinate()
        fun getHeight(): Float = Edge.BOTTOM.getCoordinate() - Edge.TOP.getCoordinate()

        private fun adjustLeft(x: Float, imageRect: RectF, imageSnapRadius: Float, aspectRatio: Float): Float {
            var resultX: Float
            if (x - imageRect.left < imageSnapRadius) {
                resultX = imageRect.left
            } else {
                var resultXHoriz = Float.POSITIVE_INFINITY
                var resultXVert = Float.POSITIVE_INFINITY
                if (x >= Edge.RIGHT.getCoordinate() - MIN_CROP_LENGTH_PX) {
                    resultXHoriz = Edge.RIGHT.getCoordinate() - MIN_CROP_LENGTH_PX
                }
                if (((Edge.RIGHT.getCoordinate() - x) / aspectRatio) <= MIN_CROP_LENGTH_PX) {
                    resultXVert = Edge.RIGHT.getCoordinate() - (MIN_CROP_LENGTH_PX * aspectRatio)
                }
                resultX = Math.min(x, Math.min(resultXHoriz, resultXVert))
            }
            return resultX
        }

        private fun adjustRight(x: Float, imageRect: RectF, imageSnapRadius: Float, aspectRatio: Float): Float {
            var resultX: Float
            if (imageRect.right - x < imageSnapRadius) {
                resultX = imageRect.right
            } else {
                var resultXHoriz = Float.NEGATIVE_INFINITY
                var resultXVert = Float.NEGATIVE_INFINITY
                if (x <= Edge.LEFT.getCoordinate() + MIN_CROP_LENGTH_PX) {
                    resultXHoriz = Edge.LEFT.getCoordinate() + MIN_CROP_LENGTH_PX
                }
                if (((x - Edge.LEFT.getCoordinate()) / aspectRatio) <= MIN_CROP_LENGTH_PX) {
                    resultXVert = Edge.LEFT.getCoordinate() + (MIN_CROP_LENGTH_PX * aspectRatio)
                }
                resultX = Math.max(x, Math.max(resultXHoriz, resultXVert))
            }
            return resultX
        }

        private fun adjustTop(y: Float, imageRect: RectF, imageSnapRadius: Float, aspectRatio: Float): Float {
            var resultY: Float
            if (y - imageRect.top < imageSnapRadius) {
                resultY = imageRect.top
            } else {
                var resultYVert = Float.POSITIVE_INFINITY
                var resultYHoriz = Float.POSITIVE_INFINITY
                if (y >= Edge.BOTTOM.getCoordinate() - MIN_CROP_LENGTH_PX) {
                    resultYHoriz = Edge.BOTTOM.getCoordinate() - MIN_CROP_LENGTH_PX
                }
                if (((Edge.BOTTOM.getCoordinate() - y) * aspectRatio) <= MIN_CROP_LENGTH_PX) {
                    resultYVert = Edge.BOTTOM.getCoordinate() - (MIN_CROP_LENGTH_PX / aspectRatio)
                }
                resultY = Math.min(y, Math.min(resultYHoriz, resultYVert))
            }
            return resultY
        }

        private fun adjustBottom(y: Float, imageRect: RectF, imageSnapRadius: Float, aspectRatio: Float): Float {
            var resultY: Float
            if (imageRect.bottom - y < imageSnapRadius) {
                resultY = imageRect.bottom
            } else {
                var resultYVert = Float.NEGATIVE_INFINITY
                var resultYHoriz = Float.NEGATIVE_INFINITY
                if (y <= Edge.TOP.getCoordinate() + MIN_CROP_LENGTH_PX) {
                    resultYVert = Edge.TOP.getCoordinate() + MIN_CROP_LENGTH_PX
                }
                if (((y - Edge.TOP.getCoordinate()) * aspectRatio) <= MIN_CROP_LENGTH_PX) {
                    resultYHoriz = Edge.TOP.getCoordinate() + (MIN_CROP_LENGTH_PX / aspectRatio)
                }
                resultY = Math.max(y, Math.max(resultYHoriz, resultYVert))
            }
            return resultY
        }

    }

    private var _coordinate = 0.0f
    fun setCoordinate(coordinate: Float) {
        _coordinate = coordinate
    }
    fun offset(distance: Float) {
        _coordinate += distance
    }

    fun getCoordinate(): Float = _coordinate

    fun adjustCoordinate(x: Float, y: Float, imageRect: RectF, imageSnapRadius: Float, aspectRatio: Float) {
        when (this) {
            LEFT -> _coordinate = adjustLeft(x, imageRect, imageSnapRadius, aspectRatio)
            TOP -> _coordinate = adjustTop(y, imageRect, imageSnapRadius, aspectRatio)
            RIGHT -> _coordinate = adjustRight(x, imageRect, imageSnapRadius, aspectRatio)
            BOTTOM -> _coordinate = adjustBottom(y, imageRect, imageSnapRadius, aspectRatio)
        }
    }

    fun adjustCoordinate(aspectRatio: Float) {
        val left = Edge.LEFT.getCoordinate()
        val top = Edge.TOP.getCoordinate()
        val right = Edge.RIGHT.getCoordinate()
        val bottom = Edge.BOTTOM.getCoordinate()
        when (this) {
            LEFT -> _coordinate = AspectRatioUtil.calculateLeft(top, right, bottom, aspectRatio)
            TOP -> _coordinate = AspectRatioUtil.calculateTop(left, right, bottom, aspectRatio)
            RIGHT -> _coordinate = AspectRatioUtil.calculateRight(left, top, bottom, aspectRatio)
            BOTTOM -> _coordinate = AspectRatioUtil.calculateBottom(left, top, right, aspectRatio)
        }
    }

    private fun isOutOfBounds(top: Float, left: Float, bottom: Float, right: Float, imageRect: RectF): Boolean = (top < imageRect.top || left < imageRect.left || bottom > imageRect.bottom || right > imageRect.right)

    fun snapOffset(imageRect: RectF): Float {
        val oldCoordinate = _coordinate
        var newCoordinate: Float
        when (this) {
            LEFT -> newCoordinate = imageRect.left
            TOP -> newCoordinate = imageRect.top
            RIGHT -> newCoordinate = imageRect.right
            else -> newCoordinate = imageRect.bottom
        }
        return newCoordinate - oldCoordinate
    }

    fun isOutsideMargin(rect: RectF, margin: Float): Boolean {
        var result: Boolean
        when (this) {
            LEFT -> result = _coordinate - rect.left < margin
            TOP -> result = _coordinate - rect.top < margin
            RIGHT -> result = rect.right - _coordinate < margin
            else -> result = rect.bottom - _coordinate < margin
        }
        return result
    }

    fun snapToRect(imageRect: RectF): Float {
        val oldCoordinate = _coordinate
        when (this) {
            LEFT -> _coordinate = imageRect.left
            TOP -> _coordinate = imageRect.top
            RIGHT -> _coordinate = imageRect.right
            BOTTOM -> _coordinate = imageRect.bottom
        }
        return _coordinate - oldCoordinate
    }

    fun isNewRectangleOutOfBounds(edge: Edge, imageRect: RectF, aspectRatio: Float): Boolean {
        val offset = edge.snapOffset(imageRect)
        when (this) {
            LEFT -> {
                if (edge.equals(Edge.TOP)) {
                    val top = imageRect.top
                    val bottom = Edge.BOTTOM.getCoordinate() - offset
                    val right = Edge.RIGHT.getCoordinate()
                    val left = AspectRatioUtil.calculateLeft(top, right, bottom, aspectRatio)
                    return isOutOfBounds(top, left, bottom, right, imageRect)

                } else if (edge.equals(Edge.BOTTOM)) {
                    val bottom = imageRect.bottom
                    val top = Edge.TOP.getCoordinate() - offset
                    val right = Edge.RIGHT.getCoordinate()
                    val left = AspectRatioUtil.calculateLeft(top, right, bottom, aspectRatio)
                    return isOutOfBounds(top, left, bottom, right, imageRect)
                }
            }
            TOP -> {
                if (edge.equals(Edge.LEFT)) {
                    val left = imageRect.left
                    val right = Edge.RIGHT.getCoordinate() - offset
                    val bottom = Edge.BOTTOM.getCoordinate()
                    val top = AspectRatioUtil.calculateTop(left, right, bottom, aspectRatio)
                    return isOutOfBounds(top, left, bottom, right, imageRect)
                } else if (edge.equals(Edge.RIGHT)) {
                    val right = imageRect.right
                    val left = Edge.LEFT.getCoordinate() - offset
                    val bottom = Edge.BOTTOM.getCoordinate()
                    val top = AspectRatioUtil.calculateTop(left, right, bottom, aspectRatio)
                    return isOutOfBounds(top, left, bottom, right, imageRect)
                }
            }
            RIGHT -> {
                if (edge.equals(Edge.TOP)) {
                    val top = imageRect.top
                    val bottom = Edge.BOTTOM.getCoordinate() - offset
                    val left = Edge.LEFT.getCoordinate()
                    val right = AspectRatioUtil.calculateRight(left, top, bottom, aspectRatio)
                    return isOutOfBounds(top, left, bottom, right, imageRect)

                } else if (edge.equals(Edge.BOTTOM)) {
                    val bottom = imageRect.bottom
                    val top = Edge.TOP.getCoordinate() - offset
                    val left = Edge.LEFT.getCoordinate()
                    val right = AspectRatioUtil.calculateRight(left, top, bottom, aspectRatio)
                    return isOutOfBounds(top, left, bottom, right, imageRect)
                }
            }
            BOTTOM -> {
                if (edge.equals(Edge.LEFT)) {
                    val left = imageRect.left
                    val right = Edge.RIGHT.getCoordinate() - offset
                    val top = Edge.TOP.getCoordinate()
                    val bottom = AspectRatioUtil.calculateBottom(left, top, right, aspectRatio)
                    return isOutOfBounds(top, left, bottom, right, imageRect)
                } else if (edge.equals(Edge.RIGHT)) {
                    val right = imageRect.right
                    val left = Edge.LEFT.getCoordinate() - offset
                    val top = Edge.TOP.getCoordinate()
                    val bottom = AspectRatioUtil.calculateBottom(left, top, right, aspectRatio)
                    return isOutOfBounds(top, left, bottom, right, imageRect)
                }
            }
        }
        return true
    }







}