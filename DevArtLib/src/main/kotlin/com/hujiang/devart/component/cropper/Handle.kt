package com.hujiang.devart.component.cropper

import android.graphics.RectF

/**
 * Created by rarnu on 4/26/16.
 */
enum class Handle {

    TOP_LEFT(CornerHandleHelper(Edge.TOP, Edge.LEFT)),
    TOP_RIGHT(CornerHandleHelper(Edge.TOP, Edge.RIGHT)),
    BOTTOM_LEFT( CornerHandleHelper(Edge.BOTTOM, Edge.LEFT)),
    BOTTOM_RIGHT(CornerHandleHelper(Edge.BOTTOM, Edge.RIGHT)),
    LEFT(VerticalHandleHelper(Edge.LEFT)),
    TOP(HorizontalHandleHelper(Edge.TOP)),
    RIGHT(VerticalHandleHelper(Edge.RIGHT)),
    BOTTOM(HorizontalHandleHelper(Edge.BOTTOM)),
    CENTER(CenterHandleHelper());

    private var _helper: HandleHelper? = null
    constructor(helper: HandleHelper?) {
        _helper = helper
    }


    fun updateCropWindow(x: Float, y: Float, imageRect: RectF, snapRadius: Float) {
        _helper?.updateCropWindow(x, y, imageRect, snapRadius)
    }

    fun updateCropWindow(x: Float, y: Float, targetAspectRatio: Float, imageRect: RectF, snapRadius: Float) {
        _helper?.updateCropWindow(x, y, targetAspectRatio, imageRect, snapRadius)
    }
}