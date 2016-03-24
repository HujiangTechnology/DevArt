package com.hujiang.devart.component.sliding

import android.graphics.Canvas

/**
 * Created by rarnu on 3/24/16.
 */
interface CanvasTransformer {

    fun transformCanvas(canvas: Canvas?, percentOpen: Float)
}