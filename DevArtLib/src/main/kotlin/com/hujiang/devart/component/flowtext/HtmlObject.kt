package com.hujiang.devart.component.flowtext

import android.text.TextPaint

/**
 * Created by rarnu on 4/15/16.
 */
open class HtmlObject {

    var content: String? = null
    var start = 0
    var end = 0
    var xOffset = 0.0f
    var paint: TextPaint? = null
    var recycle = false

    constructor(content: String?, start: Int, end: Int, xOffset: Float, paint: TextPaint?) {
        this.content = content
        this.start = start
        this.end = end
        this.xOffset = xOffset
        this.paint = paint
    }

}