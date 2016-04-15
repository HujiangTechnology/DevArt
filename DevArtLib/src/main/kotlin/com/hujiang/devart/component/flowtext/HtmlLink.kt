package com.hujiang.devart.component.flowtext

import android.text.TextPaint

/**
 * Created by rarnu on 4/15/16.
 */
class HtmlLink: HtmlObject {

    var width = 0.0f
    var height = 0.0f
    var yOffset = 0.0f
    var url: String? = null

    constructor(content: String?, start: Int, end: Int, xOffset: Float, paint: TextPaint?, url: String?): super(content, start, end, xOffset, paint) {
        this.url = url
    }

}