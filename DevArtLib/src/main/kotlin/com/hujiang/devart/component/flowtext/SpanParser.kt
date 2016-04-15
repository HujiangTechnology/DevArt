package com.hujiang.devart.component.flowtext

import android.graphics.Typeface
import android.text.Spannable
import android.text.style.StyleSpan
import android.text.style.URLSpan
import java.util.*

/**
 * Created by rarnu on 4/15/16.
 */
class SpanParser {

    companion object {
        private fun isArrayFull(array: BooleanArray?): Boolean {
            for(i in 0..array!!.size - 1) {
                if(!array[i]) {
                    return false
                }
            }
            return true
        }
    }

    private var _paintHelper:PaintHelper? = null
    private var _flowTextView: FlowTextView? = null
    private var _links = arrayListOf<HtmlLink>()
    private var _textLength = 0
    private var _spannable: Spannable? = null
    private var _sorterMap = hashMapOf<Int, HtmlObject>()

    constructor(flowTextView: FlowTextView?, paintHelper: PaintHelper?) {
        _flowTextView = flowTextView
        _paintHelper = paintHelper
    }

    fun parseSpans(objects: MutableList<HtmlObject>? , spans: Array<Any?>, lineStart: Int, lineEnd: Int, baseXOffset: Float): Float {
        _sorterMap.clear()
        val charFlagSize = lineEnd - lineStart
        val charFlags = BooleanArray(charFlagSize)
        var tempString: String?
        var spanStart: Int
        var spanEnd: Int

        for (span in spans) {
            spanStart = _spannable!!.getSpanStart(span)
            spanEnd = _spannable!!.getSpanEnd(span)
            if(spanStart <lineStart) {
                spanStart = lineStart
            }
            if(spanEnd >lineEnd) {
                spanEnd = lineEnd
            }
            for(i in spanStart..spanEnd - 1) {
                val charFlagIndex = i - lineStart
                charFlags[charFlagIndex] = true
            }
            tempString = extractText(spanStart, spanEnd)
            _sorterMap.put(spanStart, parseSpan(span, tempString, spanStart, spanEnd))
        }
        var charCounter = 0
        while(!isArrayFull(charFlags)){
            while(true){
                if(charCounter >= charFlagSize) {
                    break
                }
                if(charFlags[charCounter]){
                    charCounter++
                    continue
                }
                val temp1 = charCounter
                while(true){
                    if(charCounter > charFlagSize) {
                        break
                    }
                    if(charCounter < charFlagSize){
                        if(!charFlags[charCounter]){
                            charFlags[charCounter] = true
                            charCounter++
                            continue
                        }
                    }
                    val temp2 = charCounter
                    spanStart = lineStart + temp1
                    spanEnd = lineStart + temp2
                    tempString = extractText(spanStart, spanEnd)
                    _sorterMap.put(spanStart, parseSpan(null, tempString, spanStart, spanEnd))
                    break
                }
            }
        }
        val sorterKeys = _sorterMap.keys.toTypedArray()
        Arrays.sort(sorterKeys)
        var thisXoffset = baseXOffset
        for(i in 0..sorterKeys.size - 1){
            val thisObj = _sorterMap[sorterKeys[i]]
            thisObj?.xOffset = thisXoffset
            thisXoffset += thisObj!!.paint!!.measureText(thisObj.content)
            objects?.add(thisObj)
        }
        return thisXoffset - baseXOffset
    }

    private fun extractText(start: Int, end: Int): String? {
        var nstart = start
        var nend = end
        if (nstart < 0) {
            nstart = 0
        }
        if (nend > _textLength - 1) {
            nend = _textLength - 1
        }
        return _spannable?.subSequence(nstart, nend).toString()
    }

    private fun parseSpan(span: Any?, content: String?, start: Int, end: Int): HtmlObject {
        if(span is URLSpan){
            return getHtmlLink(span, content, start, end, 0.0f)
        }else if(span is StyleSpan){
            return getStyledObject(span, content, start, end, 0.0f)
        }else{
            return getHtmlObject(content, start, end, 0.0f)
        }
    }

    private fun getHtmlObject(content: String?, start: Int, end: Int, thisXOffset: Float): HtmlObject {
        return HtmlObject(content, start, end, thisXOffset, _flowTextView?.getTextPaint())
    }

    private fun getHtmlLink(span: URLSpan?, content: String?, start: Int, end: Int, thisXOffset: Float): HtmlLink {
        val obj = HtmlLink(content, start, end, thisXOffset, _flowTextView?.getLinkPaint(), span?.url)
        _links.add(obj)
        return obj
    }

    private fun getStyledObject(span: StyleSpan?, content: String?, start: Int, end: Int, thisXOffset: Float): HtmlObject {
        val paint = _paintHelper?.getPaintFromHeap()
        paint?.typeface = Typeface.defaultFromStyle(span!!.style)
        paint?.textSize = _flowTextView!!.getTextSize()
        paint?.color = _flowTextView!!.getColor()
        span.updateDrawState(paint)
        span.updateMeasureState(paint)
        val obj = HtmlObject(content, start, end, thisXOffset, paint)
        obj.recycle = true
        return obj
    }

    fun reset(){
        _links.clear()
    }

    fun addLink(thisLink: HtmlLink, yOffset: Float, width: Float, height: Float){
        thisLink.yOffset = yOffset - 20
        thisLink.width = width
        thisLink.height = height + 20
        _links.add(thisLink)
    }

    fun getLinks(): MutableList<HtmlLink> = _links

    fun getSpannable(): Spannable? = _spannable

    fun setSpannable(spannable: Spannable?) {
        _spannable = spannable
        _textLength = _spannable!!.length
    }

}