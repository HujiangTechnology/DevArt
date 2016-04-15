package com.hujiang.devart.component.flowtext

import android.graphics.Paint
import android.text.TextPaint

/**
 * Created by rarnu on 4/15/16.
 */
class PaintHelper {

    private var _paintHeap = arrayListOf<TextPaint?>()

    fun getPaintFromHeap(): TextPaint? {
        if(_paintHeap.size >0){
            return _paintHeap.removeAt(0)
        }else{
            return TextPaint(Paint.ANTI_ALIAS_FLAG)
        }
    }

    fun setColor(color: Int){
        for (paint in _paintHeap) {
            paint?.color = color
        }
    }

    fun recyclePaint(paint: TextPaint?){
        _paintHeap.add(paint)
    }

}