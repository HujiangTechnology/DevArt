package com.hujiang.devart.component.flowtext

import android.view.MotionEvent
import android.view.View

/**
 * Created by rarnu on 4/15/16.
 */
class ClickHandler: View.OnTouchListener {

    companion object {
        private fun getPointDistance(x1: Float, y1: Float, x2: Float, y2: Float): Double = Math.sqrt(Math.pow((x1 - x2).toDouble(), 2.toDouble()) + Math.pow((y1- y2).toDouble(), 2.toDouble()))
    }

    private var _spanParser: SpanParser? = null
    private var _onLinkClickListener: OnLinkClickListener? = null
    private var _distance = 0.toDouble()
    private var _x1 = 0.0f
    private var _y1 = 0.0f
    private var _x2 = 0.0f
    private var _y2 = 0.0f

    constructor(spanParser: SpanParser?) {
        _spanParser = spanParser
    }

    override fun onTouch(v: View?, event: MotionEvent?): Boolean {
        val action = event!!.action
        if(action == MotionEvent.ACTION_DOWN){
            _distance = 0.toDouble()
            _x1 = event.x
            _y1 = event.y
        }
        if(action == MotionEvent.ACTION_MOVE){
            _x2 = event.x
            _y2 = event.y
            _distance = getPointDistance(_x1, _y1, _x2, _y2)
        }
        if(_distance < 10) {
            if (action == MotionEvent.ACTION_UP) {
                return onClick(event.x, event.y)
            }
            return true
        }
        return false
    }

    private fun onClick(x: Float, y: Float): Boolean {
        val links = _spanParser?.getLinks()
        for (link in links!!) {
            val tlX = link.xOffset
            val tlY = link.yOffset
            val brX = link.xOffset + link.width
            val brY = link.yOffset + link.height
            if(x > tlX && x < brX){
                if(y > tlY && y < brY){
                    onLinkClick(link.url)
                    return true
                }
            }
        }
        return false
    }

    private fun onLinkClick(url: String?) = _onLinkClickListener?.onLinkClick(url)

    fun getOnLinkClickListener():OnLinkClickListener? = _onLinkClickListener

    fun setOnLinkClickListener(listener: OnLinkClickListener?) {
        _onLinkClickListener = listener
    }
}