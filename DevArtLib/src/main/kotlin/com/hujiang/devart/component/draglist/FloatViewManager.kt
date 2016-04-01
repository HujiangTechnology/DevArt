package com.hujiang.devart.component.draglist

import android.graphics.Bitmap
import android.graphics.Color
import android.graphics.Point
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.ListView

/**
 * Created by rarnu on 4/1/16.
 */
open class FloatViewManager: IFloatViewManager {

    private var _floatBitmap: Bitmap? = null
    private var _imageView: ImageView? = null
    private var _floatBGColor = Color.BLACK
    private var _listView: ListView? = null

    constructor(lv: ListView?) {
        _listView = lv
    }

    fun setBackgroundColor(color: Int) {
        _floatBGColor = color
    }

    override fun onCreateFloatView(position: Int): View? {
        val v = _listView!!.getChildAt(position + _listView!!.headerViewsCount - _listView!!.firstVisiblePosition) ?: return null
        v.isPressed = false
        v.isDrawingCacheEnabled = true
        _floatBitmap = Bitmap.createBitmap(v.drawingCache)
        v.isDrawingCacheEnabled = false
        if (_imageView == null) {
            _imageView = ImageView(_listView!!.context)
        }
        _imageView?.setBackgroundColor(_floatBGColor)
        _imageView?.setPadding(0, 0, 0, 0)
        _imageView?.setImageBitmap(_floatBitmap)
        _imageView?.layoutParams = ViewGroup.LayoutParams(v.width, v.height)
        return _imageView
    }

    override fun onDragFloatView(floatView: View?, location: Point?, touch: Point?) { }

    override fun onDestroyFloatView(floatView: View?) {
        (floatView as ImageView).setImageDrawable(null)
        _floatBitmap?.recycle()
        _floatBitmap = null
    }
}