package com.hujiang.devart.sample.adapter

import android.content.Context
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import com.hujiang.devart.component.coverflow.CoverFlowAdapter
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 4/26/16.
 */
class ImageCoverFlowAdapter: CoverFlowAdapter {

    private var _dataChanged = false
    private var _image1: Bitmap? = null
    private var _image2: Bitmap? = null

    constructor(context: Context) {
        _image1 = BitmapFactory.decodeResource(context.resources, R.drawable.coverbg)
        _image2 = BitmapFactory.decodeResource(context.resources, R.drawable.ic_launcher)
    }

    fun changeBitmap() {
        _dataChanged = !_dataChanged
        notifyDataSetChanged()
    }

    override fun getCount(): Int = if (_dataChanged) 3 else 8

    override fun getImage(position: Int): Bitmap? =if (_dataChanged && position == 0) _image2 else _image1
}