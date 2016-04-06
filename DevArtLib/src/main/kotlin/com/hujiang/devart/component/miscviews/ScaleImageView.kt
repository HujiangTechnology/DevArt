package com.hujiang.devart.component.miscviews

import android.content.Context
import android.graphics.Bitmap
import android.graphics.drawable.Drawable
import android.util.AttributeSet
import android.widget.ImageView
import android.widget.RelativeLayout

/**
 * Created by rarnu on 4/6/16.
 */
class ScaleImageView: ImageView {

    private var _imageChangeListener: ImageChangeListener? = null
    var imageChangeListener: ImageChangeListener?
        get() = _imageChangeListener
        set(value) { _imageChangeListener = value }
    private var _scaleToWidth = false

    constructor(context: Context): this(context, null)

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        init()
    }

    constructor(context: Context, attrs: AttributeSet?): this(context, attrs, 0)


    private fun init() {
        scaleType = ScaleType.CENTER_INSIDE
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        val widthMode = MeasureSpec.getMode(widthMeasureSpec)
        val heightMode = MeasureSpec.getMode(heightMeasureSpec)
        var width = MeasureSpec.getSize(widthMeasureSpec)
        var height = MeasureSpec.getSize(heightMeasureSpec)
        if (widthMode == MeasureSpec.EXACTLY || widthMode == MeasureSpec.AT_MOST) {
            _scaleToWidth = true
        } else if (heightMode == MeasureSpec.EXACTLY || heightMode == MeasureSpec.AT_MOST) {
            _scaleToWidth = false
        } else {
            throw IllegalStateException()
        }

        if (drawable == null || drawable.intrinsicWidth == 0) {
            super.onMeasure(widthMeasureSpec, heightMeasureSpec)
            return
        } else {
            if (_scaleToWidth) {
                val iw = drawable.intrinsicWidth
                val ih = drawable.intrinsicHeight
                var heightC = width * ih / iw
                if (height > 0)
                    if (heightC > height) {
                        heightC = height
                        width = heightC * iw / ih
                    }
                scaleType = ScaleType.CENTER_CROP
                setMeasuredDimension(width, heightC)
            } else {
                var marg = 0
                if (parent != null) {
                    if (parent.parent != null) {
                        marg += (parent.parent as RelativeLayout).paddingTop
                        marg += (parent.parent as RelativeLayout).paddingBottom
                    }
                }
                val iw = drawable.intrinsicWidth
                val ih = drawable.intrinsicHeight
                width = height * iw / ih
                height -= marg
                setMeasuredDimension(width, height)
            }
        }
    }

    interface ImageChangeListener {
        fun changed(isEmpty: Boolean)
    }

    override fun setImageBitmap(bm: Bitmap?) {
        super.setImageBitmap(bm)
        _imageChangeListener?.changed(bm == null)

    }

    override fun setImageDrawable(drawable: Drawable?) {
        super.setImageDrawable(drawable)
        _imageChangeListener?.changed(drawable == null)
    }

    override fun setImageResource(resId: Int) {
        super.setImageResource(resId)
        _imageChangeListener?.changed(resId == 0)
    }


}