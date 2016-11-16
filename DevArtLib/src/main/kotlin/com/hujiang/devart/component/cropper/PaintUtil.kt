package com.hujiang.devart.component.cropper

import android.content.Context
import android.content.res.Resources
import android.graphics.Paint
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/26/16.
 */
object PaintUtil {

    fun newBorderPaint(context: Context): Paint {
        val paint = Paint()
        paint.style = Paint.Style.STROKE
        paint.strokeWidth = context.resources.getDimension(R.dimen.border_thickness)
        paint.color = context.resources.getColor(R.color.white_translucent, context.theme)
        return paint
    }

    fun newGuidelinePaint(context: Context): Paint {
        val paint = Paint()
        paint.style = Paint.Style.STROKE
        paint.strokeWidth = context.resources.getDimension(R.dimen.guideline_thickness)
        paint.color = context.resources.getColor(R.color.white_translucent, context.theme)
        return paint
    }

    fun newSurroundingAreaOverlayPaint(context: Context): Paint {
        val paint = Paint()
        paint.style = Paint.Style.FILL
        paint.color = context.resources.getColor(R.color.black_translucent, context.theme)
        return paint
    }

    fun newCornerPaint(context: Context): Paint {
        val paint = Paint()
        paint.style = Paint.Style.STROKE
        paint.strokeWidth = context.resources.getDimension(R.dimen.corner_thickness)
        paint.color = context.resources.getColor(R.color.white, context.theme)
        return paint
    }


}