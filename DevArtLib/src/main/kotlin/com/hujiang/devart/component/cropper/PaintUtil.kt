package com.hujiang.devart.component.cropper

import android.content.res.Resources
import android.graphics.Paint
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/26/16.
 */
object PaintUtil {

    fun newBorderPaint(resources: Resources): Paint {
        val paint = Paint()
        paint.style = Paint.Style.STROKE
        paint.strokeWidth = resources.getDimension(R.dimen.border_thickness)
        paint.color = resources.getColor(R.color.white_translucent)
        return paint
    }

    fun newGuidelinePaint(resources: Resources): Paint {
        val paint = Paint()
        paint.style = Paint.Style.STROKE
        paint.strokeWidth = resources.getDimension(R.dimen.guideline_thickness)
        paint.color = resources.getColor(R.color.white_translucent)
        return paint
    }

    fun newSurroundingAreaOverlayPaint(resources: Resources): Paint {
        val paint = Paint()
        paint.style = Paint.Style.FILL
        paint.color = resources.getColor(R.color.black_translucent)
        return paint
    }

    fun newCornerPaint(resources: Resources): Paint {
        val paint = Paint()
        paint.style = Paint.Style.STROKE
        paint.strokeWidth = resources.getDimension(R.dimen.corner_thickness)
        paint.color = resources.getColor(R.color.white)
        return paint
    }


}