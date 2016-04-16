package com.hujiang.devart.component.progress

import android.graphics.Canvas
import android.graphics.Paint

/**
 * Created by rarnu on 4/15/16.
 */
interface ProgressBarDelegate {
    fun draw(canvas: Canvas?, paint: Paint?)
    fun start()
    fun stop()
    fun progressiveStop(listener: CircularProgressDrawable.OnEndListener?)

}