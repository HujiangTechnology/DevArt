package com.hujiang.devart.component.progress

import android.graphics.Canvas
import android.graphics.Paint
import android.os.SystemClock
import java.util.concurrent.TimeUnit

/**
 * Created by rarnu on 4/15/16.
 */
class PowerSaveModeDelegate: ProgressBarDelegate {

    companion object {
        private val REFRESH_RATE = TimeUnit.SECONDS.toMillis(1L)
    }

    private var _parent: CircularProgressDrawable? = null
    private var _currentRotation = 0

    private fun getRunnable(): Runnable? = _runnable
    private val _runnable = Runnable {
            _currentRotation += 50
            _currentRotation %= 360
            if (_parent!!.isRunning)
                _parent?.scheduleSelf(getRunnable(), SystemClock.uptimeMillis() + REFRESH_RATE)
            _parent?.invalidate()
    }

    constructor(parent: CircularProgressDrawable) {
        _parent = parent
    }

    override fun draw(canvas: Canvas?, paint: Paint?) {
        canvas?.drawArc(_parent?.getDrawableBounds(), _currentRotation.toFloat(), 300.0f, false, paint)
    }

    override fun start() {
        _parent?.invalidate()
        _parent?.scheduleSelf(_runnable, SystemClock.uptimeMillis() + REFRESH_RATE)
    }

    override fun stop() {
        _parent?.unscheduleSelf(_runnable)
    }

    override fun progressiveStop(listener: CircularProgressDrawable.OnEndListener?) {
        _parent?.stop()
    }
}