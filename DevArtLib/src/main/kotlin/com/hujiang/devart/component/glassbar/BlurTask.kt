package com.hujiang.devart.component.glassbar

import android.content.Context
import android.graphics.Bitmap
import android.graphics.Canvas
import android.os.AsyncTask

/**
 * Created by rarnu on 4/14/16.
 */
class BlurTask {

    interface Listener {
        fun onBlurOperationFinished()
    }

    private var _source: Bitmap? = null
    private var _canvas: Canvas? = null
    private var _task: AsyncTask<Void, Void, Void>? = null
    private var _blurred: Bitmap? = null
    private var _listener: Listener? = null
    private var _context: Context? = null
    private var _radius = 0


    constructor(context: Context, listener: Listener?, source: Bitmap?): this(context, listener, source, GlassActionBar.DEFAULT_BLUR_RADIUS)

    constructor(context: Context, listener: Listener?, source: Bitmap?, radius: Int) {
        _context = context
        _listener = listener
        _source = source
        _radius = radius
        _canvas = Canvas(source)
        startTask()
    }

    private fun startTask() {
        _task = object : AsyncTask<Void, Void, Void>() {

            override fun doInBackground(vararg params: Void?): Void? {
                blurSourceBitmap()
                return null
            }

            override fun onPostExecute(result: Void?) {
                _canvas?.drawBitmap(_blurred, 0.0f, 0.0f, null)
                _blurred?.recycle()
                _listener?.onBlurOperationFinished()
            }
        }
        _task?.execute()
    }

    private fun blurSourceBitmap() {
        var section: Bitmap? = _source ?: return
        val start = System.nanoTime()
        _blurred = Blur.apply(_context!!, _source, _radius)
    }

    fun cancel() {
        _task?.cancel(true)
        _task = null
    }
}