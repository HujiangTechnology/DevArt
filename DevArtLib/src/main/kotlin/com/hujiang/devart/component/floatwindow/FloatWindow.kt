package com.hujiang.devart.component.floatwindow

import android.content.Context
import android.graphics.Color
import android.graphics.PixelFormat
import android.view.MotionEvent
import android.view.View
import android.view.WindowManager

/**
 * Created by rarnu on 3/25/16.
 */
class FloatWindow: View, View.OnTouchListener {

    private var _lastDownTime = 0L
    private var _lastUpTime = 0L
    private var _pressedTime = 0L
    private var _downX = 0.0f
    private var _downY = 0.0f
    private var _upX = 0.0f
    private var _upY = 0.0f

    private var _windowMgr: WindowManager? = null
    private var _wmParams: WindowManager.LayoutParams? = null
    private var _view: View? = null
    private var _lastX = 0.0f
    private var _lastY = 0.0f
    private var _moveListener: FloatWindowListener? = null

    constructor(context: Context, view: View?, listener: FloatWindowListener?): super(context) {
        _windowMgr = context.getSystemService(Context.WINDOW_SERVICE) as WindowManager
        _moveListener = listener
        initView(view)
    }


    private fun initView(view: View?) {
        _view = view
        _view?.setBackgroundColor(Color.TRANSPARENT)
        _view?.setOnTouchListener(this)
    }

    fun show(x: Int, y: Int) {
        _wmParams = WindowManager.LayoutParams()
        _wmParams?.type = WindowManager.LayoutParams.TYPE_SYSTEM_ALERT or WindowManager.LayoutParams.TYPE_SYSTEM_OVERLAY
        _wmParams?.flags = WindowManager.LayoutParams.FLAG_NOT_TOUCH_MODAL or WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE
        _wmParams?.width = WindowManager.LayoutParams.WRAP_CONTENT
        _wmParams?.height = WindowManager.LayoutParams.WRAP_CONTENT
        _wmParams?.format = PixelFormat.TRANSLUCENT
        if (x != -1) {
            _wmParams?.x = x
        }
        if (y != -1) {
            _wmParams?.y = y
        }
        _windowMgr?.addView(_view, _wmParams)
    }

    fun hide() = _windowMgr?.removeView(_view)

    fun setNewView(view: View?, x: Int, y: Int) {
        hide()
        initView(view)
        show(x, y)
    }

    override fun onTouch(v: View?, event: MotionEvent?): Boolean {
        val x = event!!.rawX
        val y = event.rawY

        when (event.action) {
            MotionEvent.ACTION_DOWN -> {
                _lastDownTime = System.currentTimeMillis()
                _lastX = x
                _lastY = y
                _downX = x
                _downY = y
            }

            MotionEvent.ACTION_MOVE -> {
                _wmParams!!.x += (x - _lastX).toInt()
                _wmParams!!.y += (y - _lastY).toInt()
                _windowMgr?.updateViewLayout(_view, _wmParams)
                _lastX = x
                _lastY = y
                if (Math.abs(_lastX - _downX) < 10 && Math.abs(_lastY - _downY) < 10) {
                    _pressedTime = System.currentTimeMillis()
                    if (_lastDownTime != 0L) {
                        if ((_pressedTime - _lastDownTime) > 1500) {
                            _lastDownTime = 0L
                            if (_moveListener != null) {
                                _moveListener!!.onFloatWindowLongClick()
                            }
                        }
                    }
                }
            }

            MotionEvent.ACTION_UP -> {
                if (_moveListener != null) {
                    _moveListener!!.onPositionChanged(v, _wmParams!!.x, _wmParams!!.y)
                }
                _upX = x
                _upY = y
                if (Math.abs(_upX - _downX) < 10 && Math.abs(_upY - _downY) < 10) {
                    _lastUpTime = System.currentTimeMillis()
                    if ((_lastUpTime - _lastDownTime) < 500) {
                        if (_moveListener != null) {
                            _moveListener!!.onFloatWindowClick()
                        }
                    }
                }
            }

        }

        return true
    }
}