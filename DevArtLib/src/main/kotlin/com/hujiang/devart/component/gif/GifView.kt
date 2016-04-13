package com.hujiang.devart.component.gif

import android.content.Context
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.drawable.BitmapDrawable
import android.os.Handler
import android.os.Message
import android.os.Parcelable
import android.os.SystemClock
import android.util.AttributeSet
import android.view.View
import android.widget.ImageView
import java.io.InputStream

/**
 * Created by rarnu on 3/29/16.
 */
class GifView : ImageView, GifAction {

    private var _gifDecoder: GifDecoder? = null
    private var _currentImage: Bitmap? = null
    private var _isRun = true
    private var _pause = false
    private var _drawThread: DrawThread? = null
    private var _cacheImage = false
    private var _backView: View? = null
    private var _animationType = GifImageType.SYNC_DECODER

    private var _redrawHandler = object : Handler() {
        override fun handleMessage(msg: Message?) {
            try {
                if (_backView != null) {
                    _backView?.background = BitmapDrawable(context.resources, _currentImage)
                } else {
                    drawImage()
                }
            } catch (e: Exception) {

            }
        }
    }

    constructor(context: Context) : this(context, null)

    constructor(context: Context, attrs: AttributeSet?) : this(context, attrs, 0)

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int) : super(context, attrs, defStyle) {
        scaleType = ImageView.ScaleType.FIT_XY
    }

    override fun parseOk(parseStatus: Boolean, frameIndex: Int) {
        if (parseStatus) {
            if (_gifDecoder != null) {
                when (_animationType) {
                    GifImageType.WAIT_FINISH -> {
                        if (frameIndex == -1) {
                            if (_gifDecoder!!.frameCount > 1) {
                                DrawThread().start()
                            } else {
                                reDraw()
                            }
                        }
                    }
                    GifImageType.COVER -> {
                        if (frameIndex == 1) {
                            _currentImage = _gifDecoder?.getImage()
                            reDraw()
                        } else if (frameIndex == -1) {
                            if (_gifDecoder!!.frameCount > 1) {
                                if (_drawThread == null) {
                                    _drawThread = DrawThread()
                                    _drawThread?.start()
                                }
                            } else {
                                reDraw()
                            }
                        }
                    }
                    GifImageType.SYNC_DECODER -> {
                        if (frameIndex == 1) {
                            _currentImage = _gifDecoder?.getImage()
                            reDraw()
                        } else if (frameIndex == -1) {
                            reDraw()
                        } else {
                            if (_drawThread == null) {
                                _drawThread = DrawThread()
                                _drawThread?.start()
                            }
                        }
                    }
                }
            }
        }
    }

    private fun reDraw() {
        val msg = _redrawHandler.obtainMessage()
        _redrawHandler.sendMessage(msg)
    }

    private fun drawImage() {
        setImageBitmap(_currentImage)
        invalidate()
    }

    enum class GifImageType {
        WAIT_FINISH(0), SYNC_DECODER(1), COVER(2);

        var nativeInt: Int

        constructor(i: Int) {
            nativeInt = i
        }
    }

    inner class DrawThread : Thread() {
        override fun run() {
            if (_gifDecoder == null) {
                return
            }
            while (_isRun) {
                if (_gifDecoder!!.frameCount == 1) {
                    val f = _gifDecoder?.next()
                    _currentImage = f?.image
                    _gifDecoder?.free()
                    reDraw()
                    break
                }
                if (_pause == false) {
                    val frame = _gifDecoder?.next()
                    if (frame == null) {
                        SystemClock.sleep(50)
                        continue
                    }
                    if (frame.image != null) {
                        _currentImage = frame.image
                    } else if (frame.imageName != null) {
                        _currentImage = BitmapFactory.decodeFile(frame.imageName)
                    }
                    val sp = frame.delay
                    reDraw()
                    SystemClock.sleep(sp.toLong())
                } else {
                    SystemClock.sleep(50)
                }
            }
        }
    }

    fun showCover() {
        if (_gifDecoder == null) {
            return
        }
        _pause = true
        _currentImage = _gifDecoder?.getImage()
        invalidate()
    }

    fun showAnimation() {
        if (_pause) {
            _pause = false
        }
    }

    override fun onSaveInstanceState(): Parcelable? {
        super.onSaveInstanceState()
        _gifDecoder?.free()
        return null
    }

    private fun setGifDecoderImage(gif: ByteArray?) {
        if (_gifDecoder == null) {
            _gifDecoder = GifDecoder(this)
        }
        _gifDecoder?.setGifImage(gif)
        _gifDecoder?.start()
    }

    private fun setGifDecoderImage(ins: InputStream?) {
        if (_gifDecoder == null) {
            _gifDecoder = GifDecoder(this)
        }
        _gifDecoder?.setGifImage(ins)
        _gifDecoder?.start()
    }

    fun setAsBackground(v: View?) {
        _backView = v
    }

    fun setGifImage(gif: ByteArray?) = setGifDecoderImage(gif)

    fun setGifImage(ins: InputStream?) = setGifDecoderImage(ins)

    fun setGifImage(resId: Int) = setGifDecoderImage(resources.openRawResource(resId))

    fun destroy() = _gifDecoder?.free()

    fun setGifImageType(type: GifImageType) {
        if (_gifDecoder == null) {
            _animationType = type
        }
    }
}