package com.hujiang.devart.component.sliding

import android.content.Context
import android.graphics.Bitmap
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Paint
import android.graphics.drawable.Drawable
import android.util.AttributeSet
import android.util.TypedValue
import android.view.MotionEvent
import android.view.View
import android.view.ViewGroup
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/24/16.
 */
class CustomViewBehind : ViewGroup {

    companion object {
        private val TAG = "CustomViewBehind"
        private val MARGIN_THRESHOLD = 16.0f
    }

    private val _fadePaint = Paint()
    private var _touchMode = SlidingMenu.TOUCHMODE_MARGIN
    var touchMode: Int
        get() = _touchMode
        set(value) {
            _touchMode = value
        }
    private var _viewAbove: CustomViewAbove? = null
    var viewAbove: CustomViewAbove?
        get() = _viewAbove
        set(value) {
            _viewAbove = value
        }
    private var _content: View? = null
    var content: View?
        get() = _content
        set(value) {
            if (_content != null) {
                removeView(_content)
            }
            _content = value
            addView(_content)
        }
    private var _secondaryContent: View? = null
    var secondaryContent: View?
        get() = _secondaryContent
        set(value) {
            if (_secondaryContent != null) {
                removeView(_secondaryContent)
            }
            _secondaryContent = value
            addView(_secondaryContent)
        }
    private var _marginThreshold = 0
    private var _widthOffset = 0
    var widthOffset: Int
        get() = _widthOffset
        set(value) {
            _widthOffset = value
            requestLayout()
        }
    private var _transformer: CanvasTransformer? = null
    var transformer: CanvasTransformer?
        get() = _transformer
        set(value) {
            _transformer = value
        }
    private var _childrenEnabled = false
    var childrenEnabled: Boolean
        get() = _childrenEnabled
        set(value) {
            _childrenEnabled = value
        }
    private var _mode = 0
    var mode: Int
        get() = _mode
        set(value) {
            if (value == SlidingMenu.LEFT || value == SlidingMenu.RIGHT) {
                _content?.visibility = View.VISIBLE
                _secondaryContent?.visibility = View.INVISIBLE
            }
            _mode = value
        }
    private var _fadeEnabled = false
    var fadeEnabled: Boolean
        get() = _fadeEnabled
        set(value) {
            _fadeEnabled = value
        }
    private var _scrollScale = 0.0f
    var scrollScale: Float
        get() = _scrollScale
        set(value) {
            _scrollScale = value
        }
    private var _shadowDrawable: Drawable? = null
    var shadowDrawable: Drawable?
        get() = _shadowDrawable
        set(value) {
            _shadowDrawable = value
            invalidate()
        }
    private var _secondaryShadowDrawable: Drawable? = null
    var secondaryShadowDrawable: Drawable?
        get() = _secondaryShadowDrawable
        set(value) {
            _secondaryShadowDrawable = value
            invalidate()
        }
    private var _shadowWidth = 0
    var shadowWidth: Int
        get() = _shadowWidth
        set(value) {
            _shadowWidth = value
            invalidate()
        }
    private var _fadeDegree = 0.0f
    var fadeDegree: Float
        get() = _fadeDegree
        set(value) {
            if (value > 1.0f || value < 0.0f) {
                throw IllegalStateException("The BehindFadeDegree must be between 0.0f and 1.0f")
            }
            _fadeDegree = value
        }
    private var _selectorEnabled = true
    var selectorEnabled: Boolean
        get() = _selectorEnabled
        set(value) {
            _selectorEnabled = value
        }
    private var _selectorDrawable: Bitmap? = null
    var selectorDrawable: Bitmap?
        get() = _selectorDrawable
        set(value) {
            _selectorDrawable = value
            refreshDrawableState()
        }
    private var _selectedView: View? = null
    var selectedView: View?
        get() = _selectedView
        set(value) {
            if (_selectedView != null) {
                _selectedView!!.setTag(R.id.selected_view, null)
                _selectedView = null
            }
            if (value != null && value.parent != null) {
                _selectedView = value
                _selectedView?.setTag(R.id.selected_view, TAG + "SelectedView")
                invalidate()
            }
        }

    constructor(context: Context) : this(context, null)

    constructor(context: Context, attrs: AttributeSet?) : super(context, attrs) {
        _marginThreshold = TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, MARGIN_THRESHOLD, resources.displayMetrics).toInt()
    }

    override fun scrollTo(x: Int, y: Int) {
        super.scrollTo(x, y)
        if (_transformer != null) {
            invalidate()
        }
    }

    override fun onInterceptTouchEvent(ev: MotionEvent?): Boolean = !_childrenEnabled

    override fun onTouchEvent(event: MotionEvent?): Boolean = !_childrenEnabled

    override fun dispatchDraw(canvas: Canvas?) {
        if (_transformer != null) {
            canvas?.save()
            _transformer?.transformCanvas(canvas, _viewAbove!!.percentOpen)
            super.dispatchDraw(canvas)
            canvas?.restore()
        } else {
            super.dispatchDraw(canvas)
        }
    }

    var behindWidth: Int = 0
        get() = _content!!.width

    override fun onLayout(changed: Boolean, l: Int, t: Int, r: Int, b: Int) {
        val width = r - l
        val height = b - t
        _content?.layout(0, 0, width - _widthOffset, height)
        _secondaryContent?.layout(0, 0, width - _widthOffset, height)
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        val width = getDefaultSize(0, widthMeasureSpec)
        val height = getDefaultSize(0, heightMeasureSpec)
        setMeasuredDimension(width, height)
        val contentWidth = getChildMeasureSpec(widthMeasureSpec, 0, width - _widthOffset)
        val contentHeight = getChildMeasureSpec(heightMeasureSpec, 0, height)
        _content?.measure(contentWidth, contentHeight)
        _secondaryContent?.measure(contentWidth, contentHeight)
    }

    fun getMenuPage(page: Int): Int {
        val npage = if (page > 1) {
            2
        } else {
            if (page < 1) {
                0
            } else {
                page
            }
        }
        return if (_mode == SlidingMenu.LEFT && page > 1) {
            0
        } else if (_mode == SlidingMenu.RIGHT && page < 1) {
            2
        } else {
            npage
        }
    }

    fun scrollBehindTo(content: View?, x: Int, y: Int) {
        var vis = View.VISIBLE
        if (_mode == SlidingMenu.LEFT) {
            if (x >= content!!.left) {
                vis = View.INVISIBLE
            }
            scrollTo(((x + behindWidth) * _scrollScale).toInt(), y)
        } else if (_mode == SlidingMenu.RIGHT) {
            if (x <= content!!.left) {
                vis = View.INVISIBLE
            }
            scrollTo((behindWidth - width + (x - behindWidth) * _scrollScale).toInt(), y)
        } else if (_mode == SlidingMenu.LEFT_RIGHT) {
            _content?.visibility = if (x >= content!!.left) {
                View.INVISIBLE
            } else {
                View.VISIBLE
            }
            _secondaryContent?.visibility = if (x <= content.left) {
                View.INVISIBLE
            } else {
                View.VISIBLE
            }
            vis = if (x == 0) {
                View.INVISIBLE
            } else {
                View.VISIBLE
            }
            if (x <= content.left) {
                scrollTo(((x + behindWidth) * _scrollScale).toInt(), y)
            } else {
                scrollTo((behindWidth - width + (x - behindWidth) * _scrollScale).toInt(), y)
            }
        }
        visibility = vis
    }

    fun getMenuLeft(content: View?, page: Int): Int = when (_mode) {
        SlidingMenu.LEFT -> {
            when (page) {
                0 -> content!!.left - behindWidth
                else -> content!!.left
            }
        }
        SlidingMenu.RIGHT -> {
            when (page) {
                2 -> content!!.left + behindWidth
                else -> content!!.left
            }
        }
        SlidingMenu.LEFT_RIGHT -> {
            when (page) {
                0 -> content!!.left - behindWidth
                2 -> content!!.left + behindWidth
                else -> content!!.left
            }
        }
        else -> content!!.left
    }

    fun getAbsLeftBound(content: View?): Int = when (_mode) {
        SlidingMenu.LEFT, SlidingMenu.LEFT_RIGHT -> content!!.left - behindWidth
        SlidingMenu.RIGHT -> content!!.left
        else -> 0
    }

    fun getAbsRightBound(content: View?): Int = when (_mode) {
        SlidingMenu.LEFT -> content!!.left
        SlidingMenu.RIGHT, SlidingMenu.LEFT_RIGHT -> content!!.left + behindWidth
        else -> 0
    }

    fun marginTouchAllowed(content: View?, x: Int): Boolean {
        val left = content!!.left
        val right = content.right
        return when (_mode) {
            SlidingMenu.LEFT -> (x >= left && x <= _marginThreshold + left)
            SlidingMenu.RIGHT -> (x <= right && x >= right - _marginThreshold)
            SlidingMenu.LEFT_RIGHT -> (x >= left && x <= _marginThreshold + left) || (x <= right && x >= right - _marginThreshold)
            else -> false
        }
    }

    fun menuOpenTouchAllowed(content: View?, currPage: Int, x: Float): Boolean = when (_touchMode) {
        SlidingMenu.TOUCHMODE_FULLSCREEN -> true
        SlidingMenu.TOUCHMODE_MARGIN -> menuTouchInQuickReturn(content, currPage, x)
        else -> false
    }

    fun menuTouchInQuickReturn(content: View?, currPage: Int, x: Float): Boolean =
            if (_mode == SlidingMenu.LEFT || (_mode == SlidingMenu.LEFT_RIGHT && currPage == 0)) {
                x >= content!!.left
            } else if (_mode == SlidingMenu.RIGHT || (_mode == SlidingMenu.LEFT_RIGHT && currPage == 2)) {
                x <= content!!.right
            } else {
                false
            }

    fun menuClosedSlideAllowed(dx: Float): Boolean = when (_mode) {
        SlidingMenu.LEFT -> dx > 0
        SlidingMenu.RIGHT -> dx < 0
        SlidingMenu.LEFT_RIGHT -> true
        else -> false
    }

    fun menuOpenSlideAllowed(dx: Float): Boolean = when (_mode) {
        SlidingMenu.LEFT -> dx < 0
        SlidingMenu.RIGHT -> dx > 0
        SlidingMenu.LEFT_RIGHT -> true
        else -> false
    }

    fun drawShadow(content: View?, canvas: Canvas?) {
        if (_shadowDrawable == null || _shadowWidth <= 0) {
            return
        }
        var left = 0
        if (_mode == SlidingMenu.LEFT) {
            left = content!!.left - _shadowWidth
        } else if (_mode == SlidingMenu.RIGHT) {
            left = content!!.right
        } else if (_mode == SlidingMenu.LEFT_RIGHT) {
            if (_secondaryShadowDrawable != null) {
                left = content!!.right
                _secondaryShadowDrawable!!.setBounds(left - _shadowWidth, 0, left, height)
                _secondaryShadowDrawable!!.draw(canvas)
            }
            left = content!!.left - _shadowWidth
        }
        _shadowDrawable?.setBounds(left, 0, left + _shadowWidth, height)
        _shadowDrawable?.draw(canvas)
    }

    fun drawFade(content: View?, canvas: Canvas?, openPercent: Float) {
        if (!_fadeEnabled) {
            return
        }
        val alpha = (_fadeDegree * 255 * Math.abs(1 - openPercent)).toInt()
        _fadePaint.color = Color.argb(alpha, 0, 0, 0)
        var left = 0
        var right = 0
        if (_mode == SlidingMenu.LEFT) {
            left = content!!.left - behindWidth
            right = content.left
        } else if (_mode == SlidingMenu.RIGHT) {
            left = content!!.right
            right = content.right + behindWidth
        } else if (_mode == SlidingMenu.LEFT_RIGHT) {
            left = content!!.left - behindWidth
            right = content.left
            canvas?.drawRect(left.toFloat(), 0.0f, right.toFloat(), height.toFloat(), _fadePaint)
            left = content.right
            right = content.right + behindWidth
        }
        canvas?.drawRect(left.toFloat(), 0.0f, right.toFloat(), height.toFloat(), _fadePaint)
    }

    fun drawSelector(content: View?, canvas: Canvas?, openPercent: Float) {
        if (!_selectorEnabled) {
            return
        }
        if (_selectorDrawable != null && _selectedView != null) {
            val tag = _selectedView!!.getTag(R.id.selected_view) as String
            if (tag == TAG + "SelectedView") {
                canvas?.save()
                var left: Int
                var right: Int
                val offset = (_selectorDrawable!!.width * openPercent).toInt()
                if (_mode == SlidingMenu.LEFT) {
                    right = content!!.left
                    left = right - offset
                    canvas?.clipRect(left, 0, right, height)
                    canvas?.drawBitmap(_selectorDrawable, left.toFloat(), selectorTop.toFloat(), null)
                } else if (_mode == SlidingMenu.RIGHT) {
                    left = content!!.right
                    right = left + offset
                    canvas?.clipRect(left, 0, right, height)
                    canvas?.drawBitmap(_selectorDrawable, (right - _selectorDrawable!!.width).toFloat(), selectorTop.toFloat(), null)
                }
                canvas?.restore()
            }
        }
    }

    private var selectorTop: Int = 0
        get() {
            var y = _selectedView!!.top
            y += ((_selectedView!!.height - _selectorDrawable!!.height) * 1.0f / 2).toInt()
            return y
        }
}