package com.hujiang.devart.component.sliding

import android.app.Activity
import android.content.Context
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.graphics.Point
import android.graphics.Rect
import android.graphics.drawable.Drawable
import android.os.Build
import android.os.Handler
import android.os.Parcel
import android.os.Parcelable
import android.support.v4.view.ViewPager
import android.util.AttributeSet
import android.view.*
import android.widget.FrameLayout
import android.widget.RelativeLayout
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/24/16.
 */
class SlidingMenu : RelativeLayout {

    companion object {
        val SLIDING_WINDOW = 0
        val SLIDING_CONTENT = 1
        val TOUCHMODE_MARGIN = 0
        val TOUCHMODE_FULLSCREEN = 1
        val TOUCHMODE_NONE = 2
        val LEFT = 0
        val RIGHT = 1
        val LEFT_RIGHT = 2
    }

    private var _actionbarOverlay = false
    private var _viewAbove: CustomViewAbove? = null
    private var _viewBehind: CustomViewBehind? = null
    private var _openListener: OnSlidingOpenListener? = null
    var openListener: OnSlidingOpenListener?
        get() = _openListener
        set(value) {
            _openListener = value
        }
    private var _closeListener: OnSlidingCloseListener? = null
    var closeListener: OnSlidingCloseListener?
        get() = _closeListener
        set(value) {
            _closeListener = value
        }
    private var _handler = Handler()

    constructor(context: Context) : this(context, null)

    constructor(context: Context, attrs: AttributeSet?) : this(context, attrs, 0)

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int) : super(context, attrs, defStyle) {

        val behindParams = LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT)
        _viewBehind = CustomViewBehind(context)
        addView(_viewBehind, behindParams)
        val aboveParams = LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT)
        _viewAbove = CustomViewAbove(context)
        addView(_viewAbove, aboveParams)

        _viewAbove?.viewBehind = _viewBehind
        _viewBehind?.viewAbove = _viewAbove
        _viewAbove?.onPageChangeListener = object : ViewPager.OnPageChangeListener {

            val POSITION_OPEN = 0
            val POSITION_CLOSE = 1

            override fun onPageScrolled(position: Int, positionOffset: Float, positionOffsetPixels: Int) {
            }

            override fun onPageSelected(position: Int) {
                if (position == POSITION_OPEN) {
                    _openListener?.onSlidingOpen()
                } else if (position == POSITION_CLOSE) {
                    _closeListener?.onSlidingClose()
                }
            }

            override fun onPageScrollStateChanged(state: Int) { }
        }

        val ta = context.obtainStyledAttributes(attrs, R.styleable.SlidingMenu)
        val mode = ta.getInt(R.styleable.SlidingMenu_mode, LEFT)
        this.mode = mode
        val viewAbove = ta.getResourceId(R.styleable.SlidingMenu_viewAbove, -1)
        if (viewAbove != -1) {
            setAboveContent(viewAbove)
        } else {
            aboveContent = FrameLayout(context)
        }
        val viewBehind = ta.getResourceId(R.styleable.SlidingMenu_viewBehind, -1)
        if (viewBehind != -1) {
            setBehindMenu(viewBehind)
        } else {
            behindMenu = FrameLayout(context)
        }
        val touchModeAbove = ta.getInt(R.styleable.SlidingMenu_touchModeAbove, TOUCHMODE_MARGIN)
        this.touchModeAbove = touchModeAbove
        val touchModeBehind = ta.getInt(R.styleable.SlidingMenu_touchModeBehind, TOUCHMODE_MARGIN)
        this.touchModeBehind = touchModeBehind
        val offsetBehind = ta.getDimension(R.styleable.SlidingMenu_behindOffset, -1.0f).toInt()
        val widthBehind = ta.getDimension(R.styleable.SlidingMenu_behindWidth, -1.0f).toInt()
        if (offsetBehind != -1 && widthBehind != -1) {
            throw IllegalStateException("Cannot set both behindOffset and behindWidth for a SlidingMenu")
        } else if (offsetBehind != -1) {
            behindOffset = offsetBehind
        } else if (widthBehind != -1) {
            setBehindWidth(widthBehind)
        } else {
            behindOffset = 0
        }
        val scrollOffsetBehind = ta.getFloat(R.styleable.SlidingMenu_behindScrollScale, 0.33f)
        behindScrollScale = scrollOffsetBehind
        val shadowRes = ta.getResourceId(R.styleable.SlidingMenu_shadowDrawable, -1)
        if (shadowRes != -1) {
            setBehindShadowDrawable(shadowRes)
        }
        val shadowWidth = ta.getDimension(R.styleable.SlidingMenu_shadowWidth, 0.0f).toInt()
        behindShadowWidth = shadowWidth
        val fadeEnabled = ta.getBoolean(R.styleable.SlidingMenu_fadeEnabled, true)
        behindFadeEnabled = fadeEnabled
        val fadeDeg = ta.getFloat(R.styleable.SlidingMenu_fadeDegree, 0.33f)
        behindFadeDegree = fadeDeg
        val selectorEnabled = ta.getBoolean(R.styleable.SlidingMenu_selectorEnabled, false)
        behindSelectorEnabled = selectorEnabled
        val selectorRes = ta.getResourceId(R.styleable.SlidingMenu_selectorDrawable, -1)
        if (selectorRes != -1) {
            setBehindSelectorBitmap(selectorRes)
        }
        ta.recycle()
    }

    constructor(activity: Activity, slideStyle: Int) : this(activity, null) {
        attachToActivity(activity, slideStyle)
    }

    fun attachToActivity(activity: Activity, slideStyle: Int) = attachToActivity(activity, slideStyle, false)

    fun attachToActivity(activity: Activity, slideStyle: Int, actionbarOverlay: Boolean) {
        if (slideStyle != SLIDING_WINDOW && slideStyle != SLIDING_CONTENT) {
            throw IllegalArgumentException("slideStyle must be either SLIDING_WINDOW or SLIDING_CONTENT")
        }
        if (parent != null) {
            throw IllegalStateException("This SlidingMenu appears to already be attached")
        }

        val a = activity.theme.obtainStyledAttributes(intArrayOf(android.R.attr.windowBackground))
        val background = a.getResourceId(0, 0)
        a.recycle()

        when (slideStyle) {
            SLIDING_WINDOW -> {
                _actionbarOverlay = false
                val decor = activity.window.decorView as ViewGroup
                val decorChild = decor.getChildAt(0) as ViewGroup
                decorChild.setBackgroundResource(background)
                decor.removeView(decorChild)
                decor.addView(this)
                aboveContent = decorChild
            }

            SLIDING_CONTENT -> {
                _actionbarOverlay = actionbarOverlay
                val contentParent = activity.findViewById(android.R.id.content) as ViewGroup
                val content = contentParent.getChildAt(0)
                contentParent.removeView(content)
                contentParent.addView(this)
                aboveContent = content
                if (content.background == null) {
                    content.setBackgroundResource(background)
                }
            }
        }
    }

    var aboveContent: View?
        get() = _viewAbove!!.content
        set(value) {
            _viewAbove!!.content = value
            showContent()
        }

    fun setAboveContent(res: Int) {
        aboveContent = LayoutInflater.from(context).inflate(res, null)
    }

    var behindMenu: View?
        get() = _viewBehind!!.content
        set(value) {
            _viewBehind!!.content = value
        }

    fun setBehindMenu(res: Int) {
        behindMenu = LayoutInflater.from(context).inflate(res, null)
    }

    var behindSecondaryMenu: View?
        get() = _viewBehind!!.secondaryContent
        set(value) {
            _viewBehind!!.secondaryContent = value
        }

    fun setBehindSecondaryMenu(res: Int) {
        behindSecondaryMenu = LayoutInflater.from(context).inflate(res, null)
    }

    var isSlidingEnabled: Boolean
        get() = _viewAbove!!.isSlideEnabled
        set(value) {
            _viewAbove!!.isSlideEnabled = value
        }

    var mode: Int
        get() = _viewBehind!!.mode
        set(value) {
            if (value != LEFT && value != RIGHT && value != LEFT_RIGHT) {
                throw IllegalStateException("SlidingMenu mode must be LEFT, RIGHT, or LEFT_RIGHT")
            }
            _viewBehind!!.mode = value
        }

    fun setStatic(b: Boolean) {
        if (b) {
            isSlidingEnabled = false
            _viewAbove!!.viewBehind = null
            _viewAbove!!.setCurrentItem(1, false)

        } else {
            _viewAbove!!.setCurrentItem(1, false)
            _viewAbove!!.viewBehind = _viewBehind
            isSlidingEnabled = true
        }
    }

    fun showMenu() = showMenu(true)

    fun showMenu(animate: Boolean) = _viewAbove!!.setCurrentItem(0, animate)

    fun showSecondaryMenu() = showSecondaryMenu(true)

    fun showSecondaryMenu(animate: Boolean) = _viewAbove!!.setCurrentItem(2, animate)

    fun showContent() = showContent(true)

    fun showContent(animate: Boolean) = _viewAbove!!.setCurrentItem(1, animate)

    fun toggle() = toggle(true)

    fun toggle(animate: Boolean) = if (isMenuShowing) {
        showContent(animate)
    } else {
        showMenu(animate)
    }

    var isMenuShowing: Boolean = false
        get() = _viewAbove?.currentItem == 0 || _viewAbove?.currentItem == 2

    var isSecondaryMenuShowing: Boolean = false
        get() = _viewAbove?.currentItem == 2

    var behindOffset: Int
        get() = (_viewBehind!!.layoutParams as RelativeLayout.LayoutParams).rightMargin
        set(value) {
            _viewBehind!!.widthOffset = value
        }

    fun setBehindOffsetRes(resId: Int) {
        val i = context.resources.getDimension(resId).toInt()
        behindOffset = i
    }

    fun setAboveOffset(i: Int) = _viewAbove!!.setAboveOffset(i)

    fun setAboveOffsetRes(resId: Int) {
        val i = context.resources.getDimension(resId).toInt()
        setAboveOffset(i)
    }

    @Suppress("DEPRECATION")
    fun setBehindWidth(i: Int) {
        var width: Int
        val display = (context.getSystemService(Context.WINDOW_SERVICE) as WindowManager).defaultDisplay
        try {
            val parameter = Point()
            display.getSize(parameter)
            width = parameter.x
        } catch (e: Exception) {
            width = display.width
        }
        behindOffset = width - i
    }

    fun setBehindWidthRes(res: Int) {
        val i = context.resources.getDimension(res).toInt()
        setBehindWidth(i)
    }

    var behindScrollScale: Float
        get() = _viewBehind!!.scrollScale
        set(value) {
            if (value < 0 && value > 1) {
                throw IllegalStateException("ScrollScale must be between 0 and 1")
            }
            _viewBehind!!.scrollScale = value
        }

    var touchModeAbove: Int
        get() = _viewAbove!!.touchMode
        set(value) {
            if (value != TOUCHMODE_FULLSCREEN && value != TOUCHMODE_MARGIN && value != TOUCHMODE_NONE) {
                throw IllegalStateException("TouchMode must be set to eitherTOUCHMODE_FULLSCREEN or TOUCHMODE_MARGIN or TOUCHMODE_NONE.")
            }
            _viewAbove!!.touchMode = value
        }


    fun setBehindCanvasTransformer(t: CanvasTransformer?) {
        _viewBehind!!.transformer = t
    }

    var touchModeBehind: Int
        get() = _viewBehind!!.touchMode
        set(value) {
            if (value != TOUCHMODE_FULLSCREEN && value != TOUCHMODE_MARGIN && value != TOUCHMODE_NONE) {
                throw IllegalStateException("TouchMode must be set to eitherTOUCHMODE_FULLSCREEN or TOUCHMODE_MARGIN or TOUCHMODE_NONE.")
            }
            _viewBehind!!.touchMode = value
        }

    var behindShadowDrawable: Drawable?
        get() = _viewBehind!!.shadowDrawable
        set(value) {
            _viewBehind!!.shadowDrawable = value
        }

    fun setBehindShadowDrawable(resId: Int) {
        behindShadowDrawable = resources.getDrawable(resId)
    }

    var behindSecondaryShadowDrawable: Drawable?
        get() = _viewBehind!!.secondaryShadowDrawable
        set(value) {
            _viewBehind!!.secondaryShadowDrawable = value
        }

    fun setBehindSecondaryShadowDrawable(resId: Int) {
        behindSecondaryShadowDrawable = resources.getDrawable(resId)
    }

    var behindShadowWidth: Int
        get() = _viewBehind!!.shadowWidth
        set(value) {
            _viewBehind!!.shadowWidth = value
        }

    fun setShadowWidthRes(resId: Int) {
        behindShadowWidth = resources.getDimension(resId).toInt()
    }

    var behindFadeEnabled: Boolean
        get() = _viewBehind!!.fadeEnabled
        set(value) {
            _viewBehind!!.fadeEnabled = value
        }


    var behindFadeDegree: Float
        get() = _viewBehind!!.fadeDegree
        set(value) {
            _viewBehind!!.fadeDegree = value
        }


    var behindSelectorEnabled: Boolean
        get() = _viewBehind!!.selectorEnabled
        set(value) {
            _viewBehind!!.selectorEnabled = value
        }

    var behindSelectedView: View?
        get() = _viewBehind!!.selectedView
        set(value) {
            _viewBehind!!.selectedView = value
        }

    var behindSelectorBitmap: Bitmap?
        get() = _viewBehind!!.selectorDrawable
        set(value) {
            _viewBehind!!.selectorDrawable = value
        }

    fun setBehindSelectorBitmap(resId: Int) {
        behindSelectorBitmap = BitmapFactory.decodeResource(resources, resId)
    }

    fun addIgnoredView(v: View?) = _viewAbove!!.addIgnoredView(v)

    fun removeIgnoredView(v: View?) = _viewAbove!!.removeIgnoredView(v)

    fun clearIgnoredViews() = _viewAbove!!.clearIgnoredViews()


    var aboveOpenedListener: OnSlidingOpenedListener?
        get() = _viewAbove!!.openedListener
        set(value) {
            _viewAbove!!.openedListener = value
        }

    var aboveClosedListener: OnSlidingClosedListener?
        get() = _viewAbove!!.closedListener
        set(value) {
            _viewAbove!!.closedListener = value
        }


    override fun onSaveInstanceState(): Parcelable? {
        val superState = super.onSaveInstanceState()
        val ss = SavedState(superState, _viewAbove!!.currentItem)
        return ss
    }

    override fun onRestoreInstanceState(state: Parcelable?) {
        val ss = state as SavedState
        super.onRestoreInstanceState(ss.superState)
        _viewAbove!!.setCurrentItem(ss.item, false)
    }

    override fun fitSystemWindows(insets: Rect?): Boolean {
        val leftPadding = insets!!.left
        val rightPadding = insets.right
        val topPadding = insets.top
        val bottomPadding = insets.bottom
        if (!_actionbarOverlay) {
            setPadding(leftPadding, topPadding, rightPadding, bottomPadding)
        }
        return true
    }

    fun manageLayers(percentOpen: Float) {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB) {
            return
        }
        val layer = percentOpen > 0.0f && percentOpen < 1.0f
        val layerType = if (layer) {
            View.LAYER_TYPE_HARDWARE
        } else {
            View.LAYER_TYPE_NONE
        }

        if (layerType != aboveContent!!.layerType) {
            _handler.post {
                aboveContent!!.setLayerType(layerType, null)
                behindMenu!!.setLayerType(layerType, null)
                behindSecondaryMenu?.setLayerType(layerType, null)
            }
        }
    }

    class SavedState : BaseSavedState {

        companion object {
            val CREATOR = object : Parcelable.Creator<SavedState> {
                override fun newArray(size: Int): Array<SavedState?> {
                    return arrayOfNulls(size)
                }

                override fun createFromParcel(source: Parcel?): SavedState? {
                    return SavedState(source!!)
                }

            }
        }

        private var _item: Int = 0
        var item: Int = 0
            get() = _item

        constructor(pin: Parcel) : super(pin) {
            _item = pin.readInt()
        }

        constructor(superState: Parcelable, item: Int) : super(superState) {
            _item = item
        }

        override fun writeToParcel(out: Parcel, flags: Int) {
            super.writeToParcel(out, flags)
            out.writeInt(_item)
        }
    }
}
