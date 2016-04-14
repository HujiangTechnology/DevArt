package com.hujiang.devart.component.glassbar

import android.content.Context
import android.graphics.Bitmap
import android.graphics.drawable.Drawable
import android.util.TypedValue
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.view.ViewTreeObserver
import android.widget.*
import com.hujiang.devart.R
import com.hujiang.devart.utils.ImageUtils

/**
 * Created by rarnu on 4/14/16.
 */
class GlassActionBarHelper: ViewTreeObserver.OnGlobalLayoutListener, NotifyingScrollView.OnScrollChangedListener, BlurTask.Listener, ListViewScrollObserver.OnListViewScrollListener {



    private var _contentLayout = 0
    private var _frame: FrameLayout? = null
    private var _content: View? = null
    private var _adapter: ListAdapter? = null
    private var _blurredOverlay: ImageView? = null
    private var _actionBarHeight = 0
    private var _width = 0
    private var _height = 0
    private var _scaled: Bitmap? = null
    private var _blurRadius = GlassActionBar.DEFAULT_BLUR_RADIUS
    private var _blurTask: BlurTask? = null
    private var _lastScrollPosition = -1
    private var _scrollView: NotifyingScrollView? = null
    private var _listView: ListView? = null
    private var _downSampling = GlassActionBar.DEFAULT_DOWNSAMPLING
    private var _windowBackground: Drawable? = null


    fun contentLayout(layout: Int): GlassActionBarHelper? {
        _contentLayout = layout
        return this
    }

    fun contentLayout(layout: Int, adapter: ListAdapter?): GlassActionBarHelper? {
        _contentLayout = layout
        _adapter = adapter
        return this
    }

    override fun onGlobalLayout() {
        if (_width != 0) {
            return
        }
        val widthMeasureSpec = View.MeasureSpec.makeMeasureSpec(_frame!!.width, View.MeasureSpec.AT_MOST)
        var heightMeasureSpec: Int
        if (_listView != null) {
            heightMeasureSpec = View.MeasureSpec.makeMeasureSpec(_frame!!.height, View.MeasureSpec.EXACTLY)
        } else {
            heightMeasureSpec = View.MeasureSpec.makeMeasureSpec(ViewGroup.LayoutParams.WRAP_CONTENT, View.MeasureSpec.UNSPECIFIED)
        }
        _content?.measure(widthMeasureSpec, heightMeasureSpec)
        _width = _frame!!.width
        _height = _content!!.measuredHeight
        _lastScrollPosition = if (_scrollView != null) _scrollView!!.scrollY else 0
        invalidate()
    }

    fun invalidate() {
        _scaled = null
        computeBlurOverlay()
        updateBlurOverlay(_lastScrollPosition, true)
    }

    private fun updateBlurOverlay(top: Int, force: Boolean) {
        if (_scaled == null) {
            return
        }
        var ntop = top
        if (ntop < 0) {
            ntop = 0
        }
        if (!force && _lastScrollPosition == top) {
            return
        }
        _lastScrollPosition = ntop
        val actionBarSection = Bitmap.createBitmap(_scaled, 0, top / _downSampling, _width / _downSampling, _actionBarHeight / _downSampling)
        var blurredBitmap: Bitmap?
        if (isBlurTaskFinished()) {
            blurredBitmap = actionBarSection
        } else {
            blurredBitmap = Blur.apply(_frame!!.context, actionBarSection)
        }
        val enlarged = Bitmap.createScaledBitmap(blurredBitmap, _width, _actionBarHeight, false)
        blurredBitmap?.recycle()
        actionBarSection?.recycle()
        _blurredOverlay?.setImageBitmap(enlarged)
    }

    private fun isBlurTaskFinished(): Boolean = _blurTask == null

    private fun computeBlurOverlay() {
        if (_scaled != null) {
            return
        }
        var scrollPosition = 0
        if (_scrollView != null) {
            scrollPosition = _scrollView!!.scrollY
        }
        val start = System.nanoTime()
        _scaled = ImageUtils.drawViewToBitmap(_scaled, _content, _width, _height, _downSampling, _windowBackground)
        startBlurTask()
        _scrollView?.scrollTo(0, scrollPosition)
    }

    private fun startBlurTask() {
        _blurTask?.cancel()
        _blurTask = BlurTask(_frame!!.context, this, _scaled, _blurRadius)
    }

    override fun onScrollChanged(who: ScrollView?, l: Int, t: Int, oldl: Int, oldt: Int) {
        onNewScroll(t)
    }

    private fun onNewScroll(t: Int) {
        updateBlurOverlay(t, false)
    }

    override fun onBlurOperationFinished() {
        _blurTask = null
        updateBlurOverlay(_lastScrollPosition, true)
    }

    override fun onScrollUpDownChanged(delta: Int, scrollPosition: Int, exact: Boolean) {
        if (exact) {
            onNewScroll(-scrollPosition)
        }
    }

    override fun onScrollIdle() { }

    fun createView(context: Context): View? {
        val attrs = intArrayOf(android.R.attr.windowBackground)
        val outValue = TypedValue()
        context.theme.resolveAttribute(android.R.attr.windowBackground, outValue, true)
        val style = context.theme.obtainStyledAttributes(outValue.resourceId, attrs)
        _windowBackground = style.getDrawable(0)
        style.recycle()
        val inflater = LayoutInflater.from(context)
        _frame = inflater.inflate(R.layout.frame_glass, null) as FrameLayout
        _content = inflater.inflate(_contentLayout, _frame as ViewGroup, false)
        _frame?.addView(_content, 0)
        _frame?.viewTreeObserver?.addOnGlobalLayoutListener(this)
        _blurredOverlay = _frame?.findViewById(R.id.blurredOverlay) as ImageView
        if (_content is NotifyingScrollView) {
            _scrollView = _content as NotifyingScrollView
            _scrollView?.setOnScrollChangedListener(this)
        } else if (_content is ListView) {
            _listView = _content as ListView
            _listView?.adapter = _adapter
            val observer = ListViewScrollObserver(_listView)
            observer.setOnScrollUpAndDownListener(this)
        }
        _actionBarHeight = getActionBarHeight(context)
        return _frame
    }

    protected fun getActionBarHeight(context: Context): Int {
        val outValue = TypedValue()
        context.theme.resolveAttribute(android.R.attr.actionBarSize, outValue, true)
        return context.resources.getDimensionPixelSize(outValue.resourceId)
    }

    fun setBlurRadius(newValue: Int) {
        if (!GlassActionBar.isValidBlurRadius(newValue)) {
            throw IllegalArgumentException("Invalid blur radius")
        }
        if (_blurRadius == newValue) {
            return
        }
        _blurRadius = newValue
        invalidate()
    }

    fun getBlurRadius(): Int = _blurRadius

    fun setDownsampling(newValue: Int) {
        if (!GlassActionBar.isValidDownsampling(newValue)) {
            throw IllegalArgumentException("Invalid downsampling")
        }
        if (_downSampling == newValue) {
            return
        }
        _downSampling = newValue
        invalidate()
    }

    fun getDownsampling(): Int = _downSampling
}