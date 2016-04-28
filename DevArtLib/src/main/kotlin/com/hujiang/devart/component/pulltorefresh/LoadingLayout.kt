package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.content.res.ColorStateList
import android.content.res.TypedArray
import android.graphics.Typeface
import android.graphics.drawable.AnimationDrawable
import android.graphics.drawable.Drawable
import android.text.TextUtils
import android.util.TypedValue
import android.view.Gravity
import android.view.LayoutInflater
import android.view.View
import android.view.animation.LinearInterpolator
import android.widget.FrameLayout
import android.widget.ImageView
import android.widget.ProgressBar
import android.widget.TextView
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/30/16.
 */
abstract class LoadingLayout: FrameLayout, ILoadingLayout {

    companion object {
        val ANIMATION_INTERPOLATOR = LinearInterpolator()
    }

    private var _innerLayout: FrameLayout? = null
    protected var headerImage: ImageView? = null
    protected var headerProgress: ProgressBar? = null
    private var _useIntrinsicAnimation = false
    private var _headerText: TextView? = null
    private var _subHeaderText: TextView? = null
    protected var mode = PullToRefreshBase.Mode.getDefault()
    protected var scrollDirection = PullToRefreshBase.Orientation.VERTICAL
    private var _pullLabel: CharSequence? = null
    private var _refreshingLabel: CharSequence? = null
    private var _releaseLabel: CharSequence? = null

    protected abstract fun getDefaultDrawableResId(): Int

    protected abstract fun onLoadingDrawableSet(imageDrawable: Drawable?)

    protected abstract fun onPullImpl(scaleOfLayout: Float)

    protected abstract fun pullToRefreshImpl()

    protected abstract fun refreshingImpl()

    protected abstract fun releaseToRefreshImpl()

    protected abstract fun resetImpl()


    constructor(context: Context, mode: PullToRefreshBase.Mode, scrollDirection: PullToRefreshBase.Orientation, attrs: TypedArray?): super(context) {
        this.mode = mode
        this.scrollDirection = scrollDirection
        when (scrollDirection) {
            PullToRefreshBase.Orientation.HORIZONTAL -> LayoutInflater.from(context).inflate(R.layout.pull_to_refresh_header_horizontal, this)
            else -> LayoutInflater.from(context).inflate(R.layout.pull_to_refresh_header_vertical, this)
        }
        _innerLayout = findViewById(R.id.fl_inner) as FrameLayout
        _headerText = _innerLayout?.findViewById(R.id.pull_to_refresh_text) as TextView
        headerProgress = _innerLayout?.findViewById(R.id.pull_to_refresh_progress) as ProgressBar
        _subHeaderText = _innerLayout?.findViewById(R.id.pull_to_refresh_sub_text) as TextView
        headerImage = _innerLayout?.findViewById(R.id.pull_to_refresh_image) as ImageView
        val lp = _innerLayout?.layoutParams as FrameLayout.LayoutParams
        when (mode) {
            PullToRefreshBase.Mode.PULL_FROM_END -> {
                lp.gravity = if (scrollDirection == PullToRefreshBase.Orientation.VERTICAL) Gravity.TOP else Gravity.LEFT
                _pullLabel = context.getString(R.string.pull_to_refresh_from_bottom_pull_label)
                _refreshingLabel = context.getString(R.string.pull_to_refresh_from_bottom_refreshing_label)
                _releaseLabel = context.getString(R.string.pull_to_refresh_from_bottom_release_label)
            }
            else -> {
                lp.gravity = if (scrollDirection == PullToRefreshBase.Orientation.VERTICAL) Gravity.BOTTOM else Gravity.RIGHT
                _pullLabel = context.getString(R.string.pull_to_refresh_pull_label)
                _refreshingLabel = context.getString(R.string.pull_to_refresh_refreshing_label)
                _releaseLabel = context.getString(R.string.pull_to_refresh_release_label)
            }
        }
        if (attrs!!.hasValue(R.styleable.PullToRefresh_ptrHeaderBackground)) {
            val background = attrs.getDrawable(R.styleable.PullToRefresh_ptrHeaderBackground)
            if (background != null) {
                this.background = background
            }
        }
        if (attrs.hasValue(R.styleable.PullToRefresh_ptrHeaderTextAppearance)) {
            val styleID = TypedValue()
            attrs.getValue(R.styleable.PullToRefresh_ptrHeaderTextAppearance, styleID)
            setTextAppearance(styleID.data)
        }
        if (attrs.hasValue(R.styleable.PullToRefresh_ptrSubHeaderTextAppearance)) {
            val styleID = TypedValue()
            attrs.getValue(R.styleable.PullToRefresh_ptrSubHeaderTextAppearance, styleID)
            setSubTextAppearance(styleID.data)
        }
        if (attrs.hasValue(R.styleable.PullToRefresh_ptrHeaderTextColor)) {
            val colors = attrs.getColorStateList(R.styleable.PullToRefresh_ptrHeaderTextColor)
            if (colors != null) {
                setTextColor(colors)
            }
        }
        if (attrs.hasValue(R.styleable.PullToRefresh_ptrHeaderSubTextColor)) {
            val colors = attrs.getColorStateList(R.styleable.PullToRefresh_ptrHeaderSubTextColor)
            if (colors != null) {
                setSubTextColor(colors)
            }
        }
        var imageDrawable: Drawable? = null
        if (attrs.hasValue(R.styleable.PullToRefresh_ptrDrawable)) {
            imageDrawable = attrs.getDrawable(R.styleable.PullToRefresh_ptrDrawable)
        }
        when (mode) {
            PullToRefreshBase.Mode.PULL_FROM_END -> {
                if (attrs.hasValue(R.styleable.PullToRefresh_ptrDrawableEnd)) {
                    imageDrawable = attrs.getDrawable(R.styleable.PullToRefresh_ptrDrawableEnd)
                } else if (attrs.hasValue(R.styleable.PullToRefresh_ptrDrawableBottom)) {
                    imageDrawable = attrs.getDrawable(R.styleable.PullToRefresh_ptrDrawableBottom)
                }
            }
            else -> {
                if (attrs.hasValue(R.styleable.PullToRefresh_ptrDrawableStart)) {
                    imageDrawable = attrs.getDrawable(R.styleable.PullToRefresh_ptrDrawableStart)
                } else if (attrs.hasValue(R.styleable.PullToRefresh_ptrDrawableTop)) {
                    imageDrawable = attrs.getDrawable(R.styleable.PullToRefresh_ptrDrawableTop)
                }
            }
        }
        if (imageDrawable == null) {
            imageDrawable = context.resources.getDrawable(getDefaultDrawableResId())
        }
        setLoadingDrawable(imageDrawable)
        reset()
    }

    private fun setSubHeaderText(label: CharSequence?) {
        if (_subHeaderText != null) {
            if (TextUtils.isEmpty(label)) {
                _subHeaderText?.visibility = View.GONE
            } else {
                _subHeaderText?.text = label
                if (_subHeaderText!!.visibility == View.GONE) {
                    _subHeaderText?.visibility = View.VISIBLE
                }
            }
        }
    }

    private fun setSubTextAppearance(value: Int) = _subHeaderText?.setTextAppearance(context, value)

    private fun setSubTextColor(color: ColorStateList?) { _subHeaderText?.setTextColor(color) }

    private fun setTextAppearance(value: Int) {
        _headerText?.setTextAppearance(context, value)
        _subHeaderText?.setTextAppearance(context, value)
    }

    private fun setTextColor(color: ColorStateList?) {
        _headerText?.setTextColor(color)
        _subHeaderText?.setTextColor(color)
    }

    fun setHeight(height: Int) {
        val lp = layoutParams
        lp.height = height
        requestLayout()
    }

    fun setWidth(width: Int) {
        val lp = layoutParams
        lp.width = width
        requestLayout()
    }

    fun getContentSize(): Int = when (scrollDirection) {
            PullToRefreshBase.Orientation.HORIZONTAL -> _innerLayout!!.width
            else -> _innerLayout!!.height
        }

    fun hideAllViews() {
        if (_headerText!!.visibility == View.VISIBLE) {
            _headerText?.visibility = View.INVISIBLE
        }
        if (headerProgress!!.visibility == View.VISIBLE) {
            headerProgress?.visibility = View.INVISIBLE
        }
        if (headerImage!!.visibility == View.VISIBLE) {
            headerImage?.visibility = View.INVISIBLE
        }
        if (_subHeaderText!!.visibility == View.VISIBLE) {
            _subHeaderText?.visibility = View.INVISIBLE
        }
    }

    fun reset() {
        _headerText?.text = _pullLabel
        headerImage?.visibility = View.VISIBLE
        if (_useIntrinsicAnimation) {
            (headerImage?.drawable as AnimationDrawable).stop()
        } else {
            resetImpl()
        }
        if (_subHeaderText != null) {
            if (TextUtils.isEmpty(_subHeaderText?.text)) {
                _subHeaderText?.visibility = View.GONE
            } else {
                _subHeaderText?.visibility = View.VISIBLE
            }
        }
    }

    fun onPull(scaleOfLayout: Float) {
        if (!_useIntrinsicAnimation) {
            onPullImpl(scaleOfLayout)
        }
    }

    fun pullToRefresh() {
        _headerText?.text = _pullLabel
        pullToRefreshImpl()
    }

    fun refreshing() {
        _headerText?.text = _refreshingLabel
        if (_useIntrinsicAnimation) {
            (headerImage?.drawable as AnimationDrawable).start()
        } else {
            refreshingImpl()
        }
        _subHeaderText?.visibility = View.GONE
    }

    fun releaseToRefresh() {
        _headerText?.text = _releaseLabel
        releaseToRefreshImpl()
    }

    fun showInvisibleViews() {
        if (_headerText!!.visibility == View.INVISIBLE) {
            _headerText?.visibility = View.VISIBLE
        }
        if (headerProgress!!.visibility == View.INVISIBLE) {
            headerProgress?.visibility = View.VISIBLE
        }
        if (headerImage!!.visibility == View.INVISIBLE) {
            headerImage?.visibility = View.VISIBLE
        }
        if (_subHeaderText!!.visibility == View.INVISIBLE) {
            _subHeaderText?.visibility = View.VISIBLE
        }
    }

    override fun setLoadingDrawable(drawable: Drawable?) {
        headerImage?.setImageDrawable(drawable)
        _useIntrinsicAnimation = (drawable is AnimationDrawable)
        onLoadingDrawableSet(drawable)
    }

    override fun setPullLabel(pullLabel: CharSequence?) {
        _pullLabel = pullLabel
    }

    override fun setRefreshingLabel(refreshingLabel: CharSequence?) {
        _refreshingLabel = refreshingLabel
    }

    override fun setReleaseLabel(releaseLabel: CharSequence?) {
        _releaseLabel = releaseLabel
    }

    override fun setLastUpdatedLabel(label: CharSequence?) = setSubHeaderText(label)

    override fun setTextTypeface(tf: Typeface?) {
        _headerText?.typeface = tf
    }


}