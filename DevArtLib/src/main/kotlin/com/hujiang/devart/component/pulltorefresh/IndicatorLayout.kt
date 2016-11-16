package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.graphics.Matrix
import android.view.View
import android.view.animation.Animation
import android.view.animation.AnimationUtils
import android.view.animation.LinearInterpolator
import android.view.animation.RotateAnimation
import android.widget.FrameLayout
import android.widget.ImageView
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/30/16.
 */
class IndicatorLayout: FrameLayout, Animation.AnimationListener {

    companion object {
        val DEFAULT_ROTATION_ANIMATION_DURATION = 150L
    }

    private var _inAnim: Animation? = null
    private var _outAnim: Animation? = null
    private var _arrowImageView: ImageView? = null

    private var _rotateAnimation: Animation? = null
    private var _resetRotateAnimation: Animation? = null

    constructor(context: Context, mode: PullToRefreshBase.Mode): super(context) {
        _arrowImageView = ImageView(context)
        val arrowD = resources.getDrawable(R.drawable.indicator_arrow, context.theme)
        _arrowImageView?.setImageDrawable(arrowD)
        val padding = resources.getDimensionPixelSize(R.dimen.indicator_internal_padding)
        _arrowImageView?.setPadding(padding, padding, padding, padding)
        addView(_arrowImageView)

        var inAnimResId: Int
        var outAnimResId: Int
        when (mode) {
            PullToRefreshBase.Mode.PULL_FROM_END -> {
                inAnimResId = R.anim.slide_in_from_bottom
                outAnimResId = R.anim.slide_out_to_bottom
                setBackgroundResource(R.drawable.indicator_bg_bottom)
                _arrowImageView?.scaleType = ImageView.ScaleType.MATRIX
                val matrix = Matrix()
                matrix.setRotate(180f, arrowD.intrinsicWidth / 2.0f, arrowD.intrinsicHeight / 2.0f)
                _arrowImageView?.imageMatrix = matrix
            }
            else -> {
                inAnimResId = R.anim.slide_in_from_top
                outAnimResId = R.anim.slide_out_to_top
                setBackgroundResource(R.drawable.indicator_bg_top)
            }
        }
        _inAnim = AnimationUtils.loadAnimation(context, inAnimResId)
        _inAnim?.setAnimationListener(this)
        _outAnim = AnimationUtils.loadAnimation(context, outAnimResId)
        _outAnim?.setAnimationListener(this)
        val interpolator = LinearInterpolator()
        _rotateAnimation = RotateAnimation(0.0f, -180.0f, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f)
        _rotateAnimation?.interpolator = interpolator
        _rotateAnimation?.duration = DEFAULT_ROTATION_ANIMATION_DURATION
        _rotateAnimation?.fillAfter = true
        _resetRotateAnimation = RotateAnimation(-180.0f, 0.0f, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f)
        _resetRotateAnimation?.interpolator = interpolator
        _resetRotateAnimation?.duration = DEFAULT_ROTATION_ANIMATION_DURATION
        _resetRotateAnimation?.fillAfter = true
    }

    fun isVisible(): Boolean {
        val currentAnim = animation
        if (currentAnim != null) {
            return _inAnim == currentAnim
        }
        return visibility == View.VISIBLE
    }

    fun hide() = startAnimation(_outAnim)

    fun show() {
        _arrowImageView?.clearAnimation()
        startAnimation(_inAnim)
    }

    override fun onAnimationEnd(animation: Animation?) {
        if (animation == _outAnim) {
            _arrowImageView?.clearAnimation()
            visibility = View.GONE
        } else if (animation == _inAnim) {
            visibility = View.VISIBLE
        }
        clearAnimation()
    }

    override fun onAnimationRepeat(animation: Animation?) { }

    override fun onAnimationStart(animation: Animation?) {
        visibility = View.VISIBLE
    }

    fun releaseToRefresh() = _arrowImageView?.startAnimation(_rotateAnimation)

    fun pullToRefresh() = _arrowImageView?.startAnimation(_resetRotateAnimation)

}