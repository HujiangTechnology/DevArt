package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.content.res.TypedArray
import android.graphics.Matrix
import android.graphics.drawable.Drawable
import android.view.View
import android.view.animation.Animation
import android.view.animation.RotateAnimation
import android.widget.ImageView
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/30/16.
 */
class FlipLoadingLayout : LoadingLayout {

    companion object {
        val FLIP_ANIMATION_DURATION = 150L
    }

    private var _rotateAnimation: Animation? = null
    private var _resetRotateAnimation: Animation? = null

    constructor(context: Context, mode: PullToRefreshBase.Mode, scrollDirection: PullToRefreshBase.Orientation, attrs: TypedArray?) : super(context, mode, scrollDirection, attrs) {
        val rotateAngle = if (mode == PullToRefreshBase.Mode.PULL_FROM_START) -180.0f else 180.0f
        _rotateAnimation = RotateAnimation(0.0f, rotateAngle, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f)
        _rotateAnimation?.interpolator = ANIMATION_INTERPOLATOR
        _rotateAnimation?.duration = FLIP_ANIMATION_DURATION
        _rotateAnimation?.fillAfter = true
        _resetRotateAnimation = RotateAnimation(rotateAngle, 0.0f, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f)
        _resetRotateAnimation?.interpolator = ANIMATION_INTERPOLATOR
        _resetRotateAnimation?.duration = FLIP_ANIMATION_DURATION
        _resetRotateAnimation?.fillAfter = true
    }

    override fun getDefaultDrawableResId(): Int = R.drawable.default_ptr_flip

    override fun onLoadingDrawableSet(imageDrawable: Drawable?) {
        if (imageDrawable != null) {
            val dHeight = imageDrawable.intrinsicHeight
            val dWidth = imageDrawable.intrinsicWidth
            val lp = headerImage!!.layoutParams
            lp.width = Math.max(dHeight, dWidth)
            lp.height = lp.width
            headerImage?.requestLayout()
            headerImage?.scaleType = ImageView.ScaleType.MATRIX
            val matrix = Matrix()
            matrix.postTranslate((lp.width - dWidth) / 2.0f, (lp.height - dHeight) / 2.0f)
            matrix.postRotate(getDrawableRotationAngle(), lp.width / 2.0f, lp.height / 2.0f)
            headerImage?.imageMatrix = matrix
        }
    }

    override fun onPullImpl(scaleOfLayout: Float) { }

    override fun pullToRefreshImpl() {
        if (_rotateAnimation == headerImage?.animation) {
            headerImage?.startAnimation(_resetRotateAnimation)
        }
    }

    override fun refreshingImpl() {
        headerImage?.clearAnimation()
        headerImage?.visibility = View.INVISIBLE
        headerProgress?.visibility = View.VISIBLE
    }

    override fun releaseToRefreshImpl() {
        headerImage?.startAnimation(_rotateAnimation)
    }

    override fun resetImpl() {
        headerImage?.clearAnimation()
        headerProgress?.visibility = View.GONE
        headerImage?.visibility = View.VISIBLE
    }

    private fun getDrawableRotationAngle(): Float {
        var angle = 0.0f
        when (mode) {
            PullToRefreshBase.Mode.PULL_FROM_END -> angle = if (scrollDirection == PullToRefreshBase.Orientation.HORIZONTAL) 90.0f else 180.0f
            PullToRefreshBase.Mode.PULL_FROM_START -> if (scrollDirection == PullToRefreshBase.Orientation.HORIZONTAL) { angle = 270.0f }
            else -> { }
        }
        return angle
    }


}