package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.content.res.TypedArray
import android.graphics.Matrix
import android.graphics.drawable.Drawable
import android.view.animation.Animation
import android.view.animation.RotateAnimation
import android.widget.ImageView
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/30/16.
 */
class RotateLoadingLayout: LoadingLayout {

    companion object {
        val ROTATION_ANIMATION_DURATION = 1200L
    }

    private var _rotateAnimation: Animation? = null
    private var _headerImageMatrix: Matrix? = null
    private var _rotationPivotX = 0.0f
    private var _rotationPivotY = 0.0f
    private var _rotateDrawableWhilePulling = false

    constructor(context: Context, mode: PullToRefreshBase.Mode, scrollDirection: PullToRefreshBase.Orientation, attrs: TypedArray?): super(context, mode, scrollDirection, attrs) {
        _rotateDrawableWhilePulling = attrs!!.getBoolean(R.styleable.PullToRefresh_ptrRotateDrawableWhilePulling, true)
        headerImage?.scaleType = ImageView.ScaleType.MATRIX
        _headerImageMatrix = Matrix()
        headerImage?.imageMatrix = _headerImageMatrix
        _rotateAnimation = RotateAnimation(0.0f, 720.0f, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f)
        _rotateAnimation?.interpolator = ANIMATION_INTERPOLATOR
        _rotateAnimation?.duration = ROTATION_ANIMATION_DURATION
        _rotateAnimation?.repeatCount = Animation.INFINITE
        _rotateAnimation?.repeatMode = Animation.RESTART
    }

    override fun getDefaultDrawableResId(): Int = R.drawable.default_ptr_rotate

    override fun onLoadingDrawableSet(imageDrawable: Drawable?) {
        if (imageDrawable != null) {
            _rotationPivotX = Math.round(imageDrawable.intrinsicWidth / 2.0f).toFloat()
            _rotationPivotY = Math.round(imageDrawable.intrinsicHeight / 2.0f).toFloat()
        }
    }

    override fun onPullImpl(scaleOfLayout: Float) {
        val angle = if (_rotateDrawableWhilePulling) scaleOfLayout * 90.0f else Math.max(0.0f, Math.min(180.0f, scaleOfLayout * 360.0f - 180.0f))
        _headerImageMatrix?.setRotate(angle, _rotationPivotX, _rotationPivotY)
        headerImage?.imageMatrix = _headerImageMatrix
    }

    override fun pullToRefreshImpl() { }

    override fun refreshingImpl() {
        headerImage?.startAnimation(_rotateAnimation)
    }

    override fun releaseToRefreshImpl() { }

    override fun resetImpl() {
        headerImage?.clearAnimation()
        resetImageRotation()
    }

    private fun resetImageRotation() {
        if (_headerImageMatrix != null) {
            _headerImageMatrix?.reset()
            headerImage?.imageMatrix = _headerImageMatrix
        }
    }
}