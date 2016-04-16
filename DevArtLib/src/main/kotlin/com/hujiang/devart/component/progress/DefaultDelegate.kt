package com.hujiang.devart.component.progress

import android.animation.Animator
import android.animation.ArgbEvaluator
import android.animation.ValueAnimator
import android.graphics.Canvas
import android.graphics.Paint
import android.view.animation.Interpolator
import android.view.animation.LinearInterpolator

/**
 * Created by rarnu on 4/15/16.
 */
class DefaultDelegate: ProgressBarDelegate {

    companion object {
        private val COLOR_EVALUATOR = ArgbEvaluator()
        private val END_INTERPOLATOR = LinearInterpolator()
        private val ROTATION_ANIMATOR_DURATION = 2000L
        private val SWEEP_ANIMATOR_DURATION = 600L
        private val END_ANIMATOR_DURATION = 200L
    }

    private var _sweepAppearingAnimator: ValueAnimator? = null
    private var _sweepDisappearingAnimator: ValueAnimator? = null
    private var _rotationAnimator: ValueAnimator? = null
    private var _endAnimator: ValueAnimator? = null
    private var _modeAppearing = false
    private var _currentColor = 0
    private var _currentIndexColor = 0
    private var _currentSweepAngle = 0.0f
    private var _currentRotationAngleOffset = 0.0f
    private var _currentRotationAngle = 0.0f
    private var _currentEndRatio = 1.0f
    private var _firstSweepAnimation = false
    private var _angleInterpolator: Interpolator? = null
    private var _sweepInterpolator: Interpolator? = null
    private var _colors: IntArray? = null
    private var _sweepSpeed = 0.0f
    private var _rotationSpeed = 0.0f
    private var _minSweepAngle = 0
    private var _maxSweepAngle = 0
    private var _parent: CircularProgressDrawable? = null
    private var _onEndListener: CircularProgressDrawable.OnEndListener? = null

    constructor(parent: CircularProgressDrawable, options: Options) {
        _parent = parent
        _sweepInterpolator = options.sweepInterpolator
        _angleInterpolator = options.angleInterpolator
        _currentIndexColor = 0
        _colors = options.colors
        _currentColor = _colors!![0]
        _sweepSpeed = options.sweepSpeed
        _rotationSpeed = options.rotationSpeed
        _minSweepAngle = options.minSweepAngle
        _maxSweepAngle = options.maxSweepAngle
        setupAnimations()
    }

    fun setCurrentRotationAngle(currentRotationAngle: Float) {
        _currentRotationAngle = currentRotationAngle
        _parent?.invalidate()
    }

    fun setCurrentSweepAngle(currentSweepAngle: Float) {
        _currentSweepAngle = currentSweepAngle
        _parent?.invalidate()
    }

    private fun setDisappearing() {
        _modeAppearing = false
        _currentRotationAngleOffset += (360 - _maxSweepAngle)
    }

    private fun setAppearing() {
        _modeAppearing = true
        _currentRotationAngleOffset += _minSweepAngle
    }

    private fun setEndRatio(ratio: Float) {
        _currentEndRatio = ratio
        _parent?.invalidate()
    }

    private fun setupAnimations() {
        _rotationAnimator = ValueAnimator.ofFloat(0f, 360f)
        _rotationAnimator?.interpolator = _angleInterpolator
        _rotationAnimator?.duration = (ROTATION_ANIMATOR_DURATION / _rotationSpeed).toLong()
        _rotationAnimator?.addUpdateListener { animation ->
            val angle = ProgressBarUtils.getAnimatedFraction(animation) * 360f
            setCurrentRotationAngle(angle)
        }
        _rotationAnimator?.repeatCount = ValueAnimator.INFINITE
        _rotationAnimator?.repeatMode = ValueAnimator.RESTART
        _sweepAppearingAnimator = ValueAnimator.ofFloat(_minSweepAngle.toFloat(), _maxSweepAngle.toFloat())
        _sweepAppearingAnimator?.interpolator = _sweepInterpolator
        _sweepAppearingAnimator?.duration = (SWEEP_ANIMATOR_DURATION / _sweepSpeed).toLong()
        _sweepAppearingAnimator?.addUpdateListener { animation ->
            val animatedFraction = ProgressBarUtils.getAnimatedFraction(animation)
            var angle: Float
            if (_firstSweepAnimation) {
                angle = animatedFraction * _maxSweepAngle
            } else {
                angle = _minSweepAngle + animatedFraction * (_maxSweepAngle - _minSweepAngle)
            }
            setCurrentSweepAngle(angle)
        }
        _sweepAppearingAnimator?.addListener(object: SimpleAnimatorListener() {
            override fun onAnimationStart(animation: Animator?) {
                super.onAnimationStart(animation)
                _modeAppearing = true
            }
            override fun onPreAnimationEnd(animation: Animator?) {
                if (isStartedAndNotCancelled()) {
                    _firstSweepAnimation = false
                    setDisappearing()
                    _sweepDisappearingAnimator?.start()
                }
            }
        })
        _sweepDisappearingAnimator = ValueAnimator.ofFloat(_maxSweepAngle.toFloat(), _minSweepAngle.toFloat())
        _sweepDisappearingAnimator?.interpolator = _sweepInterpolator
        _sweepDisappearingAnimator?.duration = (SWEEP_ANIMATOR_DURATION / _sweepSpeed).toLong()
        _sweepDisappearingAnimator?.addUpdateListener { animation ->
            val animatedFraction = ProgressBarUtils.getAnimatedFraction(animation)
            setCurrentSweepAngle(_maxSweepAngle - animatedFraction * (_maxSweepAngle - _minSweepAngle))
            val duration = animation!!.duration
            val played = animation.currentPlayTime
            val fraction = played * 1.0f / duration
            if (_colors!!.size > 1 && fraction > 0.7f) {
                val prevColor = _currentColor
                val nextColor = _colors!![(_currentIndexColor + 1) % _colors!!.size]
                val newColor = COLOR_EVALUATOR.evaluate((fraction - .7f) / (1 - .7f), prevColor, nextColor) as Int
                _parent?.getCurrentPaint()?.color = newColor
            }
        }
        _sweepDisappearingAnimator?.addListener(object: SimpleAnimatorListener() {
            override fun onPreAnimationEnd(animation: Animator?) {
                if (isStartedAndNotCancelled()) {
                    setAppearing()
                    _currentIndexColor = (_currentIndexColor + 1) % _colors!!.size
                    _currentColor = _colors!![_currentIndexColor]
                    _parent?.getCurrentPaint()?.color = _currentColor
                    _sweepAppearingAnimator?.start()
                }
            }
        })
        _endAnimator = ValueAnimator.ofFloat(1.0f, 0.0f)
        _endAnimator?.interpolator = END_INTERPOLATOR
        _endAnimator?.duration = END_ANIMATOR_DURATION
        _endAnimator?.addUpdateListener { animation -> setEndRatio(1f - ProgressBarUtils.getAnimatedFraction(animation)) }
    }

    override fun draw(canvas: Canvas?, paint: Paint?) {
        var startAngle = _currentRotationAngle - _currentRotationAngleOffset
        var sweepAngle = _currentSweepAngle
        if (!_modeAppearing) {
            startAngle += (360 - sweepAngle)
        }
        startAngle %= 360
        if (_currentEndRatio < 1f) {
            val newSweepAngle = sweepAngle * _currentEndRatio
            startAngle = (startAngle + (sweepAngle - newSweepAngle)) % 360
            sweepAngle = newSweepAngle
        }
        canvas?.drawArc(_parent?.getDrawableBounds(), startAngle, sweepAngle, false, paint)
    }

    private fun reinitValues() {
        _firstSweepAnimation = true
        _currentEndRatio = 1.0f
        _parent?.getCurrentPaint()?.color = _currentColor
    }

    override fun start() {
        _endAnimator?.cancel()
        reinitValues()
        _rotationAnimator?.start()
        _sweepAppearingAnimator?.start()
    }

    private fun stopAnimators() {
        _rotationAnimator?.cancel()
        _sweepAppearingAnimator?.cancel()
        _sweepDisappearingAnimator?.cancel()
        _endAnimator?.cancel()
    }

    override fun stop() = stopAnimators()

    override fun progressiveStop(listener: CircularProgressDrawable.OnEndListener?) {
        if (!_parent!!.isRunning || _endAnimator!!.isRunning) {
            return
        }
        _onEndListener = listener
        _endAnimator?.addListener(object : SimpleAnimatorListener() {
            override fun onPreAnimationEnd(animation: Animator?) {
                _endAnimator?.removeListener(this)
                val endListener = _onEndListener
                _onEndListener = null
                if(isStartedAndNotCancelled()) {
                    setEndRatio(0.0f)
                    _parent?.stop()
                    endListener?.onEnd(_parent)
                }
            }
        })
        _endAnimator?.start()
    }
}