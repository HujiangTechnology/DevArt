package com.hujiang.devart.component.progress

import android.animation.Animator

/**
 * Created by rarnu on 4/15/16.
 */
abstract class SimpleAnimatorListener: Animator.AnimatorListener {

    private var _started = false
    private var _cancelled = false

    override fun onAnimationStart(animation: Animator?) {
        _cancelled = false
        _started = true
    }

    override fun onAnimationEnd(animation: Animator?) {
        onPreAnimationEnd(animation)
        _started = false
    }

    protected open fun onPreAnimationEnd(animation: Animator?) { }

    override fun onAnimationCancel(animation: Animator?) {
        _cancelled = true
    }

    override fun onAnimationRepeat(animation: Animator?) { }

    fun isStartedAndNotCancelled(): Boolean = _started && !_cancelled
}