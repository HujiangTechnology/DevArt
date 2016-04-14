package com.hujiang.devart.component.arcmenu

import android.content.Context
import android.util.AttributeSet
import android.view.LayoutInflater
import android.view.MotionEvent
import android.view.View
import android.view.ViewGroup
import android.view.animation.*
import android.widget.ImageView
import android.widget.RelativeLayout
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/14/16.
 */
class RayMenu : RelativeLayout, ArcRayMenuIntf {

    companion object {
        private fun createItemDisapperAnimation(duration: Long, isClicked: Boolean): Animation? {
            val animationSet = AnimationSet(true)
            animationSet.addAnimation(ScaleAnimation(1.0f, if (isClicked) 2.0f else 0.0f, 1.0f, if (isClicked) 2.0f else 0.0f, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f))
            animationSet.addAnimation(AlphaAnimation(1.0f, 0.0f))
            animationSet.duration = duration
            animationSet.interpolator = DecelerateInterpolator()
            animationSet.fillAfter = true
            return animationSet
        }

        private fun createHintSwitchAnimation(expanded: Boolean): Animation? {
            val animation = RotateAnimation(if (expanded) 45.0f else 0.0f, if (expanded) 0.0f else 45.0f, Animation.RELATIVE_TO_SELF, 0.5f, Animation.RELATIVE_TO_SELF, 0.5f)
            animation.startOffset = 0
            animation.duration = 100
            animation.interpolator = DecelerateInterpolator()
            animation.fillAfter = true
            return animation
        }
    }

    private var _rayLayout: RayLayout? = null
    private var _hintView: ImageView? = null

    constructor(context: Context) : super(context) {
        init(context)
    }

    constructor(context: Context, attrs: AttributeSet?) : super(context, attrs) {
        init(context)
        applyAttrs(attrs)
    }

    private fun applyAttrs(attrs: AttributeSet?) {
        if (attrs != null) {
            val a = context.obtainStyledAttributes(attrs, R.styleable.ArcLayout, 0, 0)
            val defaultChildSize = _rayLayout!!.getChildSize()
            val newChildSize = a.getDimensionPixelSize(R.styleable.RayLayout_childSize, defaultChildSize)
            _rayLayout?.setChildSize(newChildSize)
            a.recycle()
        }
    }

    private fun init(context: Context) {
        layoutParams = LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT)
        clipChildren = false
        val li = LayoutInflater.from(context)
        li.inflate(R.layout.ray_menu, this)
        _rayLayout = findViewById(R.id.item_layout) as RayLayout
        val controlLayout = findViewById(R.id.control_layout) as ViewGroup
        controlLayout.isClickable = true
        controlLayout.setOnTouchListener { v, event ->
            if (event.action == MotionEvent.ACTION_DOWN) {
                _hintView?.startAnimation(createHintSwitchAnimation(_rayLayout!!.isExpanded()))
                _rayLayout?.switchState(true)
            }
            false
        }
        _hintView = findViewById(R.id.control_hint) as ImageView
    }

    override fun addItem(item: View?, listener: OnClickListener?) {
        _rayLayout?.addView(item)
        item?.setOnClickListener(getItemClickListener(listener))
    }

    private fun bindItemAnimation(child: View?, isClicked: Boolean, duration: Long): Animation? {
        val animation = createItemDisapperAnimation(duration, isClicked)
        child?.animation = animation
        return animation
    }

    private fun itemDidDisappear() {
        val itemCount = _rayLayout!!.childCount
        for (i in 0..itemCount - 1) {
            val item = _rayLayout?.getChildAt(i)
            item?.clearAnimation()
        }
        _rayLayout?.switchState(false)
    }

    private fun getItemClickListener(listener: OnClickListener?): OnClickListener? = View.OnClickListener { v ->
        val animation = bindItemAnimation(v, true, 400)
        animation?.setAnimationListener(object : Animation.AnimationListener {
            override fun onAnimationStart(animation: Animation?) {
            }

            override fun onAnimationRepeat(animation: Animation?) {
            }

            override fun onAnimationEnd(animation: Animation?) {
                postDelayed({ itemDidDisappear() }, 0)
            }
        })
        val itemCount = _rayLayout!!.childCount
        for (i in 0..itemCount - 1) {
            val item = _rayLayout?.getChildAt(i)
            if (v != item) {
                bindItemAnimation(item, false, 300)
            }
        }
        _rayLayout?.invalidate()
        _hintView?.startAnimation(createHintSwitchAnimation(true))
        listener?.onClick(v)
    }

}