package com.hujiang.devart.component.badger

import android.content.Context
import android.graphics.Color
import android.graphics.Typeface
import android.graphics.drawable.ShapeDrawable
import android.graphics.drawable.shapes.RoundRectShape
import android.util.AttributeSet
import android.util.TypedValue
import android.view.Gravity
import android.view.View
import android.view.ViewGroup
import android.view.animation.AccelerateInterpolator
import android.view.animation.AlphaAnimation
import android.view.animation.Animation
import android.view.animation.DecelerateInterpolator
import android.widget.FrameLayout
import android.widget.TabWidget
import android.widget.TextView

/**
 * Created by rarnu on 4/19/16.
 */
class BadgeView: TextView {

    companion object {
        val POSITION_TOP_LEFT = 1
        val POSITION_TOP_RIGHT = 2
        val POSITION_BOTTOM_LEFT = 3
        val POSITION_BOTTOM_RIGHT = 4
        val POSITION_CENTER = 5
        private val DEFAULT_MARGIN_DIP = 5
        private val DEFAULT_LR_PADDING_DIP = 5
        private val DEFAULT_CORNER_RADIUS_DIP = 8
        private val DEFAULT_POSITION = POSITION_TOP_RIGHT
        private val DEFAULT_BADGE_COLOR = Color.parseColor("#CCFF0000")
        private val DEFAULT_TEXT_COLOR = Color.WHITE
        private var _fadeIn: Animation? = null
        private var _fadeOut: Animation? = null
    }

    private var _context: Context? = null
    private var _target: View? = null
    private var _badgePosition = 0
    private var _badgeMarginH = 0
    private var _badgeMarginV = 0
    private var _badgeColor = 0
    private var _isShown = false
    private var _badgeBg: ShapeDrawable? = null
    private var _targetTabIndex = 0

    constructor(context: Context): this(context, null as AttributeSet?, android.R.attr.textViewStyle)
    constructor(context: Context, attrs: AttributeSet?): this(context, attrs, android.R.attr.textViewStyle)
    constructor(context: Context, target: View?): this(context, null, android.R.attr.textViewStyle, target, 0)
    constructor(context: Context, target: TabWidget?, index: Int): this(context, null, android.R.attr.textViewStyle, target, index)
    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): this(context, attrs, defStyle, null, 0)
    constructor(context: Context, attrs: AttributeSet?, defStyle: Int, target: View?, tabIndex: Int): super(context, attrs, defStyle) {
        init(context, target, tabIndex)
    }

    private fun init(context: Context, target: View?, tabIndex: Int) {
        _context = context
        _target = target
        _targetTabIndex = tabIndex
        _badgePosition = DEFAULT_POSITION
        _badgeMarginH = dipToPixels(DEFAULT_MARGIN_DIP)
        _badgeMarginV = _badgeMarginH
        _badgeColor = DEFAULT_BADGE_COLOR
        typeface = Typeface.DEFAULT_BOLD
        val paddingPixels = dipToPixels(DEFAULT_LR_PADDING_DIP)
        setPadding(paddingPixels, 0, paddingPixels, 0)
        setTextColor(DEFAULT_TEXT_COLOR)
        _fadeIn = AlphaAnimation(0.0f, 1.0f)
        _fadeIn?.interpolator = DecelerateInterpolator()
        _fadeIn?.duration = 200L
        _fadeOut = AlphaAnimation(1.0f, 0.0f)
        _fadeOut?.interpolator = AccelerateInterpolator()
        _fadeOut?.duration = 200L
        _isShown = false
        if (_target != null) {
            applyTo(_target)
        } else {
            show()
        }
    }

    private fun applyTo(target: View?) {
        val lp = target?.layoutParams
        val parent = target?.parent
        val container = FrameLayout(_context)
        if (target is TabWidget) {
            val ntarget = target.getChildTabViewAt(_targetTabIndex)
            _target = ntarget
            (ntarget as ViewGroup).addView(container, ViewGroup.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT))
            visibility = View.GONE
            container.addView(this)
        } else {
            val group = parent as ViewGroup
            val index = group.indexOfChild(target)
            group.removeView(target)
            group.addView(container, index, lp)
            container.addView(target)
            visibility = View.GONE
            container.addView(this)
            group.invalidate()
        }
    }

    fun show() = show(false, null)

    fun show(animate: Boolean) = show(animate, _fadeIn)

    fun show(anim: Animation?) = show(true, anim)

    private fun show(animate: Boolean, anim: Animation?) {
        if (background == null) {
            if (_badgeBg == null) {
                _badgeBg = getDefaultBackground()
            }
            background = _badgeBg
        }
        applyLayoutParams()
        if (animate) {
            startAnimation(anim)
        }
        visibility = View.VISIBLE
        _isShown = true
    }

    fun hide() = hide(false, null)

    fun hide(animate: Boolean) = hide(animate, _fadeOut)

    fun hide(anim: Animation?) = hide(true, anim)

    private fun hide(animate: Boolean, anim: Animation?) {
        visibility = View.GONE
        if (animate) {
            startAnimation(anim)
        }
        _isShown = false
    }

    fun toggle() = toggle(false, null, null)

    fun toggle(animate: Boolean) = toggle(animate, _fadeIn, _fadeOut)

    fun toggle(animIn: Animation?, animOut: Animation?) = toggle(true, animIn, animOut)

    private fun toggle(animate: Boolean, animIn: Animation?, animOut: Animation?) {
        if (isShown) {
            hide(animate && (animOut != null), animOut)
        } else {
            show(animate && (animIn != null), animIn)
        }
    }

    private fun applyLayoutParams() {
        val lp = FrameLayout.LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT, ViewGroup.LayoutParams.WRAP_CONTENT)
        when (_badgePosition) {
            POSITION_TOP_LEFT -> {
                lp.gravity = Gravity.LEFT or Gravity.TOP
                lp.setMargins(_badgeMarginH, _badgeMarginV, 0, 0)
            }
            POSITION_TOP_RIGHT -> {
                lp.gravity = Gravity.RIGHT or Gravity.TOP
                lp.setMargins(0, _badgeMarginV, _badgeMarginH, 0)
            }
            POSITION_BOTTOM_LEFT -> {
                lp.gravity = Gravity.LEFT or Gravity.BOTTOM
                lp.setMargins(_badgeMarginH, 0, 0, _badgeMarginV)
            }
            POSITION_BOTTOM_RIGHT -> {
                lp.gravity = Gravity.RIGHT or Gravity.BOTTOM
                lp.setMargins(0, 0, _badgeMarginH, _badgeMarginV)
            }
            POSITION_CENTER -> {
                lp.gravity = Gravity.CENTER
                lp.setMargins(0, 0, 0, 0)
            }
        }
        layoutParams = lp

    }

    private fun getDefaultBackground(): ShapeDrawable? {
        val r = dipToPixels(DEFAULT_CORNER_RADIUS_DIP).toFloat()
        val outerR = floatArrayOf(r, r, r, r, r, r, r, r)
        val rr = RoundRectShape(outerR, null, null)
        val drawable = ShapeDrawable(rr)
        drawable.paint.color = _badgeColor
        return drawable
    }

    fun increment(offset: Int): Int {
        val txt = text
        var i: Int
        if (txt != null) {
            try {
                i = txt.toString().toInt()
            } catch (e: Exception) {
                i = 0
            }
        } else {
            i = 0
        }
        i += offset
        text = i.toString()
        return i
    }

    fun decrement(offset: Int): Int = increment(-offset)

    override fun isShown(): Boolean = _isShown

    fun getTarget(): View? = _target

    fun getBadgePosition(): Int = _badgePosition

    fun setBadgePosition(layoutPosition: Int) {
        _badgePosition = layoutPosition
    }

    fun getHorizontalBadgeMargin(): Int = _badgeMarginH

    fun getVerticalBadgeMargin(): Int = _badgeMarginV

    fun setBadgeMargin(badgeMargin: Int) {
        _badgeMarginH = badgeMargin
        _badgeMarginV = badgeMargin
    }

    fun setBadgeMargin(horizontal: Int, vertical: Int) {
        _badgeMarginH = horizontal
        _badgeMarginV = vertical
    }

    fun getBadgeBackgroundColor(): Int = _badgeColor

    fun setBadgeBackgroundColor(badgeColor: Int) {
        _badgeColor = badgeColor
        _badgeBg = getDefaultBackground()
    }

    private fun dipToPixels(dip: Int): Int = TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, dip.toFloat(), resources.displayMetrics).toInt()

}