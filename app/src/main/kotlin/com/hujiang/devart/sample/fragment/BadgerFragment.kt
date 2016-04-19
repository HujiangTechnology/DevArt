package com.hujiang.devart.sample.fragment

import android.graphics.Color
import android.os.Bundle
import android.view.Menu
import android.view.View
import android.view.animation.BounceInterpolator
import android.view.animation.TranslateAnimation
import android.widget.Button
import android.widget.Toast
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.badger.BadgeView
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 4/19/16.
 */
class BadgerFragment : BaseFragment() {

    private var _defaultTarget: View? = null
    private var _bDefault: BadgeView? = null
    private var _btnPosition: Button? = null
    private var _bPosition: BadgeView? = null
    private var _btnColor: Button? = null
    private var _bColor: BadgeView? = null
    private var _btnAnim1: Button? = null
    private var _bAnim1: BadgeView? = null
    private var _btnAnim2: Button? = null
    private var _bAnim2: BadgeView? = null
    private var _btnClick: Button? = null
    private var _bClick: BadgeView? = null
    private var _btnIncrement: Button? = null
    private var _bIncrement: BadgeView? = null

    override fun getBarTitle(): Int = R.string.badger_name

    override fun getBarTitleWithPath(): Int = R.string.badger_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _defaultTarget = innerView?.findViewById(R.id.defaultTarget)
        _bDefault = BadgeView(activity, _defaultTarget)
        _bDefault?.text = "1"

        _btnPosition = innerView?.findViewById(R.id.positionTarget) as Button
        _bPosition = BadgeView(activity, _btnPosition)
        _bPosition?.text = "12"
        _bPosition?.setBadgePosition(BadgeView.POSITION_CENTER)

        _btnColor = innerView?.findViewById(R.id.colorTarget) as Button
        _bColor = BadgeView(activity, _btnColor)
        _bColor?.text = "New!"
        _bColor?.setTextColor(Color.BLUE)
        _bColor?.setBadgeBackgroundColor(Color.YELLOW)
        _bColor?.textSize = 12.0f

        _btnAnim1 = innerView?.findViewById(R.id.anim1Target) as Button
        _bAnim1 = BadgeView(activity, _btnAnim1)
        _bAnim1?.text = "84"

        _btnAnim2 = innerView?.findViewById(R.id.anim2Target) as Button
        _bAnim2 = BadgeView(activity, _btnAnim2)
        _bAnim2?.text = "123"
        _bAnim2?.setBadgePosition(BadgeView.POSITION_TOP_LEFT)
        _bAnim2?.setBadgeMargin(15, 10)
        _bAnim2?.setBadgeBackgroundColor(Color.parseColor("#A4C639"))

        // badge.setBackgroundResource(R.drawable.badge)

        _btnClick = innerView?.findViewById(R.id.clickTarget) as Button
        _bClick = BadgeView(activity, _btnClick)
        _bClick?.text = "click me"
        _bClick?.setBadgeBackgroundColor(Color.BLUE)
        _bClick?.textSize = 16.0f

        _btnIncrement = innerView?.findViewById(R.id.incrementTarget) as Button
        _bIncrement = BadgeView(activity, _btnIncrement)
        _bIncrement?.text = "0"
    }

    override fun initEvents() {
        _btnPosition?.setOnClickListener { _bPosition?.toggle() }
        _btnColor?.setOnClickListener { _bColor?.toggle() }
        _btnAnim1?.setOnClickListener { _bAnim1?.toggle(true) }
        _btnAnim2?.setOnClickListener {
            val anim = TranslateAnimation(-100.0f, 0.0f, 0.0f, 0.0f)
            anim.interpolator = BounceInterpolator()
            anim.duration = 1000L
            _bAnim2?.toggle(anim, null)
        }
        _bClick?.setOnClickListener { Toast.makeText(activity, "clicked badge", Toast.LENGTH_SHORT).show() }
        _btnClick?.setOnClickListener { _bClick?.toggle() }
        _btnIncrement?.setOnClickListener {
            if (_bIncrement!!.isShown) {
                _bIncrement?.increment(1)
            } else {
                _bIncrement?.show()
            }
        }
    }

    override fun initLogic() {
        _bDefault?.show()

    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_badger

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) {
    }

    override fun onGetNewArguments(bn: Bundle?) {
    }

    override fun getFragmentState(): Bundle? = null
}