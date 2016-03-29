package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.LayoutInflater
import android.view.Menu
import android.view.View
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.scroll.*
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.UIUtils

/**
 * Created by rarnu on 3/29/16.
 */
class HScrollFragment: BaseFragment(), OnScreenChangeListener, OnPageSelected {


    private var _barPoint: PointBar? = null
    private var _evpPoint: PointBar? = null
    private var _hsl: HScrollLayout? = null
    private var _evp: EndlessViewPager? = null
    private var _views: MutableList<View?>? = null

    override fun getBarTitle(): Int = R.string.scroll_name

    override fun getBarTitleWithPath(): Int = R.string.scroll_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _barPoint = innerView?.findViewById(R.id.barPoint) as PointBar
        _evpPoint = innerView?.findViewById(R.id.evpPoint) as PointBar
        _hsl = innerView?.findViewById(R.id.hsl) as HScrollLayout
        _evp = innerView?.findViewById(R.id.evp) as EndlessViewPager
        var height = UIUtils.height!! - activity.actionBar.height - UIUtils.statusBarHeight!!
        height /= 2
        val evplp = _evp?.layoutParams
        evplp?.height = height
        _evp?.layoutParams = evplp
        initViewPager()
    }

    private fun initViewPager() {
        _evp?.endless = true
        _views = arrayListOf<View?>()
        val inf = LayoutInflater.from(activity)
        for (i in 1..3) {
            _views?.add(inf.inflate(resources.getIdentifier("view_page${i}", "layout", activity.packageName), null))
        }
        _evp?.setData(_views)
        _evp?.currentItem = Integer.MAX_VALUE / 2 - 3
    }

    override fun initEvents() {
        _hsl?.screenChangeListener = this
        _evp?.pageSelected = this
    }

    override fun initLogic() {
        _barPoint?.setPointCount(3)
        _barPoint?.setPoint(0)
        _evpPoint?.setPointCount(3)
        _evpPoint?.setPoint(0)
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_hscroll

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onScreenChange(v: View?, screen: Int) {
        _barPoint?.setPoint(screen)
    }

    override fun onPageSelected(position: Int) {
        _evpPoint?.setPoint(position)
    }
}