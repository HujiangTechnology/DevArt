package com.hujiang.devart.sample

import android.app.Fragment
import android.os.Bundle
import com.hujiang.devart.base.BaseSlidingActivity
import com.hujiang.devart.component.sliding.SlidingMenu
import com.hujiang.devart.sample.fragment.SlideContentFragment
import com.hujiang.devart.sample.fragment.SlideMenuFragment
import com.hujiang.devart.sample.fragment.SlideSecondMenuFragment

/**
 * Created by rarnu on 3/25/16.
 */
class SlideActivity: BaseSlidingActivity() {

    private var _content = SlideContentFragment()
    var contentFragment: SlideContentFragment? = null
        get() = _content
    private var _menu = SlideMenuFragment()
    var menuFragment: SlideMenuFragment? = null
        get() = _menu
    private var _secondaryMenu = SlideSecondMenuFragment()
    var secondaryMenuFragment: SlideSecondMenuFragment? = null
        get() = _secondaryMenu

    override fun replaceMenuFragment(): Fragment? = _menu

    override fun replaceSecondMenuFragment(): Fragment? = _secondaryMenu

    override fun getBehindOffset(): Int = 200

    override fun getAboveTouchMode(): Int = SlidingMenu.TOUCHMODE_FULLSCREEN

    override fun getBehindTouchMode(): Int = SlidingMenu.TOUCHMODE_MARGIN

    override fun getSlideMode(): Int = SlidingMenu.LEFT_RIGHT

    override fun getIcon(): Int = R.mipmap.ic_launcher

    override fun replaceFragment(): Fragment = _content

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = true

    override fun loadFragments() { }

    override fun releaseFragments() { }
}