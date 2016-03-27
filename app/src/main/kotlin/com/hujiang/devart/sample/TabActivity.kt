package com.hujiang.devart.sample

import android.app.Fragment
import com.hujiang.devart.base.BaseActivity
import com.hujiang.devart.sample.fragment.TabFragment

/**
 * Created by rarnu on 3/25/16.
 */
class TabActivity: BaseActivity() {

    override fun getIcon(): Int = R.mipmap.ic_launcher

    override fun replaceFragment(): Fragment = Fragments.tabFragment

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = true
}