package com.hujiang.devart.sample

import android.app.Fragment
import com.hujiang.devart.base.BaseActivity

/**
 * Created by rarnu on 3/30/16.
 */
class GifActivity: BaseActivity() {
    override fun getIcon(): Int = R.mipmap.ic_launcher

    override fun replaceFragment(): Fragment = Fragments.gifFragment

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = true

}