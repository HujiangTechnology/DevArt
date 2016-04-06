package com.hujiang.devart.sample

import android.app.Fragment
import com.hujiang.devart.base.BaseActivity

/**
 * Created by rarnu on 4/5/16.
 */
class AlgorithmActivity: BaseActivity() {
    override fun getIcon(): Int = R.mipmap.ic_launcher

    override fun replaceFragment(): Fragment = Fragments.algorithmFragment

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = true

}