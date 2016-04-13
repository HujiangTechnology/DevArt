package com.hujiang.devart.sample

import android.app.Fragment
import com.hujiang.devart.base.BaseActivity

/**
 * Created by rarnu on 4/12/16.
 */
class TerminalActivity: BaseActivity() {
    override fun getIcon(): Int = R.mipmap.ic_launcher

    override fun replaceFragment(): Fragment = Fragments.terminalFragment

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = true

}