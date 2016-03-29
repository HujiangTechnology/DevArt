package com.hujiang.devart.sample

import android.app.Fragment
import com.hujiang.devart.base.BaseActivity

/**
 * Created by rarnu on 3/29/16.
 */
class FileActivity: BaseActivity() {

    override fun getIcon(): Int = R.mipmap.ic_launcher

    override fun replaceFragment(): Fragment = Fragments.fileFragment

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = true
}