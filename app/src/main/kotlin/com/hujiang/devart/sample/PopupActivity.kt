package com.hujiang.devart.sample

import android.app.Fragment
import com.hujiang.devart.base.BasePopupActivity

/**
 * Created by rarnu on 3/27/16.
 */
class PopupActivity: BasePopupActivity() {

    override fun getIcon(): Int = R.mipmap.ic_launcher

    override fun replaceFragment(): Fragment = Fragments.popupFragment

    override fun customTheme(): Int = 0

    override fun getActionBarCanBack(): Boolean = true
}