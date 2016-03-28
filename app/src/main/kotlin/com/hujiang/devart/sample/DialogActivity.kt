package com.hujiang.devart.sample

import android.app.Fragment
import com.hujiang.devart.base.BaseDialog

/**
 * Created by rarnu on 3/27/16.
 */
class DialogActivity: BaseDialog() {
    override fun getCloseCondition(): Boolean = false

    override fun replaceFragment(): Fragment = Fragments.dialogFragment

    override fun customTheme(): Int = 0
}