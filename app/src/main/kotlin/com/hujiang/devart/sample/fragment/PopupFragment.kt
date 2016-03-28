package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import com.hujiang.devart.base.BasePopupFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 3/27/16.
 */
class PopupFragment: BasePopupFragment() {
    override fun getBarTitle(): Int = R.string.popup_name

    override fun getBarTitleWithPath(): Int = R.string.popup_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() { }

    override fun initEvents() { }

    override fun initLogic() { }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_popup

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null
}