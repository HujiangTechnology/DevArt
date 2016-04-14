package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.View
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.glassbar.GlassActionBarHelper
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 4/14/16.
 */
class GlassBarFragment: BaseFragment() {

    private var _helper: GlassActionBarHelper? = null

    override fun getBarTitle(): Int = R.string.glassbar_name

    override fun getBarTitleWithPath(): Int = R.string.glassbar_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() { }

    override fun initEvents() { }

    override fun initLogic() { }

    override fun getFramgmentLayoutView(): View? {
        _helper = GlassActionBarHelper().contentLayout(R.layout.fragment_glassbar)
        return _helper?.createView(activity)
    }

    override fun getFragmentLayoutResId(): Int = 0

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

}