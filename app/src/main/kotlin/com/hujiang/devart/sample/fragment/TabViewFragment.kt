package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 3/25/16.
 */
class TabViewFragment: BaseFragment {

    private var _tvText: TextView? = null

    constructor(): super()

    constructor(tabTitle: String): super(tabTitle)

    override fun getBarTitle(): Int = R.string.tab_name

    override fun getBarTitleWithPath(): Int = R.string.tab_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _tvText = innerView?.findViewById(R.id.tvText) as TextView
        _tvText?.text = tabTitle
    }

    override fun initEvents() { }

    override fun initLogic() { }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_tab_view

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null
}