package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 3/25/16.
 */
class IntroFragment: BaseFragment() {

    override fun getBarTitle(): Int = R.string.fragment_intro

    override fun getBarTitleWithPath(): Int = R.string.fragment_intro

    override fun getCustomTitle(): String? = null

    override fun initComponents() { }

    override fun initEvents() { }

    override fun initLogic() { }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_intro

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null
}