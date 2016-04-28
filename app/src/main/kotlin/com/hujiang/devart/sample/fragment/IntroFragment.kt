package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.webkit.WebView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.FileUtils

/**
 * Created by rarnu on 3/25/16.
 */
class IntroFragment: BaseFragment() {

    companion object {
        val ASSET = "file:///android_asset"
    }
    private var _mdv: WebView? = null


    override fun getBarTitle(): Int = R.string.fragment_intro

    override fun getBarTitleWithPath(): Int = R.string.fragment_intro

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _mdv = innerView?.findViewById(R.id.mdv) as WebView

    }

    override fun initEvents() { }

    override fun initLogic() { }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_intro

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null
}