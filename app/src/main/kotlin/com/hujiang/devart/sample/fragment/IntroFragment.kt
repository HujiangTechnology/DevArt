package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.webkit.WebSettings
import android.webkit.WebView
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.NetworkUtils

/**
 * Created by rarnu on 3/25/16.
 */
class IntroFragment: BaseFragment() {

    private var _mdv: WebView? = null
    private var _tvHint: TextView? = null

    override fun getBarTitle(): Int = R.string.fragment_intro

    override fun getBarTitleWithPath(): Int = R.string.fragment_intro

    override fun getCustomTitle(): String? = null

    override fun initComponents() {

        _tvHint = innerView?.findViewById(R.id.tvHint) as TextView
        _tvHint?.text = "Please visit http://${NetworkUtils.ipAddressV4}:8899/intro"

        _mdv = innerView?.findViewById(R.id.mdv) as WebView
        val settings = _mdv?.settings
        settings?.javaScriptEnabled = true
        settings?.allowContentAccess = true
        settings?.allowFileAccess = true
        settings?.allowFileAccessFromFileURLs = true
        settings?.cacheMode = WebSettings.LOAD_DEFAULT
        settings?.domStorageEnabled = true
        settings?.loadsImagesAutomatically = true
        _mdv?.loadUrl("https://github.com/HujiangTechnology/DevArt")
    }

    override fun initEvents() { }

    override fun initLogic() { }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_intro

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null
}