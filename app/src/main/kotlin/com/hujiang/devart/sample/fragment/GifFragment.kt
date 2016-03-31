package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.gif.GifView
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 3/30/16.
 */
class GifFragment: BaseFragment() {

    private var _iv: GifView? = null

    override fun getBarTitle(): Int = R.string.gif_name

    override fun getBarTitleWithPath(): Int = R.string.gif_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _iv = innerView?.findViewById(R.id.iv) as GifView
    }

    override fun initEvents() { }

    override fun initLogic() {
        _iv?.setGifImage(R.drawable.hujiang)
        _iv?.showAnimation()
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_gif

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

}