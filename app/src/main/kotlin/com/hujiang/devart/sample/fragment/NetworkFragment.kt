package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.NetworkUtils

/**
 * Created by rarnu on 3/29/16.
 */
class NetworkFragment: BaseFragment() {

    private var _tvNetwork: TextView? = null

    override fun getBarTitle(): Int = R.string.network_name

    override fun getBarTitleWithPath(): Int = R.string.network_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _tvNetwork = innerView?.findViewById(R.id.tvNetwork) as TextView
    }

    override fun initEvents() { }

    override fun initLogic() {
        _tvNetwork?.text = NetworkUtils.getNetworkStatusDesc(activity)
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_network

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

}