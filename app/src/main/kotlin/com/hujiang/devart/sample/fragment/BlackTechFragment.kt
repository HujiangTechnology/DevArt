package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.BlackTechnology

/**
 * Created by rarnu on 4/2/16.
 */
class BlackTechFragment: BaseFragment() {

    private var _tvMacAddr: TextView? = null

    override fun getBarTitle(): Int = R.string.blacktech_name

    override fun getBarTitleWithPath(): Int = R.string.blacktech_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _tvMacAddr = innerView?.findViewById(R.id.tvMacAddr) as TextView
    }

    override fun initEvents() {

    }

    override fun initLogic() {
        val macStr = BlackTechnology.blackGetMacAddress()
        val macList = BlackTechnology.strToMacAddressList(macStr)
        var ret = ""
        for (mac in macList!!) {
            ret += "identifier: ${mac.identifier}\nattr: ${mac.attributes}\nmac: ${mac.macAddress}\n\n"
        }
        _tvMacAddr?.text = ret
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_blacktech

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

}