package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.Menu
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.BlackTechnology
import com.hujiang.devart.utils.MessageUtils
import kotlin.concurrent.thread

/**
 * Created by rarnu on 4/2/16.
 */
class BlackTechFragment: BaseFragment() {

    private var _tvMacAddr: TextView? = null

    private var _hMac = object : Handler() {
        override fun handleMessage(msg: Message?) {
            _tvMacAddr?.text = msg?.obj as String?
            super.handleMessage(msg)
        }
    }

    override fun getBarTitle(): Int = R.string.blacktech_name

    override fun getBarTitleWithPath(): Int = R.string.blacktech_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _tvMacAddr = innerView?.findViewById(R.id.tvMacAddr) as TextView
    }

    override fun initEvents() {

    }

    override fun initLogic() {
        thread {
            val macList = BlackTechnology.blackGetMacAddress()
            var ret = ""
            for (mac in macList!!) {
                ret += "identifier: ${mac?.identifier}\nattr: ${mac?.attributes}\nmac: ${mac?.macAddress}\n\n"
            }
            MessageUtils.sendMessage(_hMac, 0, 0, 0, ret)
        }
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_blacktech

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

}