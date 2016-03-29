package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.DeviceUtils

/**
 * Created by rarnu on 3/29/16.
 */
class DeviceFragment: BaseFragment() {

    private var _tvDevice: TextView? = null

    override fun getBarTitle(): Int = R.string.device_name

    override fun getBarTitleWithPath(): Int = R.string.device_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _tvDevice = innerView?.findViewById(R.id.tvDevice) as TextView
    }

    override fun initEvents() {

    }

    override fun initLogic() {
        _tvDevice?.text = DeviceUtils.getDeviceInfo().toString()
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_device

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null
}