package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.Button
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.floatwindow.FloatService
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.sample.service.DemoFloatWindowService

/**
 * Created by rarnu on 3/29/16.
 */
class FloatWindowFragment: BaseFragment(), View.OnClickListener {

    private var _btnShowFloatWindow: Button? = null
    private var _btnHideFloatWindow: Button? = null

    override fun getBarTitle(): Int = R.string.float_window_name

    override fun getBarTitleWithPath(): Int = R.string.float_window_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _btnShowFloatWindow = innerView?.findViewById(R.id.btnShowFloatWindow) as Button
        _btnHideFloatWindow = innerView?.findViewById(R.id.btnHideFloatWindow) as Button
    }

    override fun initEvents() {
        _btnShowFloatWindow?.setOnClickListener(this)
        _btnHideFloatWindow?.setOnClickListener(this)

    }

    override fun initLogic() { }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_float_window

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.btnShowFloatWindow -> FloatService.showFloatWindow(activity, DemoFloatWindowService::class.java)
            R.id.btnHideFloatWindow -> FloatService.hideFloatWindow(activity, DemoFloatWindowService::class.java)
        }
    }
}