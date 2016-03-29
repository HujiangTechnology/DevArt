package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.Button
import android.widget.CheckBox
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.base.common.Actions
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.NotificationUtils

/**
 * Created by rarnu on 3/29/16.
 */
class NotificationFragment : BaseFragment(), View.OnClickListener {


    private var _btnShowNotification: Button? = null
    private var _btnHideNotification: Button? = null
    private var _chkCanClose: CheckBox? = null

    override fun getBarTitle(): Int = R.string.notification_name

    override fun getBarTitleWithPath(): Int = R.string.notification_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _btnShowNotification = innerView?.findViewById(R.id.btnShowNotification) as Button
        _btnHideNotification = innerView?.findViewById(R.id.btnHideNotification) as Button
        _chkCanClose = innerView?.findViewById(R.id.chkCanClose) as CheckBox
    }

    override fun initEvents() {
        _btnShowNotification?.setOnClickListener(this)
        _btnHideNotification?.setOnClickListener(this)
    }

    override fun initLogic() {
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_notification

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) {
    }

    override fun onGetNewArguments(bn: Bundle?) {
    }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.btnShowNotification ->
                NotificationUtils.showNotification(activity, 999, R.mipmap.ic_launcher, R.string.notification_title, R.string.notification_desc, (if (_chkCanClose!!.isChecked) Actions.ACTION_NOTIFY else Actions.ACTION_NOTIFY_NULL), _chkCanClose!!.isChecked)
            R.id.btnHideNotification -> NotificationUtils.cancalAllNotification(activity, intArrayOf(999))
        }
    }
}