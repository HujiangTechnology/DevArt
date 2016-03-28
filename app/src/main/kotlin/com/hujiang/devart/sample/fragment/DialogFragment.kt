package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.Button
import com.hujiang.devart.base.BaseDialogFragment
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 3/27/16.
 */
class DialogFragment: BaseDialogFragment(), View.OnClickListener {

    private var _btnClose: Button? = null

    override fun getBarTitle(): Int = 0

    override fun getBarTitleWithPath(): Int = 0

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _btnClose = innerView?.findViewById(R.id.btnClose) as Button
    }

    override fun initEvents() {
        _btnClose?.setOnClickListener(this)
    }

    override fun initLogic() { }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_dialog

    override fun getMainActivityName(): String? = null

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View?) {
        activity.finish()
    }

}