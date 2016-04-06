package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.widget.Toast
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.gesturelock.GestureLockView
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 4/6/16.
 */
class LockViewFragment: BaseFragment(), GestureLockView.OnGestureFinishListener {

    private var _glv: GestureLockView? = null

    override fun getBarTitle(): Int = R.string.lockview_name

    override fun getBarTitleWithPath(): Int = R.string.lockview_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _glv = innerView?.findViewById(R.id.glv) as GestureLockView
        _glv?.key = "00010204060708"
    }

    override fun initEvents() {
        _glv?.onGestureFinishListener = this
    }

    override fun initLogic() { }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_lockview

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun OnGestureFinish(success: Boolean, key: String?) {
        Toast.makeText(activity, "succ: ${success}, key: ${key}", Toast.LENGTH_SHORT).show()
    }
}