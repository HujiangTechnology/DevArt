package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 3/25/16.
 */
class SlideContentFragment: BaseFragment() {

    private var _tvText: TextView? = null
    private var _itemBack: MenuItem? = null

    override fun getBarTitle(): Int = R.string.slidemenu_name

    override fun getBarTitleWithPath(): Int = R.string.slidemenu_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _tvText = innerView?.findViewById(R.id.tvText) as TextView
    }

    override fun initEvents() { }

    override fun initLogic() { }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_slide_content

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) {
        _itemBack = menu?.add(0, 1, 99, "Back")
        _itemBack?.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM)
        _itemBack?.setIcon(android.R.drawable.ic_menu_revert)
    }

    override fun onOptionsItemSelected(item: MenuItem?): Boolean {
        when (item!!.itemId) {
            1 -> activity.finish()
        }
        return true
    }

    override fun onGetNewArguments(bn: Bundle?) {
        _tvText?.text = bn?.getString("text")
    }

    override fun getFragmentState(): Bundle? = null
}