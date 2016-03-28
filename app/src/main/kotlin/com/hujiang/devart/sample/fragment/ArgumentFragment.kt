package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.widget.Button
import android.widget.TextView
import android.widget.Toast
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 3/27/16.
 */
class ArgumentFragment: BaseFragment(), View.OnClickListener {

    private var _tvFragmentId: TextView? =null
    private var _btnShowId: Button? = null
    private var _itemAction: MenuItem? = null

    override fun getBarTitle(): Int = R.string.argument_name

    override fun getBarTitleWithPath(): Int = R.string.argument_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _tvFragmentId = innerView?.findViewById(R.id.tvFragmentId) as TextView
        _btnShowId = innerView?.findViewById(R.id.btnShowId) as Button
    }

    override fun initEvents() {
        _btnShowId?.setOnClickListener(this)
    }

    override fun initLogic() {
        if (innerBundle != null) {
            val key = innerBundle?.getString("key")
            _tvFragmentId?.text = key
        }
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_argument

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) {
        _itemAction = menu?.add(0, 2, 99, "Action")
        _itemAction?.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM)
        _itemAction?.setIcon(android.R.drawable.ic_menu_help)
    }

    override fun onOptionsItemSelected(item: MenuItem?): Boolean {
        when (item!!.itemId) {
            2 -> Toast.makeText(activity, _tvFragmentId?.text.toString(), Toast.LENGTH_SHORT).show()
        }
        return true
    }

    override fun onGetNewArguments(bn: Bundle?) {
        val key = bn?.getString("key")
        _tvFragmentId?.text = key
    }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View?) {
        Toast.makeText(activity, _tvFragmentId?.text.toString(), Toast.LENGTH_SHORT).show()
    }
}