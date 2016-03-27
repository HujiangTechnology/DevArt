package com.hujiang.devart.sample.fragment

import android.app.Fragment
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import com.hujiang.devart.base.BaseTabFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import java.util.*

/**
 * Created by rarnu on 3/25/16.
 */
class TabFragment: BaseTabFragment() {


    private var _itemAdd: MenuItem? = null
    private var _itemRemove: MenuItem? = null

    override fun initFragmentList(listFragment: MutableList<Fragment?>?) {
        listFragment?.add(TabViewFragment("Tab 1"))
        listFragment?.add(TabViewFragment("Tab 2"))
        listFragment?.add(TabViewFragment("Tab 3"))
        listFragment?.add(TabViewFragment("Tab 4"))
        listFragment?.add(TabViewFragment("Tab 5"))
    }

    override fun getBarTitle(): Int = R.string.tab_name

    override fun getBarTitleWithPath(): Int = R.string.tab_name_with_path

    override fun getCustomTitle(): String? = null

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) {
        _itemAdd = menu?.add(0, 1, 99, "Add")
        _itemAdd?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
        _itemRemove = menu?.add(0, 2, 100, "Remove")
        _itemRemove?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
    }

    override fun onOptionsItemSelected(item: MenuItem?): Boolean {
        when (item!!.itemId) {
            1 -> {
                val r = Random(System.currentTimeMillis())
                val tag = r.nextInt().toString()
                addTab(-1, TabViewFragment(tag))
            }
            2 -> {
                val position = currentPage
                removeTab(position)
            }
        }
        return true
    }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null
}