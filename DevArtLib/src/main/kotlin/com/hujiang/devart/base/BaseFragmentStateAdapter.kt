package com.hujiang.devart.base

import android.app.Fragment
import android.app.FragmentManager

/**
 * Created by rarnu on 3/24/16.
 */
class BaseFragmentStateAdapter: FragmentStatePagerAdapter {

    private var _list: MutableList<Fragment?>? = null

    constructor(fm: FragmentManager?, listFragment: MutableList<Fragment?>?): super(fm) {
        _list = listFragment
    }

    override fun getItem(position: Int): Fragment? = _list!![position]

    override fun getCount(): Int = _list!!.size
}