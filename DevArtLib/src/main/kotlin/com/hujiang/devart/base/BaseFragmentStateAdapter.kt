package com.hujiang.devart.base

import android.app.Fragment
import android.app.FragmentManager

/**
 * Created by rarnu on 3/24/16.
 */
class BaseFragmentStateAdapter: FragmentStatePagerAdapter {

    private var list: MutableList<Fragment?>? = null

    constructor(fm: FragmentManager?, listFragment: MutableList<Fragment?>?): super(fm) {
        list = listFragment
    }

    override fun getItem(position: Int): Fragment? = list!![position]

    override fun getCount(): Int = list!!.size
}