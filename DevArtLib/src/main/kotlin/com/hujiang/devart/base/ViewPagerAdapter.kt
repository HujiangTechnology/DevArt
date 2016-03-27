package com.hujiang.devart.base

import android.support.v4.view.PagerAdapter
import android.support.v4.view.ViewPager
import android.view.View
import android.view.ViewGroup

/**
 * Created by rarnu on 3/25/16.
 */
class ViewPagerAdapter : PagerAdapter {

    private var _views: MutableList<View?>? = null

    constructor(views: MutableList<View?>?) {
        setNewData(views)
    }

    fun setNewData(views: MutableList<View?>?) {
        _views = views
        notifyDataSetChanged()
    }

    override fun getCount(): Int {
        return _views!!.size
    }

    override fun destroyItem(container: ViewGroup?, position: Int, `object`: Any?) {

    }

    override fun instantiateItem(container: ViewGroup?, position: Int): Any? {
        try {
            (container as ViewPager?)?.addView(_views!![position], 0)
        } catch (e: Exception) {
        }
        return if (_views!!.size > 0) {
            _views!![position]
        } else {
            null
        }
    }

    override fun isViewFromObject(v: View?, obj: Any?): Boolean = v == obj
}