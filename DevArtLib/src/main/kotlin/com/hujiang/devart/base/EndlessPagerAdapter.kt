package com.hujiang.devart.base

import android.support.v4.view.PagerAdapter
import android.support.v4.view.ViewPager
import android.view.View
import android.view.ViewGroup

/**
 * Created by rarnu on 3/25/16.
 */
class EndlessPagerAdapter : PagerAdapter {

    private var _views: MutableList<View?>? = null
    private var _isEndless = false
    var isEndless: Boolean
        get() = _isEndless
        set(value) {
            _isEndless = value
            if (_views != null) {
                _count = if (_isEndless) {
                    Integer.MAX_VALUE
                } else {
                    _views!!.size
                }
            }
        }
    private var _count = 0

    constructor(views: MutableList<View?>?) {
        setNewData(views)
    }

    fun setNewData(views: MutableList<View?>?) {
        _views = views
        _count = if (_isEndless) {
            Integer.MAX_VALUE
        } else {
            _views!!.size
        }
        notifyDataSetChanged()
    }

    override fun getCount(): Int = _count

    override fun destroyItem(container: ViewGroup?, position: Int, `object`: Any?) { }

    override fun instantiateItem(container: ViewGroup?, position: Int): Any? {
        try {
            (container as ViewPager?)?.addView(_views!![position % _views!!.size], 0)
        } catch (e: Exception) {
        }
        return if (_views!!.size > 0) { _views!![position % _views!!.size] } else { null }
    }

    override fun isViewFromObject(v: View?, obj: Any?): Boolean = v == obj
}