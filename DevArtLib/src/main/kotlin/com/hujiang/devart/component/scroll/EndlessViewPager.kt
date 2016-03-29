package com.hujiang.devart.component.scroll

import android.content.Context
import android.support.v4.view.ViewPager
import android.util.AttributeSet
import android.view.View
import com.hujiang.devart.base.EndlessPagerAdapter

/**
 * Created by rarnu on 3/29/16.
 */
class EndlessViewPager: ViewPager {

    private var _adapter: EndlessPagerAdapter? = null
    private var _views: MutableList<View?>? = null
    private var _curPosition = 0
    private var _maxPage = Integer.MAX_VALUE
    private var _pageSelected: OnPageSelected? = null
    var pageSelected: OnPageSelected?
        get() = _pageSelected
        set(value) { _pageSelected = value }
    var endless: Boolean
        get() = _adapter!!.isEndless
        set(value) { _adapter!!.isEndless = value }

    constructor(context: Context): this(context, null)

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        init()
    }

    private fun init() {
        _views = arrayListOf<View?>()
        _adapter = EndlessPagerAdapter(_views)
        adapter = _adapter
        addOnPageChangeListener(object : OnPageChangeListener {
            override fun onPageScrollStateChanged(p0: Int) { }

            override fun onPageScrolled(p0: Int, p1: Float, p2: Int) { }

            override fun onPageSelected(position: Int) {
                _curPosition = position
                _pageSelected?.onPageSelected(position % _views!!.size)
            }

        })
    }

    fun scrollLeft() {
        currentItem = if (--_curPosition <= 0) _maxPage else _curPosition
    }

    fun scrollRight() {
        currentItem = if (++_curPosition >= _maxPage) 0 else _curPosition
    }

    fun setData(views: MutableList<View?>?) {
        _views = views
        _adapter?.setNewData(_views)
    }
}