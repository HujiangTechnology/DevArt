package com.hujiang.devart.component.calendar

import android.content.Context
import android.util.AttributeSet
import android.widget.GridView
import android.widget.LinearLayout

/**
 * Created by rarnu on 4/5/16.
 */
class LightCalendarView: LinearLayout {

    private var _grid: GridView? = null
    private var _adapter: LightCalendarAdapter? = null


    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        init()
    }

    constructor(context: Context, attrs: AttributeSet?): this(context,  attrs, 0)

    constructor(context: Context): this(context, null)

    private fun init() {
        orientation = VERTICAL
        _grid = GridView(context)
        _grid?.numColumns = 7
        _grid?.stretchMode = GridView.STRETCH_COLUMN_WIDTH
        val lllpGrid = LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT)
        _grid?.layoutParams = lllpGrid
        _grid?.isFocusable = false
        _grid?.isClickable = false
        _grid?.isEnabled = false
        _grid?.isVerticalScrollBarEnabled = false
        _grid?.isHorizontalScrollBarEnabled = false
        addView(_grid)
    }

    fun setDate(year: Int, month: Int, isMondayFirst: Boolean) {
        _adapter = LightCalendarAdapter(context, year, month)
        _adapter?.setMondayFirstDay(isMondayFirst)
        _grid?.adapter = _adapter
    }

    fun setWeekHeight(h: Int) {
        _adapter?.setItemHeight(h)
    }

}