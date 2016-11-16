package com.hujiang.devart.component.calendar

import android.content.Context
import android.graphics.Color
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.AbsListView
import android.widget.BaseAdapter
import android.widget.TextView
import com.hujiang.devart.R
import java.util.*

/**
 * Created by rarnu on 4/5/16.
 */
class LightCalendarAdapter: BaseAdapter {

    private var _daysOfMonth = 0
    private var _dayOfWeek = 0
    private var _context: Context? = null
    private var _dayNumber = arrayOfNulls<String>(42)
    private var _sc: SpecialCalendar? = null
    private var _currentYear = ""
    private var _currentMonth = ""
    private var _currentFlag = -1
    private var _isMondayFirstDay = false
    private var _itemHeight = ViewGroup.LayoutParams.MATCH_PARENT

    fun setMondayFirstDay(b: Boolean) {
        if (_isMondayFirstDay != b) {
            _isMondayFirstDay = b
            reloadData()
        }
    }

    fun setItemHeight(height: Int) {
        _itemHeight = height
    }

    constructor(context: Context, yearC: Int, monthC: Int) {
        _context = context
        _sc = SpecialCalendar()
        var stepYear: Int
        var stepMonth = monthC
        if (stepMonth > 0) {
            if (stepMonth % 12 == 0) {
                stepYear = yearC + stepMonth / 12 - 1
                stepMonth = 12
            } else {
                stepYear = yearC + stepMonth / 12
                stepMonth %= 12
            }
        } else {
            stepYear = yearC - 1 + stepMonth / 12
            stepMonth = stepMonth % 12 + 12
        }
        _currentYear = stepYear.toString()
        _currentMonth = stepMonth.toString()
        getCalendar(_currentYear.toInt(), _currentMonth.toInt())
    }

    @Suppress("UNUSED_PARAMETER")
    constructor(context: Context, year: Int, month: Int, day: Int) {
        _context = context
        _sc = SpecialCalendar()
        _currentYear = year.toString()
        _currentMonth = month.toString()
        getCalendar(_currentYear.toInt(), _currentMonth.toInt())
    }

    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
        var v = convertView
        if (v == null) {
            v = LayoutInflater.from(_context).inflate(R.layout.light_calendar_item, null)
        }
        var vllp = v?.layoutParams as AbsListView.LayoutParams?
        if (vllp == null) {
            vllp = AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, _itemHeight)
        } else {
            vllp.height = _itemHeight
        }
        v?.setBackgroundColor(Color.WHITE)
        v?.layoutParams = vllp
        val d = _dayNumber[position]
        val tvDay = v?.findViewById(R.id.tvDay) as TextView?
        tvDay?.text = d
        tvDay?.setTextColor(Color.GRAY)
        if (position < _daysOfMonth + _dayOfWeek && position >= _dayOfWeek) {
            tvDay?.setTextColor(Color.BLACK)
            if (_isMondayFirstDay) {
                if (position % 7 == 5 || position % 7 == 6) {
                    tvDay?.setTextColor(_context!!.resources.getColor(R.color.skyblue, _context?.theme))
                }
            } else {
                if (position % 7 == 0 || position % 7 == 6) {
                    tvDay?.setTextColor(_context!!.resources.getColor(R.color.skyblue, _context?.theme))
                }
            }
        }
        if (_currentFlag == position) {
            v?.setBackgroundColor(_context!!.resources.getColor(R.color.skyblue, _context?.theme))
            tvDay?.setTextColor(Color.WHITE)
        }
        return v
    }

    override fun getItem(position: Int): Any? = _dayNumber[position]

    override fun getItemId(position: Int): Long = position.toLong()

    override fun getCount(): Int = _dayNumber.size

    fun getCalendar(year: Int, month: Int) {
        val isLeapyear = _sc!!.isLeapYear(year)
        _daysOfMonth = _sc!!.getDaysOfMonth(isLeapyear, month)
        _dayOfWeek = _sc!!.getWeekdayOfMonth(year, month, _isMondayFirstDay)
        getweek(year, month)
    }

    private fun getweek(year: Int, month: Int) {
        _currentFlag = -1
        val c = Calendar.getInstance()
        val currentYear = c.get(Calendar.YEAR)
        val currentMonth = c.get(Calendar.MONTH) + 1
        val currentDay = c.get(Calendar.DAY_OF_MONTH)
        for (i in 0.._dayNumber.size - 1) {
            if (i < _dayOfWeek) {
                _dayNumber[i] = ""
            } else if (i < _daysOfMonth + _dayOfWeek) {
                _dayNumber[i] = (i - _dayOfWeek + 1).toString()
                if (year == currentYear && month == currentMonth && ((i - _dayOfWeek + 1) == currentDay)) {
                    _currentFlag = i
                }
            } else {
                _dayNumber[i] = ""
            }
        }
    }

    fun reloadData() {
        getCalendar(_currentYear.toInt(), _currentMonth.toInt())
        notifyDataSetChanged()
    }
}