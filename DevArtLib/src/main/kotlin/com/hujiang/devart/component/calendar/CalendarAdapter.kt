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
import java.text.SimpleDateFormat
import java.util.*

/**
 * Created by rarnu on 4/5/16.
 */
open class CalendarAdapter: BaseAdapter {

    interface CalendarAdapterCallback {
        fun onBeforeMonthChanging(year: Int, month: Int)
        fun appendDayData(year: Int, month: Int, day: Int): String
        fun getCalendarViewItem(convertView: View?, year: Int, month: Int, day: Int, lunar: String?, extra: Array<String?>?, isInMonth: Boolean, isSelected: Boolean, isToday: Boolean)
    }

    private var _isLeapyear = false
    private var _daysOfMonth = 0
    private var _dayOfWeek = 0
    private var _lastDaysOfMonth = 0
    private var _context: Context? = null
    private var _dayNumber = arrayOfNulls<String>(42)
    private var _sc: SpecialCalendar? = null
    private var _lc: LunarCalendar? = null
    private var _currentYear = ""
    private var _currentMonth = ""
    private var _currentDay = ""
    private var _sdf = SimpleDateFormat("yyyy-M-d")
    private var _currentFlag = -1
    private var _schDateTagFlag: IntArray? = null
    private var _showYear = ""
    private var _showMonth = ""
    private var _animalsYear = ""
    private var _leapMonth = ""
    private var _cyclical = ""
    private var _sysDate = ""
    private var _sysYear = ""
    private var _sysMonth = ""
    private var _sysDay = ""
    private var _currentSelected = -1
    private var _isMondayFirstDay = false
    private var _callback: CalendarAdapterCallback? = null
    private var _showLunar = true
    private var _jumpYear = 0
    private var _jumpMonth = 0
    private var _itemHeight = ViewGroup.LayoutParams.MATCH_PARENT

    constructor() {
        val date = Date()
        _sysDate = _sdf.format(date)
        _sysYear = _sysDate.split("-")[0]
        _sysMonth = _sysDate.split("-")[1]
        _sysDay = _sysDate.split("-")[2]
    }

    constructor(context: Context, year: Int, month: Int, day: Int): this() {
        _context = context
        _sc = SpecialCalendar()
        _lc = LunarCalendar()
        _currentYear = year.toString()
        _currentMonth = month.toString()
        _currentDay = day.toString()
        getCalendar(_currentYear.toInt(), _currentMonth.toInt())
    }

    constructor(context: Context, jumpMonth: Int, jumpYear: Int, yearC: Int, monthC: Int, dayC: Int, callback: CalendarAdapterCallback?): this() {
        _context = context
        _callback = callback
        _sc = SpecialCalendar()
        _lc = LunarCalendar()
        _jumpMonth = jumpMonth
        _jumpYear = jumpYear
        var stepYear: Int
        var stepMonth = monthC + jumpMonth
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
        _currentDay = dayC.toString()
        getCalendar(_currentYear.toInt(), _currentMonth.toInt())
    }

    fun getCalendar(year: Int, month: Int) {
        _isLeapyear = _sc!!.isLeapYear(year)
        _daysOfMonth = _sc!!.getDaysOfMonth(_isLeapyear, month)
        _dayOfWeek = _sc!!.getWeekdayOfMonth(year, month, _isMondayFirstDay)
        _lastDaysOfMonth = _sc!!.getDaysOfMonth(_isLeapyear, month - 1)
        getweek(year, month)
    }

    private fun getweek(year: Int, month: Int) {
        var j = 1
        var lunarDay: String
        _callback?.onBeforeMonthChanging(year, month)
        setShowYear(year.toString())
        setShowMonth(month.toString())
        setAnimalsYear(LunarCalendar.animalsYear(year))
        setLeapMonth(if (_lc!!.leapMonth == 0) "" else _lc!!.leapMonth.toString())
        setCyclical(LunarCalendar.cyclical(year))
        for (i in 0.._dayNumber.size - 1) {
            if (i < _dayOfWeek) {
                val temp = _lastDaysOfMonth - _dayOfWeek + 1
                lunarDay = _lc!!.getLunarDate(year, month - 1, temp + i, false)
                _dayNumber[i] = (temp + i).toString() + "." + lunarDay
            } else if (i < _daysOfMonth + _dayOfWeek) {
                val day = (i - _dayOfWeek + 1).toString()
                lunarDay = _lc!!.getLunarDate(year, month, i - _dayOfWeek + 1, false)
                _dayNumber[i] = (i - _dayOfWeek + 1).toString() + "." + lunarDay + "."
                if (_callback != null) {
                    _dayNumber[i] += _callback!!.appendDayData(year, month, i - _dayOfWeek + 1)
                }
                if (_sysYear == year.toString() && _sysMonth == month.toString() && _sysDay == day) {
                    _currentFlag = i
                    _currentSelected = _currentFlag
                }
            } else {
                lunarDay = _lc!!.getLunarDate(year, month + 1, j, false)
                _dayNumber[i] = j.toString() + "." + lunarDay
                j++
            }
        }
    }

    fun getShowYear(): String = _showYear

    fun setShowYear(showYear: String) {
        _showYear = showYear
    }

    fun getShowMonth(): String = _showMonth

    fun setShowMonth(showMonth: String) {
        _showMonth = showMonth
    }

    fun getAnimalsYear(): String = _animalsYear

    fun setAnimalsYear(animalsYear: String) {
        _animalsYear = animalsYear
    }

    fun getLeapMonth(): String = _leapMonth

    fun setLeapMonth(leapMonth: String) {
        _leapMonth = leapMonth
    }

    fun getCyclical(): String = _cyclical

    fun setCyclical(cyclical: String) {
        _cyclical = cyclical
    }

    /**
     * override this method for your own calendar item layout
     */
    open fun getCalendarItemLayout(): Int {
        return 0
    }

    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
        var v = convertView
        if (v == null) {
            if (getCalendarItemLayout() == 0) {
                v = LayoutInflater.from(_context).inflate(R.layout.calendar_item, null)
            } else {
                v = LayoutInflater.from(_context).inflate(getCalendarItemLayout(), null)
            }
        }
        var vllp = v?.layoutParams as AbsListView.LayoutParams?
        if (vllp == null) {
            vllp = AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, _itemHeight)
        } else {
            vllp.height = _itemHeight
        }
        v?.setBackgroundColor(Color.WHITE)
        v?.layoutParams = vllp
        val dnarr = _dayNumber[position]!!.split(".")
        val d = dnarr[0]
        val dv = dnarr[1]
        var evt = false
        var extraArr: Array<String?>? = null
        if (dnarr.size > 2 && dnarr[2].trim() != "") {
            evt = true
            extraArr = arrayOfNulls<String>(dnarr.size - 2)
            for (i in 2..dnarr.size - 1) {
                extraArr[i - 2] = dnarr[i]
            }
        }
        if (getCalendarItemLayout() == 0) {
            val tvDay = v?.findViewById(R.id.tvDay) as TextView?
            val tvLunar = v?.findViewById(R.id.tvLunar) as TextView?
            val vEvent = v?.findViewById(R.id.vEvent)
            vEvent?.setBackgroundColor(_context!!.resources.getColor(R.color.skyblue))
            tvLunar?.visibility = if (_showLunar) View.VISIBLE else View.GONE
            tvDay?.text = d
            tvLunar?.text = dv
            tvDay?.setTextColor(Color.GRAY)
            tvLunar?.setTextColor(Color.GRAY)
            vEvent?.visibility = View.INVISIBLE
            if (position < _daysOfMonth + _dayOfWeek && position >= _dayOfWeek) {
                tvDay?.setTextColor(Color.BLACK)
                if (_isMondayFirstDay) {
                    if (position % 7 == 5 || position % 7 == 6) {
                        tvDay?.setTextColor(_context!!.resources.getColor(R.color.skyblue))
                        tvLunar?.setTextColor(_context!!.resources.getColor(R.color.skyblue))
                    }
                } else {
                    if (position % 7 == 0 || position % 7 == 6) {
                        tvDay?.setTextColor(_context!!.resources.getColor(R.color.skyblue))
                        tvLunar?.setTextColor(_context!!.resources.getColor(R.color.skyblue))
                    }
                }
                vEvent?.visibility = if (evt) View.VISIBLE else View.INVISIBLE
            }
            if (_currentSelected == position) {
                v?.setBackgroundColor(_context!!.resources.getColor(R.color.lightgray))
                tvDay?.setTextColor(Color.WHITE)
                tvLunar?.setTextColor(Color.WHITE)
            }
            if (_currentFlag == position) {
                v?.setBackgroundColor(_context!!.resources.getColor(R.color.skyblue))
                tvDay?.setTextColor(Color.WHITE)
                tvLunar?.setTextColor(Color.WHITE)
                vEvent?.setBackgroundColor(Color.WHITE)
            }
        } else {
                _callback?.getCalendarViewItem(convertView, _currentYear.toInt(), _currentMonth.toInt(), d.toInt(), dv, extraArr, (position < _daysOfMonth + _dayOfWeek && position >= _dayOfWeek), (_currentSelected == position), (_currentFlag == position))

        }
        return v
    }

    override fun getItem(position: Int): Any? = _dayNumber[position]

    override fun getItemId(position: Int): Long = position.toLong()

    override fun getCount(): Int = _dayNumber.size

    override fun isEnabled(position: Int): Boolean = (position < _daysOfMonth + _dayOfWeek && position >= _dayOfWeek)

    fun setShowLunar(b: Boolean) {
        _showLunar = b
        notifyDataSetChanged()
    }

    fun setMondayFirstDay(b: Boolean) {
        if (_isMondayFirstDay != b) {
            _isMondayFirstDay = b
            reloadData()
        }
    }

    fun setCalendarAdapterCallback(callback: CalendarAdapterCallback?) {
        _callback = callback
    }

    fun setItemHeight(height: Int) {
        _itemHeight = height
    }

    open fun matchScheduleDate(year: Int, month: Int, day: Int) {

    }

    fun getDateByClickItem(position: Int): String? = _dayNumber[position]

    fun getStartPositon(): Int = _dayOfWeek + 7

    fun getEndPosition(): Int = (_dayOfWeek + _daysOfMonth + 7) - 1

    fun setCurrentSelected(sel: Int) {
        _currentSelected = sel
        notifyDataSetChanged()
    }

    fun getCurrentSelected(): Int = _currentSelected

    fun getCurrentDay(): Int {
        var ret = 0
        if (_currentSelected != -1) {
            ret = (_dayNumber[_currentSelected]!!.split(".")[0]).toInt()
        }
        return ret
    }

    fun reloadData() {
        getCalendar(_currentYear.toInt(), _currentMonth.toInt())
        notifyDataSetChanged()
    }

    fun getJumpYear(): Int = _jumpYear

    fun getJumpMonth(): Int = _jumpMonth

    fun getToday(): Int = (_dayNumber[_currentFlag]!!.split(".")[0]).toInt()

}