package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.TextView
import android.widget.Toast
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.calendar.CalendarAdapter
import com.hujiang.devart.component.calendar.CalendarView
import com.hujiang.devart.component.calendar.CalendarViewCallback
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 4/6/16.
 */
class CalendarFragment : BaseFragment(), CalendarViewCallback, CalendarAdapter.CalendarAdapterCallback {

    private var _cv: CalendarView? = null
    private var _tvDate: TextView? = null

    override fun getBarTitle(): Int = R.string.calendar_name

    override fun getBarTitleWithPath(): Int = R.string.calendar_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _cv = innerView?.findViewById(R.id.cv) as CalendarView
        _tvDate = innerView?.findViewById(R.id.tvDate) as TextView
    }

    override fun initEvents() {
        _cv?.setCalendarViewCallback(this)
        _cv?.setAdapterCallback(this)
    }

    override fun initLogic() {
        _tvDate?.text = "${_cv!!.getYear()} / ${_cv!!.getMonth()}"
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_calendar

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onCalendarDoubleTap(v: View?) { }

    override fun onMonthChanged(v: View?, year: Int, month: Int) {
        _tvDate?.text = "${year} / ${month}"
    }

    override fun onDayClicked(v: View?, year: Int, month: Int, day: Int, lunar: String) {
        Toast.makeText(activity, lunar, Toast.LENGTH_SHORT).show()
    }

    override fun onBeforeMonthChanging(year: Int, month: Int) { }

    override fun appendDayData(year: Int, month: Int, day: Int): String = if (month == 10 && day == 1) "1" else ""

    override fun getCalendarViewItem(convertView: View?, year: Int, month: Int, day: Int, lunar: String?, extra: Array<String?>?, isInMonth: Boolean, isSelected: Boolean, isToday: Boolean) { }
}