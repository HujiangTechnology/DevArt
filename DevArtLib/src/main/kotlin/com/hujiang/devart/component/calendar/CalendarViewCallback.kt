package com.hujiang.devart.component.calendar

import android.view.View

/**
 * Created by rarnu on 4/5/16.
 */
interface CalendarViewCallback {

    fun onCalendarDoubleTap(v: View?)
    fun onMonthChanged(v: View?, year: Int, month: Int)
    fun onDayClicked(v: View?, year: Int, month: Int, day: Int, lunar: String)

}