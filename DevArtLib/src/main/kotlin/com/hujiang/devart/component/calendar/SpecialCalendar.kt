package com.hujiang.devart.component.calendar

import java.util.*

/**
 * Created by rarnu on 4/5/16.
 */
class SpecialCalendar {

    private var _daysOfMonth = 0;
    private var _dayOfWeek = 0

    fun isLeapYear(year: Int): Boolean {
        if (year % 100 == 0 && year % 400 == 0) {
            return true
        } else if (year % 100 != 0 && year % 4 == 0) {
            return true
        }
        return false
    }

    fun getDaysOfMonth(isLeapyear: Boolean, month: Int): Int {
        when (month) {
            1, 3, 5, 7, 8, 10, 12 -> _daysOfMonth = 31
            4, 6, 9, 11 -> _daysOfMonth = 30
            2 -> {
                if (isLeapyear) {
                    _daysOfMonth = 29
                } else {
                    _daysOfMonth = 28
                }
            }
        }
        return _daysOfMonth
    }

    fun getWeekdayOfMonth(year: Int, month: Int, mondayIsFirst: Boolean): Int {
        val cal = Calendar.getInstance()
        cal.set(year, month - 1, 1)
        if (mondayIsFirst) {
            _dayOfWeek = cal.get(Calendar.DAY_OF_WEEK) - 2
            if (_dayOfWeek < 0) {
                _dayOfWeek = cal.get(Calendar.DAY_OF_WEEK) + 5
            }
        } else {
            _dayOfWeek = cal.get(Calendar.DAY_OF_WEEK) - 1
        }
        return _dayOfWeek
    }

}