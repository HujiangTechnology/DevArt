package com.hujiang.devart.utils

import java.util.*

/**
 * Created by rarnu on 3/28/16.
 */
object DatetimeUtils {

    fun calendarFromTimeInMillis(millis: Long): Calendar {
        val c = Calendar.getInstance()
        c.timeInMillis = millis
        return c
    }

    fun calendarToTimeInMillis(c: Calendar): Long = c.timeInMillis

    fun offsetYears(c: Calendar, year: Int): Calendar {
        c.add(Calendar.YEAR, year)
        return c
    }

    fun offsetMonths(c: Calendar, month: Int): Calendar {
        c.add(Calendar.MONTH, month)
        return c
    }

    fun offsetDays(c: Calendar, day: Int): Calendar {
        c.add(Calendar.DAY_OF_YEAR, day)
        return c
    }

    fun offsetHours(c: Calendar, hour: Int): Calendar {
        c.add(Calendar.HOUR_OF_DAY, hour)
        return c
    }

    fun offsetMinutes(c: Calendar, minute: Int): Calendar {
        c.add(Calendar.MINUTE, minute)
        return c
    }

    fun offsetSeconds(c: Calendar, second: Int): Calendar {
        c.add(Calendar.SECOND, second)
        return c
    }

    fun getSameWeekdaysInMonth(year: Int, month: Int, day: Int, currentYear: Int, currentMonth: Int): MutableList<Int> {
        val cThen = Calendar.getInstance()
        cThen.set(Calendar.YEAR, year)
        cThen.set(Calendar.MONTH, month - 1)
        cThen.set(Calendar.DAY_OF_MONTH, day)
        val weekThen = cThen.get(Calendar.DAY_OF_WEEK)
        val cNow = Calendar.getInstance()
        cNow.set(Calendar.YEAR, currentYear)
        cNow.set(Calendar.MONTH, currentMonth - 1)
        cNow.set(Calendar.DAY_OF_MONTH, 1)
        val listNow = arrayListOf<Int>()
        var weekNow = -1
        for (i in 0..7 - 1) {
            weekNow = cNow.get(Calendar.DAY_OF_WEEK)
            if (weekNow == weekThen) {
                break
            }
            cNow.add(Calendar.DAY_OF_MONTH, 1)
        }
        var currentDay = cNow.get(Calendar.DAY_OF_MONTH)
        while (currentDay <= 31) {
            listNow.add(currentDay)
            currentDay += 7
        }
        return listNow
    }

    fun compareWeekday(year: Int, month: Int, day: Int, currentYear: Int, currentMonth: Int, currentDay: Int): Boolean {
        val cThen = Calendar.getInstance()
        cThen.set(Calendar.YEAR, year)
        cThen.set(Calendar.MONTH, month - 1)
        cThen.set(Calendar.DAY_OF_MONTH, day)
        val cNow = Calendar.getInstance()
        cNow.set(Calendar.YEAR, currentYear)
        cNow.set(Calendar.MONTH, currentMonth - 1)
        cNow.set(Calendar.DAY_OF_MONTH, currentDay)
        val weekThen = cThen.get(Calendar.DAY_OF_WEEK)
        val weekNow = cNow.get(Calendar.DAY_OF_WEEK)
        return weekThen == weekNow
    }

    fun getNextWeekday(year: Int, month: Int, day: Int): Calendar {
        val cThen = Calendar.getInstance()
        cThen.set(Calendar.YEAR, year)
        cThen.set(Calendar.MONTH, month - 1)
        cThen.set(Calendar.DAY_OF_MONTH, day)
        val weekThen = cThen.get(Calendar.DAY_OF_WEEK)
        val cNow = Calendar.getInstance()
        cNow.add(Calendar.DAY_OF_MONTH, 1)
        var weekNow = -1
        for (i in 0..7-1) {
            weekNow = cNow.get(Calendar.DAY_OF_WEEK)
            if (weekNow == weekThen) {
                break
            }
            cNow.add(Calendar.DAY_OF_MONTH, 1)
        }
        return cNow
    }

    fun millisBetween(fromDate: Calendar, toDate: Calendar): Long = toDate.timeInMillis - fromDate.timeInMillis

    fun isDateAfter(now: Calendar, base: Calendar): Boolean = now.timeInMillis > base.timeInMillis

}