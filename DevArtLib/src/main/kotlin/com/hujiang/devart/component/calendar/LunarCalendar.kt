package com.hujiang.devart.component.calendar

import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.*

/**
 * Created by rarnu on 4/5/16.
 */
class LunarCalendar {

    companion object {
        val chineseNumber = arrayOf("一", "二", "三", "四", "五", "六", "七", "八", "九", "十", "十一", "十二")
        var chineseDateFormat = SimpleDateFormat("yyyy年MM月dd日")
        val lunarInfo = longArrayOf(
            0x04bd8, 0x04ae0, 0x0a570, 0x054d5, 0x0d260, 0x0d950, 0x16554, 0x056a0, 0x09ad0, //
            0x055d2, 0x04ae0, 0x0a5b6, 0x0a4d0, 0x0d250, 0x1d255, 0x0b540, 0x0d6a0, 0x0ada2, //
            0x095b0, 0x14977, 0x04970, 0x0a4b0, 0x0b4b5, 0x06a50, 0x06d40, 0x1ab54, 0x02b60, //
            0x09570, 0x052f2, 0x04970, 0x06566, 0x0d4a0, 0x0ea50, 0x06e95, 0x05ad0, 0x02b60, //
            0x186e3, 0x092e0, 0x1c8d7, 0x0c950, 0x0d4a0, 0x1d8a6, 0x0b550, 0x056a0, 0x1a5b4, //
            0x025d0, 0x092d0, 0x0d2b2, 0x0a950, 0x0b557, 0x06ca0, 0x0b550, 0x15355, 0x04da0, //
            0x0a5d0, 0x14573, 0x052d0, 0x0a9a8, 0x0e950, 0x06aa0, 0x0aea6, 0x0ab50, 0x04b60, //
            0x0aae4, 0x0a570, 0x05260, 0x0f263, 0x0d950, 0x05b57, 0x056a0, 0x096d0, 0x04dd5, //
            0x04ad0, 0x0a4d0, 0x0d4d4, 0x0d250, 0x0d558, 0x0b540, 0x0b5a0, 0x195a6, 0x095b0, //
            0x049b0, 0x0a974, 0x0a4b0, 0x0b27a, 0x06a50, 0x06d40, 0x0af46, 0x0ab60, 0x09570, //
            0x04af5, 0x04970, 0x064b0, 0x074a3, 0x0ea50, 0x06b58, 0x055c0, 0x0ab60, 0x096d5, //
            0x092e0, 0x0c960, 0x0d954, 0x0d4a0, 0x0da50, 0x07552, 0x056a0, 0x0abb7, 0x025d0, //
            0x092d0, 0x0cab5, 0x0a950, 0x0b4a0, 0x0baa4, 0x0ad50, 0x055d9, 0x04ba0, 0x0a5b0, //
            0x15176, 0x052b0, 0x0a930, 0x07954, 0x06aa0, 0x0ad50, 0x05b52, 0x04b60, 0x0a6e6, //
            0x0a4e0, 0x0d260, 0x0ea65, 0x0d530, 0x05aa0, 0x076a3, 0x096d0, 0x04bd7, 0x04ad0, //
            0x0a4d0, 0x1d0b6, 0x0d250, 0x0d520, 0x0dd45, 0x0b5a0, 0x056d0, 0x055b2, 0x049b0, //
            0x0a577, 0x0a4b0, 0x0aa50, 0x1b255, 0x06d20, 0x0ada0)

        val lunarHoliday = arrayOf("0101 春节", "0115 元宵", "0505 端午", "0707 情人", "0715 中元", "0815 中秋", "0909 重阳", "1208 腊八", "1224 小年", "0100 除夕")

        val solarHoliday = arrayOf(
            "0101 元旦", "0214 情人", "0308 妇女", "0312 植树", "0315 消费者权益日", "0401 愚人", "0501 劳动", "0504 青年",
            "0512 护士", "0601 儿童", "0701 建党", "0801 建军", "0808 父亲", "0909 毛泽东逝世纪念", "0910 教师", "0928 孔子诞辰",
            "1001 国庆", "1006 老人", "1024 联合国日", "1112 孙中山诞辰纪念", "1220 澳门回归纪念", "1225 圣诞", "1226 毛泽东诞辰纪念")

        private fun yearDays(y: Int): Int {
            var sum = 348
            var i = 0x8000

            while(i > 0x8) {
                if (y >= 0 && (y - 1900) >= 0) {
                    if ((lunarInfo[y - 1900].toInt() and i) != 0)
                    sum += 1
                }
                i = i shr 1
            }
            return (sum + leapDays(y))
        }

        private fun leapDays(y: Int): Int {
            if (leapMonth(y) != 0) {
                if ((lunarInfo[y - 1900].toInt() and 0x10000) != 0) {
                    return 30
                } else {
                    return 29
                }
            } else {
                return 0
            }
        }

        private fun leapMonth(y: Int): Int {
            var result = 0
            if (y >= 0 && (y - 1900) >= 0) {
                result = (lunarInfo[y - 1900] and 0xf).toInt()
            }
            return result
        }

        private fun monthDays(y: Int, m: Int): Int {
            if ((lunarInfo[y - 1900].toInt() and (0x10000 shr m)) == 0) {
                return 29
            } else {
                return 30
            }
        }

        fun animalsYear(year: Int): String {
            val animals = arrayOf("鼠", "牛", "虎", "兔", "龙", "蛇", "马", "羊", "猴", "鸡", "狗", "猪")
            return animals[(year - 4) % 12]
        }

        private fun cyclicalm(num: Int): String {
            val gan = arrayOf("甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸")
            val zhi = arrayOf("子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥")
            return (gan[num % 10] + zhi[num % 12])
        }

        fun cyclical(year: Int): String {
            val num = year - 1900 + 36
            return (cyclicalm(num))
        }

        fun getChinaDayString(day: Int): String {
            val chineseTen = arrayOf("初", "十", "廿", "卅")
            val n = if (day % 10 == 0) { 9 } else { day % 10 - 1 }
            if (day > 30) {
                return ""
            }
            if (day == 10) {
                return "初十"
            }
            else {
                return chineseTen[day / 10] + chineseNumber[n]
            }
        }

    }

    private var _year = 0
    var year: Int
        get() = _year
        set(value) { _year = value }
    private var _month = 0;
    private var _day = 0
    private var _lunarMonth = ""
    var lunarMonth: String
        get() = _lunarMonth
        set(value) { _lunarMonth = value }
    private var _leap = false
    var leapMonth = 0


    fun getLunarDate(yearLog: Int, monthLog: Int, dayLog: Int, isday: Boolean): String {
        var baseDate: Date? = null
        var nowaday: Date? = null
        try {
            baseDate = chineseDateFormat.parse("1900年1月31日")
        } catch (e: ParseException) {

        }

        val nowadays = "${yearLog}年${monthLog}月${dayLog}日"
        try {
            nowaday = chineseDateFormat.parse(nowadays)
        } catch (e: ParseException) {
        }

        var offset = ((nowaday!!.time - baseDate!!.time) / 86400000L).toInt()
        var dayCyl = offset + 40
        var monCyl = 14
        var daysOfYear = 0
        var iYear = 1900
        while (iYear < 10000 && offset > 0) {
            daysOfYear = yearDays(iYear)
            offset -= daysOfYear
            monCyl += 12
            iYear++
        }
        if (offset < 0) {
            offset += daysOfYear
            iYear--
            monCyl -= 12
        }
        _year = iYear
        var yearCyl = iYear - 1864
        leapMonth = leapMonth(iYear)
        _leap = false
        var daysOfMonth = 0
        var iMonth = 1
        while (iMonth < 13 && offset > 0) {
            if (leapMonth > 0 && iMonth == (leapMonth + 1) && !_leap) {
                --iMonth
                _leap = true
                daysOfMonth = leapDays(_year)
            } else {
                daysOfMonth = monthDays(_year, iMonth)
            }
            offset -= daysOfMonth
            if (_leap && iMonth == (leapMonth + 1)) {
                _leap = false
            }
            if (!_leap) {
                monCyl++
            }
            iMonth++
        }
        if (offset == 0 && leapMonth > 0 && iMonth == leapMonth + 1) {
            if (_leap) {
                _leap = false
            } else {
                _leap = true
                --iMonth
                --monCyl
            }
        }
        if (offset < 0) {
            offset += daysOfMonth
            --iMonth
            --monCyl
        }
        _month = iMonth
        _lunarMonth = chineseNumber[_month - 1] + "月"
        _day = offset + 1
        if (!isday) {
            for (i in 0..solarHoliday.size - 1) {
                val sd = solarHoliday[i].split(" ")[0]
                val sdv = solarHoliday[i].split(" ")[1]
                var smonthV = monthLog.toString()
                var sdayV = dayLog.toString()
                if (monthLog < 10) {
                    smonthV = "0" + monthLog
                }
                if (dayLog < 10) {
                    sdayV = "0" + dayLog
                }
                val smd = smonthV + sdayV
                if (sd.trim().equals(smd.trim())) {
                    return sdv
                }
            }
            for (i in 0..lunarHoliday.size - 1) {
                val ld = lunarHoliday[i].split(" ")[0]
                val ldv = lunarHoliday[i].split(" ")[1]
                var lmonthV = _month.toString()
                var ldayV = _day.toString()
                if (_month < 10) {
                    lmonthV = "0" + _month
                }
                if (_day < 10) {
                    ldayV = "0" + _day
                }
                val lmd = lmonthV + ldayV
                if (ld.trim().equals(lmd.trim())) {
                    return ldv
                }
            }
        }
        if (_day == 1) {
            return chineseNumber[_month - 1] + "月"
        } else {
            return getChinaDayString(_day)
        }
    }


    override fun toString(): String {
        if (chineseNumber[_month - 1] == "一" && getChinaDayString(_day) == "初一") {
            return "农历" + _year + "年"
        } else if (getChinaDayString(_day) == "初一") {
            return chineseNumber[_month - 1] + "月"
        } else {
            return getChinaDayString(_day)
        }
    }
}