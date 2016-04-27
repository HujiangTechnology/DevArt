package com.hujiang.devart.server

import java.text.SimpleDateFormat
import java.util.*

/**
 * Created by rarnu on 4/27/16.
 */
class Cookie {

    companion object {
        fun getHTTPTime(days: Int): String? {
            val calendar = Calendar.getInstance()
            val dateFormat = SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US)
            dateFormat.timeZone = TimeZone.getTimeZone("GMT")
            calendar.add(Calendar.DAY_OF_MONTH, days)
            return dateFormat.format(calendar.time)
        }
    }

    var _n: String? = null
    var _v: String? = null
    var _e: String? = null

    constructor(name: String?, value: String?): this(name, value, 30)

    constructor(name: String?, value: String?, numDays: Int) {
        _n = name
        _v = value
        _e = getHTTPTime(numDays)
    }

    constructor(name: String?, value: String?, expires: String?) {
        _n = name
        _v = value
        _e = expires
    }
    fun getHTTPHeader(): String? = "${_n}=${_v}; expires=${_e}"

}