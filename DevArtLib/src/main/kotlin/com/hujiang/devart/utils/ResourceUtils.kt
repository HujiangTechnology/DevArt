package com.hujiang.devart.utils

import android.content.Context

/**
 * Created by rarnu on 3/28/16.
 */
object ResourceUtils {

    private var _context: Context? = null

    fun initResource(context: Context) {
        _context = context
    }

    fun getString(res: Int): String? = _context?.getString(res)

    fun getString(res: Int, vararg params: Any?): String? = _context?.getString(res, params)

    fun getStringArray(res: Int): Array<String>? = _context?.resources?.getStringArray(res)

}