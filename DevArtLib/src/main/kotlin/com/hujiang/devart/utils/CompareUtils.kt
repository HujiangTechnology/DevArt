package com.hujiang.devart.utils

import java.util.*

/**
 * Created by rarnu on 3/28/16.
 */
class CompareUtils<T> {

    companion object {
        val CU_EQUAL = 0
        val CU_LEFT_BIGGER_THAN_RIGHT = 1
        val CU_LEFT_SMALLER_THAN_RIGHT = -1
    }

    interface CompareCallback<T> {
        fun onCompare(util: CompareUtils<T>, left: T, right: T): Int
    }

    private var _comp: Comparator<T>? = null
    var comparator: Comparator<T>? = null
        get() = _comp
    private var _innerCallback: CompareCallback<T>? = null

    constructor(callback: CompareCallback<T>?) {
        _innerCallback = callback
        _comp = Comparator<T> { lhs, rhs ->
            var ret = 0
            if (_innerCallback != null) {
                ret = _innerCallback!!.onCompare(this@CompareUtils, lhs, rhs)
            }
            return@Comparator ret
        }
    }

}