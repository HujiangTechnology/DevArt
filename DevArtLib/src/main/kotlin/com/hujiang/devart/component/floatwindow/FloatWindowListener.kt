package com.hujiang.devart.component.floatwindow

import android.view.View

/**
 * Created by rarnu on 3/25/16.
 */
interface FloatWindowListener {

    fun onPositionChanged(v: View?, x: Int, y: Int)
    fun onFloatWindowClick()
    fun onFloatWindowLongClick()
}