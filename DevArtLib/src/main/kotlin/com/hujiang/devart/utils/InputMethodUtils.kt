package com.hujiang.devart.utils

import android.app.Activity
import android.content.Context
import android.view.View
import android.view.inputmethod.InputMethodManager

/**
 * Created by rarnu on 3/28/16.
 */
object InputMethodUtils {

    fun showInputMethod(context: Context, v: View?) =
            (context.getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager).showSoftInput(v, InputMethodManager.SHOW_FORCED)

    fun hideInputMethod(context: Context) =
            (context.getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager).hideSoftInputFromWindow((context as Activity).currentFocus.windowToken, InputMethodManager.HIDE_NOT_ALWAYS)

    fun toggleSoftKeyboard(context: Context) =
            (context.getSystemService(Context.INPUT_METHOD_SERVICE) as InputMethodManager).toggleSoftInput(InputMethodManager.SHOW_FORCED, 0)

}