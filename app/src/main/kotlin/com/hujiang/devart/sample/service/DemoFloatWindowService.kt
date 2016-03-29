package com.hujiang.devart.sample.service

import android.view.View
import com.hujiang.devart.component.floatwindow.FloatService
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 3/29/16.
 */
class DemoFloatWindowService: FloatService() {
    override fun getViewResId(): Int = R.layout.view_float

    override fun initView(view: View?) { }

    override fun getX(): Int = -1

    override fun getY(): Int = -1

    override fun onPositionChanged(v: View?, x: Int, y: Int) { }

    override fun onFloatWindowClick() { }

    override fun onFloatWindowLongClick() { }
}