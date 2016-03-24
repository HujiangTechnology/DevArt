package com.hujiang.devart.component.sliding

/**
 * Created by rarnu on 3/24/16.
 */
interface OnSlidingPageChangeListener {

    fun onSlidingPageScrolled(position: Int, positionOffset: Float, positionOffsetPixels: Int)

    fun onSlidingPageSelected(position: Int)
}