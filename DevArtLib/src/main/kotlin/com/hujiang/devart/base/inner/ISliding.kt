package com.hujiang.devart.base.inner

import android.view.View
import android.view.ViewGroup
import com.hujiang.devart.component.sliding.SlidingMenu

/**
 * Created by rarnu on 3/23/16.
 */
interface ISliding {

    fun setBehindContentView(v: View?, layoutParams: ViewGroup.LayoutParams?)

    fun setBehindContentView(v: View?)

    fun setBehindContentView(layoutResID: Int)

    fun getSlidingMenu(): SlidingMenu?

    fun toggle()

    fun showContent()

    fun showMenu()

    fun showSecondaryMenu()

    fun setSlidingActionBarEnabled(slidingActionBarEnabled: Boolean)
}