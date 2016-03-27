package com.hujiang.devart.component.sliding

import android.view.View
import android.view.ViewGroup
import com.hujiang.devart.component.sliding.SlidingMenu

/**
 * Created by rarnu on 3/23/16.
 */
interface ISliding {

    fun setBehindContentView(v: View?, params: ViewGroup.LayoutParams?)

    fun setBehindContentView(v: View?)

    fun setBehindContentView(id: Int)

    fun getSlidingMenu(): SlidingMenu?

    fun toggle()

    fun showContent()

    fun showMenu()

    fun showSecondaryMenu()

    fun setSlidingActionBarEnabled(slidingActionBarEnabled: Boolean)
}