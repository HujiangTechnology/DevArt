package com.hujiang.devart.component.swipe

/**
 * Created by rarnu on 3/31/16.
 */
interface SwipeItemManageIntf {

    fun openItem(position: Int)

    fun closeItem(position: Int)

    fun closeAllExcept(layout: SwipeLayout?)

    fun closeAllItems()

    fun getOpenItems(): MutableList<Int>?

    fun getOpenLayouts(): MutableList<SwipeLayout>?

    fun removeShownLayouts(layout: SwipeLayout?)

    fun isOpen(position: Int): Boolean

    fun getMode(): SwipeLayout.Mode

    fun setMode(mode: SwipeLayout.Mode)


}