package com.hujiang.devart.component.swipe

/**
 * Created by rarnu on 3/31/16.
 */
interface SwipeListener {

    fun onStartOpen(layout: SwipeLayout?)

    fun onOpen(layout: SwipeLayout?)

    fun onStartClose(layout: SwipeLayout?)

    fun onClose(layout: SwipeLayout?)

    fun onUpdate(layout: SwipeLayout?, leftOffset: Int, topOffset: Int)

    fun onHandRelease(layout: SwipeLayout?, xvel: Float, yvel: Float)

}