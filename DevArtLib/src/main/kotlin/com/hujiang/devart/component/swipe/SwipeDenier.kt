package com.hujiang.devart.component.swipe

import android.view.MotionEvent

/**
 * Created by rarnu on 3/31/16.
 */
interface SwipeDenier {

    fun shouldDenySwipe(ev: MotionEvent?): Boolean

}