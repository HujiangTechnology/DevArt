package com.hujiang.devart.component.swipe

import android.view.View

/**
 * Created by rarnu on 3/31/16.
 */
interface OnRevealListener {

    fun onReveal(child: View?, edge: SwipeLayout.DragEdge, fraction: Float, distance: Int)
}
