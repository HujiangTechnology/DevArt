package com.hujiang.devart.component.recycler

import android.graphics.Canvas
import android.view.View

/**
 * Created by rarnu on 4/25/16.
 */
interface ItemTouchUIUtil {

    fun onDraw(c: Canvas?, recyclerView: RecyclerView?, view: View?, dX: Float, dY: Float, actionState: Int, isCurrentlyActive: Boolean)

    fun onDrawOver(c:Canvas?, recyclerView: RecyclerView?, view: View?, dX: Float, dY: Float, actionState: Int, isCurrentlyActive: Boolean)

    fun clearView(view: View?)

    fun onSelected(view: View?)

}