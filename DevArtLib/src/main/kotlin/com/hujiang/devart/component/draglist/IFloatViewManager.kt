package com.hujiang.devart.component.draglist

import android.graphics.Point
import android.view.View

/**
 * Created by rarnu on 4/1/16.
 */
interface IFloatViewManager {

    fun onCreateFloatView(position: Int): View?

    fun onDragFloatView(floatView: View?, location: Point?, touch: Point?)

    fun onDestroyFloatView(floatView: View?)

}