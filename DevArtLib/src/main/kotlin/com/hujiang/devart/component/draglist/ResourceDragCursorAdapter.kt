package com.hujiang.devart.component.draglist

import android.content.Context
import android.database.Cursor
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup

/**
 * Created by rarnu on 4/1/16.
 */
abstract class ResourceDragCursorAdapter: BaseDragCursorAdapter {

    private var _layout = 0
    private var _dropDownLayout = 0
    private var _inflater: LayoutInflater? = null

    constructor(context: Context, layout: Int, c: Cursor?, autoRequery: Boolean): super(context, c, autoRequery) {
        init(context, layout)
    }

    constructor(context: Context, layout: Int, c: Cursor?, flags: Int): super(context, c, flags) {
        init(context, layout)
    }

    private fun init(context: Context, layout: Int) {
        _layout = layout
        _dropDownLayout = layout
        _inflater = LayoutInflater.from(context)
    }

    override fun newView(context: Context?, cursor: Cursor?, parent: ViewGroup?): View? = _inflater?.inflate(_layout, parent, false)

    override fun newDropDownView(context: Context?, cursor: Cursor?, parent: ViewGroup?): View? = _inflater?.inflate(_dropDownLayout, parent, false)

    fun setViewResource(layout: Int) {
        _layout = layout
    }

    fun setDropDownViewResource(dropDownLayout: Int) {
        _dropDownLayout = dropDownLayout
    }
}