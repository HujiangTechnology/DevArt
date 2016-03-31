package com.hujiang.devart.component.swipe

import android.content.Context
import android.database.Cursor
import android.view.View
import android.view.ViewGroup
import android.widget.SimpleCursorAdapter

/**
 * Created by rarnu on 3/31/16.
 */
abstract class SimpleCursorSwipeAdapter: SimpleCursorAdapter, SwipeItemManageIntf, SwipeAdapterInterface {

    private var _itemManger = SwipeItemAdapterManager(this)

    constructor(context: Context, layout: Int, c: Cursor?, from: Array<String>?, to: IntArray?, flags: Int): super(context, layout, c, from, to, flags)

    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
        val convertViewIsNull = convertView == null
        val v = super.getView(position, convertView, parent)
        if(convertViewIsNull){
            _itemManger.initialize(v, position)
        }else{
            _itemManger.updateConvertView(v, position)
        }
        return v
    }

    override fun openItem(position: Int) = _itemManger.openItem(position)

    override fun closeItem(position: Int) = _itemManger.closeItem(position)

    override fun closeAllExcept(layout: SwipeLayout?) = _itemManger.closeAllExcept(layout)

    override fun closeAllItems() = _itemManger.closeAllItems()

    override fun getOpenItems(): MutableList<Int>? = _itemManger.getOpenItems()

    override fun getOpenLayouts(): MutableList<SwipeLayout>? = _itemManger.getOpenLayouts()

    override fun removeShownLayouts(layout: SwipeLayout?) = _itemManger.removeShownLayouts(layout)

    override fun isOpen(position: Int): Boolean = _itemManger.isOpen(position)

    override fun getMode(): SwipeLayout.Mode = _itemManger.getMode()

    override fun setMode(mode: SwipeLayout.Mode) = _itemManger.setMode(mode)


}