package com.hujiang.devart.component.swipe

import android.view.View
import android.view.ViewGroup
import android.widget.BaseAdapter

/**
 * Created by rarnu on 3/31/16.
 */
abstract class BaseSwipeAdapter: BaseAdapter(), SwipeItemManageIntf, SwipeAdapterInterface {

    protected var itemManger = SwipeItemAdapterManager(this)

    abstract override fun getSwipeLayoutResourceId(position: Int): Int

    abstract fun generateView(position: Int, parent: ViewGroup?): View?

    abstract fun fillValues(position: Int, convertView: View?)

    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
        var v = convertView
        if (v == null) {
            v = generateView(position, parent)
            itemManger.initialize(v, position)
        } else {
            itemManger.updateConvertView(v, position)
        }
        fillValues(position, v)
        return v
    }

    override fun openItem(position: Int) = itemManger.openItem(position)

    override fun closeItem(position: Int) = itemManger.closeItem(position)

    override fun closeAllExcept(layout: SwipeLayout?) = itemManger.closeAllExcept(layout)

    override fun closeAllItems() = itemManger.closeAllItems()

    override fun getOpenItems(): MutableList<Int>? = itemManger.getOpenItems()

    override fun getOpenLayouts(): MutableList<SwipeLayout>? = itemManger.getOpenLayouts()

    override fun removeShownLayouts(layout: SwipeLayout?) = itemManger.removeShownLayouts(layout)

    override fun isOpen(position: Int): Boolean = itemManger.isOpen(position)

    override fun getMode(): SwipeLayout.Mode = itemManger.getMode()

    override fun setMode(mode: SwipeLayout.Mode) = itemManger.setMode(mode)

}