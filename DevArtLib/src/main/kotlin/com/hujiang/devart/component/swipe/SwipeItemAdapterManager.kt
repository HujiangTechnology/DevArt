package com.hujiang.devart.component.swipe

import android.view.View
import android.widget.BaseAdapter

/**
 * Created by rarnu on 3/31/16.
 */
class SwipeItemAdapterManager: SwipeItemManager {

    protected var adapter: BaseAdapter? = null

    constructor(adapter: BaseAdapter?): super(adapter) {
        this.adapter = adapter
    }

    override fun initialize(target: View?, position: Int) {
        val resId = getSwipeLayoutId(position)
        val onLayoutListener = OnLayoutListener(position)
        val swipeLayout = target?.findViewById(resId) as SwipeLayout? ?: throw IllegalStateException("can not find SwipeLayout in target view")
        val swipeMemory = SwipeMemory(position)
        swipeLayout.addSwipeListener(swipeMemory)
        swipeLayout.addOnLayoutListener(onLayoutListener)
        swipeLayout.setTag(resId, ValueBox(position, swipeMemory, onLayoutListener))
        _shownLayouts.add(swipeLayout)
    }

    override fun updateConvertView(target: View?, position: Int) {
        val resId = getSwipeLayoutId(position)
        val swipeLayout = target?.findViewById(resId) as SwipeLayout? ?: throw IllegalStateException("can not find SwipeLayout in target view")
        val valueBox = swipeLayout.getTag(resId) as ValueBox
        valueBox.swipeMemory?.setPosition(position)
        valueBox.onLayoutListener?.setPosition(position)
        valueBox.position = position
    }

    override fun bindView(target: View?, position: Int) { }
}