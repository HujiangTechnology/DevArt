package com.hujiang.devart.sample.adapter

import android.content.Context
import android.view.View
import android.view.ViewGroup
import android.widget.Button
import android.widget.TextView
import com.hujiang.devart.base.BaseAdapter
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 4/6/16.
 */
class FlipAdapter(context: Context, list: MutableList<Item>?) : BaseAdapter<FlipAdapter.Item>(context, list), View.OnClickListener {

    private var _callback: Callback? = null
    fun setCallback(callback: Callback?) {
        _callback = callback
    }

    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
        var v = convertView
        if(v == null) {
            v = inflater.inflate(R.layout.item_flippage, parent, false)
        }
        var holder = v?.tag as FlipHolder?
        if (holder == null) {
            holder = FlipHolder()
            holder.text = v?.findViewById(R.id.text) as TextView
            holder.firstPage = v?.findViewById(R.id.first_page) as Button
            holder.lastPage = v?.findViewById(R.id.last_page) as Button
            holder.firstPage?.setOnClickListener(this)
            holder.lastPage?.setOnClickListener(this)
            v?.tag = holder
        }
        holder.text?.text = "${list!![position].getId().toString()} : ${position}"
        return v
    }

    override fun onClick(v: View?) {
        when (v!!.id){
            R.id.first_page -> _callback?.onPageRequested(0)
            R.id.last_page -> _callback?.onPageRequested(count -1)
        }
    }

    fun addItems(amount: Int) {
        for(i in 0..amount - 1){
            list?.add(Item())
        }
        notifyDataSetChanged()
    }

    fun addItemsBefore(amount: Int) {
        for(i in 0..amount - 1){
            list?.add(0, Item())
        }
        notifyDataSetChanged()
    }

    override fun hasStableIds(): Boolean = true

    override fun getValueText(item: Item): String? = ""

    interface Callback{
        fun onPageRequested(page: Int)
    }


    class Item {
        companion object {
            var id = 0L
        }
        var _id = 0L
        constructor() {
            _id = id++
        }
        fun getId(): Long = _id
    }

}