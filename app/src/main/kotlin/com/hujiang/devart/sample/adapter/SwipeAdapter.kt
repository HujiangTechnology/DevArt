package com.hujiang.devart.sample.adapter

import android.content.Context
import android.os.Handler
import android.os.Message
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.*
import com.hujiang.devart.component.swipe.BaseSwipeAdapter
import com.hujiang.devart.component.swipe.DoubleClickListener
import com.hujiang.devart.component.swipe.SwipeLayout
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 3/31/16.
 */
class SwipeAdapter : BaseSwipeAdapter {

    private var _context: Context? = null
    private var _list: MutableList<String>? = null

    private var _hDelete = object : Handler() {
        override fun handleMessage(msg: Message?) {
            _list?.removeAt(msg!!.what)
            notifyDataSetChanged()
            super.handleMessage(msg)
        }
    }

    constructor(context: Context, list: MutableList<String>?) : super() {
        _context = context
        _list = list
    }

    override fun getSwipeLayoutResourceId(position: Int): Int = R.id.swipe

    override fun generateView(position: Int, parent: ViewGroup?): View? {
        val v = LayoutInflater.from(_context).inflate(R.layout.item_swipe, parent, false)
        val swipe = v.findViewById(R.id.swipe) as SwipeLayout
        swipe.setOnDoubleClickListener(object: DoubleClickListener {
            override fun onDoubleClick(layout: SwipeLayout?, surface: Boolean) {
                Toast.makeText(_context, "onDoubleClick", Toast.LENGTH_SHORT).show()
            }
        })
        v.findViewById(R.id.btnTrash).setOnClickListener( {
            Toast.makeText(_context, position.toString(), Toast.LENGTH_SHORT).show()
        })
        return v
    }

    override fun fillValues(position: Int, convertView: View?) {
        val tvPosition = convertView?.findViewById(R.id.tvPosition) as TextView
        tvPosition.text = position.toString()
        val tvText = convertView?.findViewById(R.id.tvText) as TextView
        tvText.text = "item: ${position}"
    }

    override fun getItem(position: Int): Any? = _list!![position]

    override fun getItemId(position: Int): Long = position.toLong()

    override fun getCount(): Int = _list!!.size
}