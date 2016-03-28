package com.hujiang.devart.sample.adapter

import android.content.Context
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import com.hujiang.devart.base.BaseAdapter
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 3/27/16.
 */
class StringAdapter(context: Context, list: MutableList<String>?) : BaseAdapter<String>(context, list) {
    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
        var v = convertView
        if (v == null) {
            v = inflater.inflate(R.layout.item_adapter, parent, false)
        }
        var holder = v?.tag as StringHolder?
        if (holder == null) {
            holder = StringHolder()
            holder.tvItem = v?.findViewById(R.id.tvItem) as TextView
            v?.tag = holder
        }
        val item = list!![position]
        holder.tvItem?.text = item
        return v
    }

    override fun getValueText(item: String): String? = item
}