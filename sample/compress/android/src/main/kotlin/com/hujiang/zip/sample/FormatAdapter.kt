package com.hujiang.zip.sample

import android.content.Context
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.BaseAdapter
import android.widget.TextView

/**
 * Created by rarnu on 7/18/16.
 */
class FormatAdapter: BaseAdapter {

    private var ctx: Context? = null
    private var inflater: LayoutInflater? = null
    private var list: List<FormatInfo>? = null

    constructor(ctx: Context?, list: List<FormatInfo>?): super() {
        this.ctx = ctx
        this.inflater = LayoutInflater.from(this.ctx)
        this.list = list
    }

    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? {
        var v = convertView
        if (v == null) {
            v = inflater?.inflate(R.layout.item_format, parent, false)
        }
        var holder = v?.tag as FormatHolder?
        if (holder == null) {
            holder = FormatHolder()
            holder.tvFormat = v?.findViewById(R.id.tvFormat) as TextView?
            v?.tag = holder
        }

        val item = list!![position]
        holder.tvFormat?.text = item.format
        return v
    }

    override fun getItem(position: Int): Any? = list!![position]

    override fun getItemId(position: Int): Long = position.toLong()

    override fun getCount(): Int = list!!.size

    class FormatHolder {
        var tvFormat: TextView? = null
    }

}