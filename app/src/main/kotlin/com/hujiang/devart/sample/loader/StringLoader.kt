package com.hujiang.devart.sample.loader

import android.content.Context
import com.hujiang.devart.base.BaseLoader
import java.util.*

/**
 * Created by rarnu on 3/27/16.
 */
class StringLoader(context: Context?) : BaseLoader<String>(context) {

    override fun loadInBackground(): MutableList<String>? {
        val list = arrayListOf<String>()
        val r = Random(65535)
        for (i in 1..100) {
            list.add(r.nextInt().toString())
        }
        return list
    }
}