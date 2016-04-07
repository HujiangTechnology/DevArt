package com.hujiang.devart.component.daemon

import android.content.Context

/**
 * Created by rarnu on 4/7/16.
 */
open class NativeDaemonBase {

    var _context: Context? = null

    constructor(context: Context?) {
        _context = context
    }

    fun onDaemonDead(){
        IDaemonStrategy.Fetcher.fetchStrategy()?.onDaemonDead()
    }

}