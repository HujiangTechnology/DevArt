package com.hujiang.devart.component.daemon

import android.content.Context

/**
 * Created by rarnu on 4/7/16.
 */
interface IDaemonClient {

    fun onAttachBaseContext(context: Context?)
}