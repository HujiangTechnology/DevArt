package com.hujiang.devart.component.daemon

import android.app.Application
import android.content.Context

/**
 * Created by rarnu on 4/7/16.
 */
abstract class DaemonApplication: Application {

    abstract fun getDaemonConfigurations(): DaemonConfigurations?

    private var _daemonClient: IDaemonClient? = null
    private var _hasAttachBaseContext = false

    constructor() {
        _daemonClient = DaemonClient(getDaemonConfigurations())
    }


    override fun attachBaseContext(base: Context?) {
        if(_hasAttachBaseContext){
            return
        }
        _hasAttachBaseContext = true
        super.attachBaseContext(base)
        _daemonClient?.onAttachBaseContext(base)
        attachBaseContextByDaemon(base)
    }

    open fun attachBaseContextByDaemon(context: Context?){ }
}