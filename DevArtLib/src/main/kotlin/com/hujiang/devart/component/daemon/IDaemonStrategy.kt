package com.hujiang.devart.component.daemon

import android.content.Context
import android.os.Build

/**
 * Created by rarnu on 4/7/16.
 */
interface IDaemonStrategy {

    fun onInitialization(context: Context?): Boolean

    fun onPersistentCreate(context: Context?, configs: DaemonConfigurations?)

    fun onDaemonAssistantCreate(context: Context?, configs: DaemonConfigurations?)

    fun onDaemonDead()

    object Fetcher {

        private var _daemonStrategy: IDaemonStrategy? = null

        fun fetchStrategy(): IDaemonStrategy? {
            if (_daemonStrategy != null) {
                return _daemonStrategy
            }
            val sdk = Build.VERSION.SDK_INT
            when (sdk) {
                23 -> _daemonStrategy = DaemonStrategy23()
                22 -> _daemonStrategy = DaemonStrategy22()
                21 -> _daemonStrategy = DaemonStrategy21()
//                {
//                    if (Build.MODEL.toLowerCase() == "mx4 pro") {
//                        _daemonStrategy = DaemonStrategyUnder21()
//                    } else {
//                        _daemonStrategy = DaemonStrategy21()
//                    }
//                }
                else -> {
                    if (Build.MODEL != null && Build.MODEL.toLowerCase().startsWith("mi")) {
                        _daemonStrategy = DaemonStrategyXiaomi()
                    } else {
                        _daemonStrategy = DaemonStrategy21()
                    }
//                    else if (Build.MODEL != null && Build.MODEL.toLowerCase().startsWith("a31")) {
//                        _daemonStrategy = DaemonStrategy21()
//                    } else {
//                        _daemonStrategy = DaemonStrategyUnder21()
//                    }
                }
            }
            return _daemonStrategy
        }
    }

}