package com.hujiang.devart.component.daemon

import android.content.Context

/**
 * Created by rarnu on 4/7/16.
 */
class DaemonConfigurations {

    var PERSISTENT_CONFIG: DaemonConfiguration? = null
    var DAEMON_ASSISTANT_CONFIG: DaemonConfiguration? = null
    var	LISTENER: DaemonListener? = null

    constructor(persistentConfig: DaemonConfiguration?, daemonAssistantConfig: DaemonConfiguration?) {
        PERSISTENT_CONFIG = persistentConfig
        DAEMON_ASSISTANT_CONFIG = daemonAssistantConfig
        LISTENER = null
    }

    constructor(persistentConfig: DaemonConfiguration?, daemonAssistantConfig: DaemonConfiguration?, listener: DaemonListener?) {
        PERSISTENT_CONFIG = persistentConfig
        DAEMON_ASSISTANT_CONFIG = daemonAssistantConfig
        LISTENER = listener
    }

    data class DaemonConfiguration(var processName: String?, var serviceName: String?, var receiverName: String?)

    interface DaemonListener {
        fun onPersistentStart(context: Context?)
        fun onDaemonAssistantStart(context: Context?)
        fun onWatchDaemonDead()
    }
}