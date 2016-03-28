package com.hujiang.devart.component.mutax

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter

/**
 * Created by rarnu on 3/27/16.
 */
class MutaxReceiver : BroadcastReceiver {

    var filter: IntentFilter? = null
    private var _onReceive: OnMutaxMessage? = null
    var onReceive: OnMutaxMessage?
        get() = _onReceive
        set(value) {
            _onReceive = value
        }
    private var _actionMain: String? = null
    private var _actionProgress: String? = null
    private var _actionMutax: Array<String>? = null

    constructor(main: String?, progress: String?, mutax: Array<String>?) {
        _actionMain = main
        _actionProgress = progress
        _actionMutax = mutax
        filter = IntentFilter()
        filter?.addAction(main)
        if (progress != null) {
            filter?.addAction(progress)
        }
        if (mutax != null && mutax.size != 0) {
            for (m in mutax) {
                filter?.addAction(m)
            }
        }
    }

    fun register(context: Context?) = context?.registerReceiver(this, filter)

    fun unregister(context: Context?) = context?.unregisterReceiver(this)

    override fun onReceive(context: Context?, intent: Intent?) {
        if (intent != null) {
            val action = intent.action
            if (action != null) {
                if (action == _actionMain) {
                    val operating = intent.getBooleanExtra("operating", false)
                    onReceive?.onMutaxStateChange(operating)
                } else if (action == _actionProgress) {
                    val size = intent.getIntExtra("size", 0)
                    val position = intent.getIntExtra("position", 0)
                    val name = intent.getStringExtra("name")
                    onReceive?.onMutaxProgress(name, position, size)
                } else if (actionInMutax(action)) {
                    val operating = intent.getBooleanExtra("operating", false)
                    onReceive?.onMutaxMessage(operating)
                }
            }
        }
    }

    private fun actionInMutax(action: String?): Boolean {
        var ret = false
        if (_actionMutax != null && _actionMutax!!.size != 0) {
            for (m in _actionMutax!!) {
                if (m == action) {
                    ret = true
                    break
                }
            }
        }
        return ret
    }
}