package com.hujiang.devart.component.daemon

import android.content.Context
import android.os.Process
import java.io.BufferedReader
import java.io.File
import java.io.FileReader

/**
 * Created by rarnu on 4/7/16.
 */
class DaemonClient: IDaemonClient {


    private val DAEMON_PERMITTING_SP_FILENAME 	= "d_permit"
    private val DAEMON_PERMITTING_SP_KEY 		= "permitted"

    private var _configurations: DaemonConfigurations? = null
    private var _bufferedReader: BufferedReader? = null


    constructor(configurations: DaemonConfigurations?) {
        _configurations = configurations
    }

    override fun onAttachBaseContext(context: Context?) {
        initDaemon(context)
    }

    private fun initDaemon(context: Context?) {
        if(!isDaemonPermitting(context) || _configurations == null){
            return
        }
        val processName = getProcessName()!!
        val packageName = context?.packageName
        if(processName.startsWith(_configurations?.PERSISTENT_CONFIG?.processName!!)){
            IDaemonStrategy.Fetcher.fetchStrategy()?.onPersistentCreate(context, _configurations)
        }else if(processName.startsWith(_configurations?.DAEMON_ASSISTANT_CONFIG?.processName!!)){
            IDaemonStrategy.Fetcher.fetchStrategy()?.onDaemonAssistantCreate(context, _configurations)
        }else if(processName.startsWith(packageName!!)){
            IDaemonStrategy.Fetcher.fetchStrategy()?.onInitialization(context)
        }
        releaseIO()
    }

    private fun getProcessName(): String? {
        try {
            val file = File("/proc/" + Process.myPid() + "/" + "cmdline")
            _bufferedReader = BufferedReader(FileReader(file))
            return _bufferedReader?.readLine()
        } catch (e: Exception) {
            return null
        }
    }

    private fun isDaemonPermitting(context: Context?): Boolean {
        val sp = context?.getSharedPreferences(DAEMON_PERMITTING_SP_FILENAME, Context.MODE_PRIVATE)
        return sp!!.getBoolean(DAEMON_PERMITTING_SP_KEY, true)
    }

    private fun releaseIO(){
        if(_bufferedReader != null){
            _bufferedReader?.close()
            _bufferedReader = null
        }
    }

    protected open fun setDaemonPermiiting(context: Context?, isPermitting: Boolean): Boolean {
        val sp = context?.getSharedPreferences(DAEMON_PERMITTING_SP_FILENAME, Context.MODE_PRIVATE)
        val editor = sp?.edit()
        editor?.putBoolean(DAEMON_PERMITTING_SP_KEY, isPermitting)
        return editor?.commit()!!
    }
}