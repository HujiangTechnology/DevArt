package com.hujiang.devart.server

import android.util.Log
import java.io.IOException
import java.net.InetSocketAddress

/**
 * Created by rarnu on 4/27/16.
 */
class ServerRunnable: Runnable {

    var _timeout = 0
    var _bindException: IOException? = null
    var _hasBinded = false

    constructor(timeout: Int) {
        _timeout = timeout
    }

    override fun run() {
        try {
            with(HTTPServer.instance()!!) {
                _serverSocket?.bind(if (_hostname != null) InetSocketAddress(_hostname, _port) else InetSocketAddress(_port))
            }
            _hasBinded = true
        } catch (e: IOException) {
            _bindException = e
            return
        }
        do {
            try {
                val finalAccept = HTTPServer.instance()?._serverSocket?.accept()
                if (_timeout > 0) {
                    finalAccept?.soTimeout = _timeout
                }
                val inputStream = finalAccept?.inputStream
                HTTPServer.instance()?._asyncRunner?.exec(HTTPServer.instance()?.createClientHandler(finalAccept, inputStream))
            } catch (e: IOException) {
                Log.e("LOG", "Communication with the client broken ${e.message}")
            }
        } while (!HTTPServer.instance()!!._serverSocket!!.isClosed)
    }
}