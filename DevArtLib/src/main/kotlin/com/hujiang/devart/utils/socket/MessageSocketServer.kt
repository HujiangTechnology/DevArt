package com.hujiang.devart.utils.socket

import java.net.ServerSocket

/**
 * Created by rarnu on 3/29/16.
 */
class MessageSocketServer {

    private var _server: ServerSocket? = null
    private var _running = false
    private var _callback: SocketServerCallback? = null
    private var _endChar: String? = null
    private var _port = 0


    constructor(callback: SocketServerCallback?, port: Int, endChar: String?) {
        _callback = callback
        _endChar = endChar
        _port = port
    }

    fun startListen() = Thread({
        try {
            _running = true
            _server = ServerSocket(_port)
            while (_running) {
                val client = _server?.accept()
                Thread(MessageInnerSocket(client, _callback, _endChar)).start()
            }
        } catch (e: Exception) {
            _callback?.onError(e.message)
        }
    }).start()


    fun stopListen() {
        _running = false
        try {
            _server?.close()
        } catch (e: Exception) {
        }
    }

}