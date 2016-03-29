package com.hujiang.devart.utils.socket

import java.io.DataInputStream
import java.io.DataOutputStream
import java.net.Socket

/**
 * Created by rarnu on 3/29/16.
 */
class MessageInnerSocket : Runnable {

    private var _client: Socket? = null
    private var _listMsg: String? = null
    private var _callback: SocketServerCallback? = null
    private var _endChar: String? = null

    constructor(client: Socket?, callback: SocketServerCallback?, endChar: String?) {
        _client = client
        _callback = callback
        _endChar = endChar
    }

    override fun run() {
        try {
            val input = DataInputStream(_client?.inputStream)
            val output = DataOutputStream(_client?.outputStream)
            while (true) {
                _listMsg = input.readUTF()
                if (_listMsg != null) {
                    _callback?.onReceiveMessage(_listMsg)
                    output.writeUTF(_listMsg)
                    if (_listMsg == _endChar) {
                        break
                    }
                }
            }
        } catch (e: Exception) {
            _callback?.onError(e.message)
        }
    }
}