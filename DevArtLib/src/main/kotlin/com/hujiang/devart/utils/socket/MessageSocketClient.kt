package com.hujiang.devart.utils.socket

import java.io.DataInputStream
import java.io.DataOutputStream
import java.net.Socket
import kotlin.concurrent.thread

/**
 * Created by rarnu on 3/29/16.
 */
class MessageSocketClient {

    private var _socket: Socket? = null
    private var _dos: DataOutputStream? = null
    private var _dis: DataInputStream? = null
    private var _callback: SocketClientCallback? = null

    constructor(callback: SocketClientCallback?, ip: String?, port: Int) {
        _callback = callback
        thread {
            try {
                _socket = Socket(ip, port)
                _dos = DataOutputStream(_socket?.outputStream)
                _dis = DataInputStream(_socket?.inputStream)
            } catch (e: Exception) {
                _callback?.onError(e.message)
            }
        }
    }

    fun close() {
        _dos?.close()
        _dis?.close()
        try {
            _socket?.close()
        } catch (e: Exception) {
        }
    }

    fun sendMessage(msg: String) = thread {
        try {
            _dos?.writeUTF(msg)
            val res = _dis?.readUTF()
            _callback?.onCallback(res)
        } catch (e: Exception) {
            _callback?.onError(e.message)
        }
    }


}