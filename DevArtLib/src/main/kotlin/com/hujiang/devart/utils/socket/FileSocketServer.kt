package com.hujiang.devart.utils.socket

import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.net.ServerSocket
import java.net.Socket
import java.util.*

/**
 * Created by rarnu on 3/29/16.
 */
class FileSocketServer {

    private var _receiving = false
    private var _server: ServerSocket? = null
    private var _savePath: String? = null
    private var _port = 0
    private var _callback: SocketServerCallback? = null

    constructor(callback: SocketServerCallback?, port: Int, savePath: String?) {
        _savePath = savePath
        _port = port
        _callback = callback
    }

    fun startListen() = Thread({
        try {
            _receiving = true
            _server = ServerSocket(_port)
            while (_receiving) {
                val client = _server?.accept()
                doReceiveFile(client)
                client?.close()
            }
        } catch (e: Exception) {
            _callback?.onError(e.message)
        }

    }).start()

    private fun doReceiveFile(client: Socket?) = try {
        val randomId = Random(System.currentTimeMillis()).nextInt(65536)
        val dis = DataInputStream(client?.inputStream)
        val bufferSize = 1024
        val buf = ByteArray(bufferSize)
        var passedlen = 0L
        val savePathReal = _savePath + dis.readUTF()
        val fileOut = DataOutputStream(FileOutputStream(savePathReal))
        val len = dis.readLong()
        _callback?.onReceiveFile(randomId, savePathReal, len, 0L, 0)
        var count: Int
        while (true) {
            count = dis.read(buf)
            if (count == -1) {
                break
            }
            passedlen += count
            fileOut.write(buf, 0, count)
            _callback?.onReceiveFile(randomId, savePathReal, len, passedlen, 2)
        }
        dis.close()
        fileOut.close()
        _callback?.onReceiveFile(randomId, savePathReal, len, len, 1)
    } catch (e: Exception) {
        _callback?.onError(e.message)
    }


    fun stopListen() {
        _receiving = false
        try {
            _server?.close()
        } catch (e: Exception) {
        }
    }

}