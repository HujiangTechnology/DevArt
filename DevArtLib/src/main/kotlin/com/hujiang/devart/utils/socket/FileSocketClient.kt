package com.hujiang.devart.utils.socket

import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.File
import java.io.FileInputStream
import java.net.Socket
import java.util.*

/**
 * Created by rarnu on 3/29/16.
 */
class FileSocketClient {

    private var _callback: SocketClientCallback? = null
    private var _ip: String? = null
    private var _port = 0
    private var _socket: Socket? = null

    constructor(callback: SocketClientCallback?, ip: String?, port: Int) {
        _callback = callback
        _ip = ip
        _port = port
    }

    fun sendFile(filePath: String) = Thread({
        try {
            val randomId = Random(System.currentTimeMillis()).nextInt(65536)
            val file = File(filePath)
            _socket = Socket(_ip, _port)
            val dos = DataOutputStream(_socket?.outputStream)
            val dis = DataInputStream(FileInputStream(filePath))
            val buffferSize = 1024
            val bufArray = ByteArray(buffferSize)
            var passedlen = 0L
            val len = file.length()
            dos.writeUTF(file.name)
            dos.flush()
            dos.writeLong(len)
            dos.flush()
            _callback?.onSendFile(randomId, filePath, len, 0L, 0)
            var count: Int
            while (true) {
                count = dis.read(bufArray)
                if (count == -1) {
                    break
                }
                passedlen += count
                dos.write(bufArray, 0, count)
                _callback?.onSendFile(randomId, filePath, len, passedlen, 2)
            }
            dos.flush()
            dos.close()
            dis.close()
            _socket?.close()
            _callback?.onSendFile(randomId, filePath, len, len, 1)
        } catch (e: Exception) {
            _callback?.onError(e.message)
        }
    }).start()


    fun stop() = try {
        _socket?.close()
    } catch (e: Exception) {
    }

}