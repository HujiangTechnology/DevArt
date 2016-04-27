package com.hujiang.devart.server

import android.util.Log
import java.io.InputStream
import java.io.OutputStream
import java.net.Socket
import java.net.SocketException
import java.net.SocketTimeoutException

/**
 * Created by rarnu on 4/27/16.
 */
class ClientHandler: Runnable {

    var _inputStream: InputStream? = null
    var _acceptSocket: Socket? = null

    constructor(inputStream: InputStream?, acceptSocket: Socket?) {
        _inputStream = inputStream
        _acceptSocket = acceptSocket
    }

    fun close() {
        HTTPServer.safeClose(_inputStream)
        HTTPServer.safeClose(_acceptSocket)
    }

    override fun run() {
        var outputStream: OutputStream? = null
        try {
            outputStream = _acceptSocket?.outputStream
            val tempFileManager = HTTPServer.instance()?._tempFileManagerFactory?.create()
            val session = HTTPSession(tempFileManager, _inputStream, outputStream, _acceptSocket?.inetAddress)
            while (!_acceptSocket!!.isClosed) {
                session.execute()
            }
        } catch (e: Exception) {
            if (e !is SocketException && e.message == "HTTPServer Shutdown" && e !is SocketTimeoutException) {
                Log.e("LOG", "Communication with the client broken, or an bug in the handler code: ${e.message}")
            }
        } finally {
            HTTPServer.safeClose(outputStream)
            HTTPServer.safeClose(_inputStream)
            HTTPServer.safeClose(_acceptSocket)
            HTTPServer.instance()?._asyncRunner?.closed(this)
        }
    }

}