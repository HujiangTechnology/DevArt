package com.hujiang.devart.server

import java.util.*

/**
 * Created by rarnu on 4/27/16.
 */
class DefaultAsyncRunner: AsyncRunner {

    var _requestCount = 0L
    val _running = Collections.synchronizedList(arrayListOf<ClientHandler?>())
    fun getRunning(): MutableList<ClientHandler?>? = _running

    override fun closeAll() {
        for (clientHandler in _running) {
            clientHandler?.close()
        }
    }

    override fun closed(clientHandler: ClientHandler?) {
        _running.remove(clientHandler)
    }

    override fun exec(code: ClientHandler?) {
        ++_requestCount
        val t = Thread(code)
        t.isDaemon = true
        t.name = "HTTPServer Request Processor (#$_requestCount)"
        _running.add(code)
        t.start()
    }
}