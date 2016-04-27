package com.hujiang.devart.server

import java.net.ServerSocket

/**
 * Created by rarnu on 4/27/16.
 */
class DefaultServerSocketFactory: ServerSocketFactory {
    override fun create(): ServerSocket? = ServerSocket()
}