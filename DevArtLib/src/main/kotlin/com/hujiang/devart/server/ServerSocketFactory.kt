package com.hujiang.devart.server

import java.net.ServerSocket

/**
 * Created by rarnu on 4/27/16.
 */
interface ServerSocketFactory {
    fun create(): ServerSocket?
}