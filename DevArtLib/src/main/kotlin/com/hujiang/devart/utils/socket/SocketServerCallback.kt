package com.hujiang.devart.utils.socket

/**
 * Created by rarnu on 3/29/16.
 */
interface SocketServerCallback {

    fun onError(msg: String?)

    fun onReceiveMessage(msg: String?)

    /**
     *
     * @param fileName
     * @param total
     * @param progress
     * @param status 0: start<br/>1: end<br/>2: progress
     */
    fun onReceiveFile(id: Int, fileName: String, total: Long, progress: Long, status: Int)

}