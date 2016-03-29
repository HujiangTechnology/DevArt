package com.hujiang.devart.utils.socket

/**
 * Created by rarnu on 3/29/16.
 */
interface SocketClientCallback {

    fun onCallback(msg: String?)

    fun onError(msg: String?)

    /**
     * @param fileName
     * @param total
     * @param progress
     * @param status   0: start<br/>1: end<br/>2: progress
     */
    fun onSendFile(id: Int, fileName: String, total: Long, progress: Long, status: Int)

}