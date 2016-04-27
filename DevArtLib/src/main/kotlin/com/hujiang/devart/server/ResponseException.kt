package com.hujiang.devart.server

/**
 * Created by rarnu on 4/27/16.
 */
class ResponseException: Exception {

    companion object {
        val serialVersionUID = 6569838532917408380L
    }
    var _status: Response.Status? = null

    constructor(status: Response.Status?, message: String?): super(message) {
        _status = status
    }

    constructor(status: Response.Status?, message: String?, e: Exception?): super(message, e) {
        _status = status
    }

    fun getStatus(): Response.Status? = _status

}