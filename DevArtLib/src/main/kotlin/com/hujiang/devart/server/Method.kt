package com.hujiang.devart.server

/**
 * Created by rarnu on 4/27/16.
 */
enum class Method {

    GET, PUT, POST, DELETE, HEAD, OPTIONS, TRACE, CONNECT, PATCH, PROPFIND, PROPPATCH, MKCOL, MOVE, COPY, LOCK, UNLOCK;

    companion object {
        fun lookup(method: String?): Method? {
            if (method == null)
                return null
            try {
                return valueOf(method)
            } catch (e: Exception) {
                return null
            }
        }
    }

}