package com.hujiang.devart.server

import java.io.InputStream

/**
 * Created by rarnu on 4/27/16.
 */
interface IHTTPSession {
    fun execute()
    fun getCookies(): CookieHandler?
    fun getHeaders(): MutableMap<String?, String?>?
    fun getInputStream(): InputStream?
    fun getMethod(): Method?
    fun getParms(): MutableMap<String?, String?>?
    fun getQueryParameterString(): String?
    fun getUri(): String?
    fun parseBody(files: MutableMap<String?, String?>?)
    fun getRemoteIpAddress(): String?
    fun getRemoteHostName(): String?

}