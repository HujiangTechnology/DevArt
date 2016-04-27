package com.hujiang.devart.server

/**
 * Created by rarnu on 4/27/16.
 */
class CookieHandler: Iterable<String?> {


    val _cookies = hashMapOf<String?, String?>()
    val _queue = arrayListOf<Cookie?>()

    constructor(httpHeaders: MutableMap<String?, String?>?) {
        val raw = httpHeaders?.get("cookie")
        if (raw != null) {
            val tokens = raw.split(";")
            for (token in tokens) {
                val data = token.trim().split("=")
                if (data.size == 2) {
                    _cookies.put(data[0], data[1])
                }
            }
        }
    }

    fun delete(name: String?) = set(name, "-delete-", -30)
    override fun iterator(): Iterator<String?> = _cookies.keys.iterator()
    fun read(name: String?): String? = _cookies[name]
    fun set(cookie: Cookie?) = _queue.add(cookie)
    fun set(name: String?, value: String?, expires: Int) = _queue.add(Cookie(name, value, Cookie.getHTTPTime(expires)))

    fun unloadQueue(response: Response?) {
        for (cookie in _queue) {
            response?.addHeader("Set-Cookie", cookie?.getHTTPHeader())
        }
    }
}