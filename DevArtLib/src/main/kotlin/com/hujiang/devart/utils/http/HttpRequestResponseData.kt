package com.hujiang.devart.utils.http

import org.apache.http.client.CookieStore
import java.io.Serializable

/**
 * Created by rarnu on 3/25/16.
 */
class HttpRequestResponseData: Serializable {
    var cookie: CookieStore? = null
    var `data`: String? = null
}

