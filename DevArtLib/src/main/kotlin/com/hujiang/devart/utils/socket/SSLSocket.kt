package com.hujiang.devart.utils.socket

import org.apache.http.conn.ssl.SSLSocketFactory
import java.net.Socket
import java.security.KeyStore
import java.security.cert.X509Certificate
import javax.net.ssl.SSLContext
import javax.net.ssl.X509TrustManager

/**
 * Created by rarnu on 3/25/16.
 */
class SSLSocket: SSLSocketFactory {

    var sslContext = SSLContext.getInstance("TLS")

    constructor(trustStore: KeyStore): super(trustStore) {
        val tm = object : X509TrustManager {
            override fun checkClientTrusted(chain: Array<out X509Certificate>?, authType: String?) {

            }

            override fun checkServerTrusted(chain: Array<out X509Certificate>?, authType: String?) {

            }

            override fun getAcceptedIssuers(): Array<out X509Certificate>? {
                return null
            }

        }
        sslContext.init(null, arrayOf(tm), null)
    }

    override fun createSocket(): Socket? = sslContext.socketFactory.createSocket()

    override fun createSocket(socket: Socket?, host: String?, port: Int, autoClose: Boolean): Socket? = super.createSocket(socket, host, port, autoClose)

}