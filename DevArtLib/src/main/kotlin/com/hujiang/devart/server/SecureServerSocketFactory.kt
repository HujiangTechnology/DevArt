package com.hujiang.devart.server

import java.net.ServerSocket
import javax.net.ssl.SSLServerSocket
import javax.net.ssl.SSLServerSocketFactory

/**
 * Created by rarnu on 4/27/16.
 */
class SecureServerSocketFactory: ServerSocketFactory {

    var _sslServerSocketFactory: SSLServerSocketFactory? = null
    var _sslProtocols: Array<String?>? = null

    constructor(sslServerSocketFactory: SSLServerSocketFactory?, sslProtocols: Array<String?>?) {
        _sslServerSocketFactory = sslServerSocketFactory
        _sslProtocols = sslProtocols
    }

    override fun create(): ServerSocket? {
        var ss: SSLServerSocket?
        ss = _sslServerSocketFactory?.createServerSocket() as SSLServerSocket
        if (_sslProtocols != null) {
            ss.enabledProtocols = _sslProtocols
        } else {
            ss.enabledProtocols = ss.supportedProtocols
        }
        ss.useClientMode = false
        ss.wantClientAuth = false
        ss.needClientAuth = false
        return ss
    }
}