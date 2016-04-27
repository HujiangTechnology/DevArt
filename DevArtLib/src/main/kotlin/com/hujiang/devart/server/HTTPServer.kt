package com.hujiang.devart.server

import android.util.Log
import java.io.ByteArrayInputStream
import java.io.Closeable
import java.io.IOException
import java.io.InputStream
import java.net.ServerSocket
import java.net.Socket
import java.net.URLDecoder
import java.nio.charset.Charset
import java.security.KeyStore
import java.util.*
import java.util.regex.Pattern
import javax.net.ssl.*

/**
 * Created by rarnu on 4/27/16.
 */
abstract class HTTPServer {

    companion object {
        private var _instace: HTTPServer? = null
        fun instance(): HTTPServer? = _instace

        val SOCKET_READ_TIMEOUT = 5000
        val MIME_PLAINTEXT = "text/plain"
        val MIME_HTML = "text/html"
        val QUERY_STRING_PARAMETER = "HTTPServer.QUERY_STRING"
        var MIME_TYPES: MutableMap<String?, String?>? = null
        val CONTENT_DISPOSITION_REGEX = "([ |\t]*Content-Disposition[ |\t]*:)(.*)"
        val CONTENT_DISPOSITION_PATTERN = Pattern.compile(CONTENT_DISPOSITION_REGEX, Pattern.CASE_INSENSITIVE)
        val CONTENT_TYPE_REGEX = "([ |\t]*content-type[ |\t]*:)(.*)"
        val CONTENT_TYPE_PATTERN = Pattern.compile(CONTENT_TYPE_REGEX, Pattern.CASE_INSENSITIVE)
        val CONTENT_DISPOSITION_ATTRIBUTE_REGEX = "[ |\t]*([a-zA-Z]*)[ |\t]*=[ |\t]*['|\"]([^\"^']*)['|\"]"
        val CONTENT_DISPOSITION_ATTRIBUTE_PATTERN = Pattern.compile(CONTENT_DISPOSITION_ATTRIBUTE_REGEX)

        fun safeClose(closeable: Any?) {
            try {
                if (closeable != null) {
                    if (closeable is Closeable) {
                        closeable.close()
                    } else if (closeable is Socket) {
                        closeable.close()
                    } else if (closeable is ServerSocket) {
                        closeable.close()
                    } else {
                        throw IllegalArgumentException("Unknown object to close")
                    }
                }
            } catch (e: IOException) {
                Log.e("LOG", "Could not close => ${e.message}")
            }
        }

        fun decodePercent(str: String?): String? {
            var decoded: String? = null
            try {
                decoded = URLDecoder.decode(str, "UTF8")
            } catch (e: Exception) {
                Log.e("LOG", "Encoding not supported, ignored ${e.message}")
            }
            return decoded
        }

        fun useGzipWhenAccepted(r: Response?): Boolean = r?.getMimeType() != null && r?.getMimeType()?.toLowerCase()!!.contains("text/")

        fun newChunkedResponse(status: Response.IStatus?, mimeType: String?, data: InputStream?): Response? = Response(status, mimeType, data, -1)

        fun newFixedLengthResponse(status:Response.IStatus?, mimeType: String?, data: InputStream?, totalBytes: Long): Response? = Response(status, mimeType, data, totalBytes)

        fun newFixedLengthResponse(status: Response.IStatus?, mimeType: String?, txt: String?): Response? {
            var contentType: ContentType? = ContentType(mimeType)
            if (txt == null) {
                return newFixedLengthResponse(status, mimeType, ByteArrayInputStream(ByteArray(0)), 0)
            } else {
                var bytes: ByteArray
                try {
                    val newEncoder = Charset.forName(contentType?.getEncoding()).newEncoder()
                    if (!newEncoder.canEncode(txt)) {
                        contentType = contentType?.tryUTF8()
                    }
                    bytes = txt.toByteArray(Charset.forName(contentType?.getEncoding()))
                } catch (e: Exception) {
                    Log.e("LOG", "encoding problem, responding nothing: ${e.message}")
                    bytes = ByteArray(0)
                }
                return newFixedLengthResponse(status, contentType?.getContentTypeHeader(), ByteArrayInputStream(bytes), bytes.size.toLong())
            }
        }

        fun newFixedLengthResponse(msg: String?): Response? {
            return newFixedLengthResponse(Response.Status.OK, MIME_HTML, msg)
        }

        fun mimeTypes(): MutableMap<String?, String?>? {
            if (MIME_TYPES == null) {
                MIME_TYPES = hashMapOf<String?, String?>()
                loadMimeTypes(MIME_TYPES, "META-INF/HTTPServer/default-mimetypes.properties")
                loadMimeTypes(MIME_TYPES, "META-INF/HTTPServer/mimetypes.properties")
                if (MIME_TYPES!!.isEmpty()) {
                    Log.e("LOG", "no mime types found in the classpath! please provide mimetypes.properties")
                }
            }
            return MIME_TYPES
        }

        fun loadMimeTypes(result: MutableMap<String?, String?>?, resourceName: String?) {
            try {
                val resources = HTTPServer.javaClass.classLoader.getResources(resourceName)
                while (resources.hasMoreElements()) {
                    val url = resources.nextElement()
                    val properties = Properties()
                    var stream: InputStream? = null
                    try {
                        stream = url.openStream()
                        properties.load(url.openStream())
                    } catch (e: IOException) {
                        Log.e("LOG", "could not load mimetypes from ${url}, ${e.message}")
                    } finally {
                        safeClose(stream)
                    }
                    result?.putAll(properties as Map<String?, String?>)
                }
            } catch (e: IOException) {
                Log.e("LOG", "no mime types available at ${resourceName}")
            }
        }

        fun makeSSLSocketFactory(loadedKeyStore: KeyStore?, keyManagers: Array<KeyManager?>?): SSLServerSocketFactory? {
            var res: SSLServerSocketFactory?
            try {
                val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm())
                trustManagerFactory.init(loadedKeyStore)
                val ctx = SSLContext.getInstance("TLS")
                ctx.init(keyManagers, trustManagerFactory.trustManagers, null)
                res = ctx.serverSocketFactory
            } catch (e: Exception) {
                throw IOException(e.message)
            }
            return res
        }

        fun makeSSLSocketFactory(loadedKeyStore: KeyStore?, loadedKeyFactory: KeyManagerFactory?): SSLServerSocketFactory? {
            try {
                return makeSSLSocketFactory(loadedKeyStore, loadedKeyFactory?.keyManagers)
            } catch (e: Exception) {
                throw IOException(e.message)
            }
        }

        fun makeSSLSocketFactory(keyAndTrustStoreClasspathPath: String?, passphrase: CharArray?): SSLServerSocketFactory? {
            try {
                val keystore = KeyStore.getInstance(KeyStore.getDefaultType());
                val keystoreStream = HTTPServer.javaClass.getResourceAsStream(keyAndTrustStoreClasspathPath) ?: throw IOException("Unable to load keystore from classpath: ${keyAndTrustStoreClasspathPath}")
                keystore.load(keystoreStream, passphrase)
                val keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm())
                keyManagerFactory.init(keystore, passphrase)
                return makeSSLSocketFactory(keystore, keyManagerFactory)
            } catch (e: Exception) {
                throw IOException(e.message)
            }
        }

        fun getMimeTypeForFile(uri: String?): String? {
            val dot = uri!!.lastIndexOf('.')
            var mime: String? = null
            if (dot >= 0) {
                mime = mimeTypes()?.get(uri.substring(dot + 1).toLowerCase())
            }
            return if (mime == null) "application/octet-stream" else mime
        }

        fun decodeParameters(parms: MutableMap<String?, String?>?): MutableMap<String?, MutableList<String?>?>? = decodeParameters(parms?.get(QUERY_STRING_PARAMETER))

        fun decodeParameters(queryString: String?): MutableMap<String?, MutableList<String?>?>? {
            val parms = hashMapOf<String?, MutableList<String?>?>()
            if (queryString != null) {
                val st = StringTokenizer(queryString, "&")
                while (st.hasMoreTokens()) {
                    val e = st.nextToken()
                    val sep = e.indexOf('=')
                    val propertyName = if (sep >= 0) decodePercent(e.substring(0, sep))?.trim() else decodePercent(e)?.trim()
                    if (!parms.containsKey(propertyName)) {
                        parms.put(propertyName, arrayListOf<String?>())
                    }
                    val propertyValue = if (sep >= 0) decodePercent(e.substring(sep + 1)) else null
                    if (propertyValue != null) {
                        parms[propertyName]?.add(propertyValue)
                    }
                }
            }
            return parms
        }


    }

    var _hostname: String? = null
    var _port = 0
    var _serverSocket: ServerSocket? = null
    var _serverSocketFactory: ServerSocketFactory? = DefaultServerSocketFactory()
    var _thread: Thread? = null
    var _asyncRunner: AsyncRunner? = null
    var _tempFileManagerFactory: TempFileManagerFactory? = null

    constructor(port: Int): this(null, port)
    constructor(hostname: String?, port: Int) {
        _instace = this
        _hostname = hostname
        _port = port
        setTempFileManagerFactory(DefaultTempFileManagerFactory())
        setAsyncRunner(DefaultAsyncRunner())
    }

    fun createClientHandler(finalAccept: Socket?, inputStream: InputStream?): ClientHandler? = ClientHandler(inputStream, finalAccept)

    fun setAsyncRunner(asyncRunner: AsyncRunner?) {
        _asyncRunner = asyncRunner
    }

    fun setTempFileManagerFactory(tempFileManagerFactory: TempFileManagerFactory?) {
        _tempFileManagerFactory = tempFileManagerFactory
    }

    open fun serve(session: IHTTPSession?): Response? {
        val files = hashMapOf<String?, String?>()
        val method = session?.getMethod()
        if (Method.PUT.equals(method) || Method.POST.equals(method)) {
            try {
                session?.parseBody(files)
            } catch (ioe: IOException) {
                return newFixedLengthResponse(Response.Status.INTERNAL_ERROR, MIME_PLAINTEXT, "SERVER INTERNAL ERROR: IOException: ${ioe.message}")
            } catch (re: ResponseException) {
                return newFixedLengthResponse(re.getStatus(), MIME_PLAINTEXT, re.message)
            }
        }
        val parms = session?.getParms()
        parms?.put(QUERY_STRING_PARAMETER, session?.getQueryParameterString())
        return serve(session?.getUri(), method, session?.getHeaders(), parms, files)
    }

    open fun serve(uri: String?, method: Method?, headers: MutableMap<String?, String?>?, parms: MutableMap<String?, String?>?, files: MutableMap<String?, String?>?): Response?  =newFixedLengthResponse(Response.Status.NOT_FOUND, MIME_PLAINTEXT, "Not Found")

    fun closeAllConnections() = stop()

    fun createServerRunnable(timeout: Int): ServerRunnable? = ServerRunnable(timeout)

    fun getListeningPort(): Int = if (_serverSocket == null) -1 else _serverSocket!!.localPort

    fun isAlive(): Boolean = wasStarted() && !_serverSocket!!.isClosed && _thread!!.isAlive

    fun getServerSocketFactory(): ServerSocketFactory? = _serverSocketFactory

    fun setServerSocketFactory(serverSocketFactory: ServerSocketFactory?) {
        _serverSocketFactory = serverSocketFactory
    }

    fun getHostname(): String? = _hostname

    fun getTempFileManagerFactory(): TempFileManagerFactory? = _tempFileManagerFactory

    fun makeSecure(sslServerSocketFactory: SSLServerSocketFactory?, sslProtocols: Array<String?>?) {
        _serverSocketFactory = SecureServerSocketFactory(sslServerSocketFactory, sslProtocols)
    }

    fun start() = start(SOCKET_READ_TIMEOUT)

    fun start(timeout: Int) = start(timeout, true)

    fun start(timeout: Int, daemon: Boolean) {
        _serverSocket = getServerSocketFactory()?.create()
        _serverSocket?.reuseAddress = true
        val serverRunnable = createServerRunnable(timeout)
        _thread = Thread(serverRunnable)
        _thread?.isDaemon = daemon
        _thread?.name = "HTTPServer Main Listener"
        _thread?.start()
        while (!serverRunnable!!._hasBinded && serverRunnable._bindException == null) {
            try {
                Thread.sleep(10L)
            } catch (e: Throwable) {

            }
        }
        if (serverRunnable._bindException != null) {
            throw serverRunnable._bindException!!
        }
    }

    fun stop() {
        try {
            safeClose(_serverSocket)
            _asyncRunner?.closeAll()
            _thread?.join()
        } catch (e: Exception) {
            Log.e("LOG", "Could not stop all connections, ${e.message}")
        }
    }

    fun wasStarted(): Boolean = _serverSocket != null && _thread != null

}