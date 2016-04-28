package com.hujiang.devart.server

import android.util.Log
import java.io.*
import java.net.InetAddress
import java.net.SocketException
import java.net.SocketTimeoutException
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.Charset
import java.util.*
import javax.net.ssl.SSLException

/**
 * Created by rarnu on 4/27/16.
 */
class HTTPSession: IHTTPSession {

    companion object {
        val REQUEST_BUFFER_LEN = 512
        val MEMORY_STORE_LIMIT = 1024
        val BUFSIZE = 8192
        val MAX_HEADER_SIZE = 1024
    }

    var _tempFileManager: TempFileManager? = null
    var _outputStream: OutputStream? = null
    var _inputStream: BufferedInputStream? = null
    var _splitbyte = 0
    var _rlen = 0
    var _uri: String? = null
    var _method: Method? = null
    var _parms: MutableMap<String?, String?>? = null
    var _headers: MutableMap<String?, String?>? = null
    var _cookies: CookieHandler? = null
    var _queryParameterString: String? = null
    var _remoteIp: String? = null
    var _remoteHostname: String? = null
    var _protocolVersion: String? = null

    constructor(tempFileManager: TempFileManager?, inputStream: InputStream?, outputStream: OutputStream?) {
        _tempFileManager = tempFileManager
        _inputStream = BufferedInputStream(inputStream, HTTPSession.BUFSIZE)
        _outputStream = outputStream
    }

    constructor(tempFileManager: TempFileManager?, inputStream: InputStream?, outputStream: OutputStream?, inetAddress: InetAddress?) {
        _tempFileManager = tempFileManager
        _inputStream = BufferedInputStream(inputStream, HTTPSession.BUFSIZE)
        _outputStream = outputStream
        _remoteIp = if (inetAddress!!.isLoopbackAddress || inetAddress.isAnyLocalAddress) "127.0.0.1" else inetAddress.hostAddress.toString()
        _remoteHostname = if (inetAddress.isLoopbackAddress || inetAddress.isAnyLocalAddress) "localhost" else inetAddress.hostName.toString()
        _headers = hashMapOf<String?, String?>()
    }

    override fun execute() {
        var r: Response? = null
        try {
            val buf = ByteArray(HTTPSession.BUFSIZE)
            _splitbyte = 0
            _rlen = 0
            var read: Int
            _inputStream?.mark(HTTPSession.BUFSIZE)
            try {
                read = _inputStream!!.read(buf, 0, HTTPSession.BUFSIZE)
            } catch (e: SSLException) {
                throw e
            } catch (e: IOException) {
                HTTPServer.safeClose(_inputStream)
                HTTPServer.safeClose(_outputStream)
                throw SocketException("HTTPServer Shutdown")
            }
            if (read == -1) {
                HTTPServer.safeClose(_inputStream)
                HTTPServer.safeClose(_outputStream)
                throw SocketException("HTTPServer Shutdown")
            }
            while (read > 0) {
                _rlen += read
                _splitbyte = findHeaderEnd(buf, _rlen)
                if (_splitbyte > 0) {
                    break
                }
                read = _inputStream!!.read(buf, _rlen, HTTPSession.BUFSIZE - _rlen)
            }

            if (_splitbyte < _rlen) {
                _inputStream?.reset()
                _inputStream?.skip(_splitbyte.toLong())
            }
            _parms = hashMapOf<String?, String?>()
            if (_headers == null) {
                _headers = hashMapOf<String?, String?>()
            } else {
                _headers?.clear()
            }
            val hin = BufferedReader(InputStreamReader(ByteArrayInputStream(buf, 0, _rlen)))
            val pre = hashMapOf<String?, String?>()
            decodeHeader(hin, pre, _parms, _headers)
            if (_remoteIp != null) {
                _headers?.put("remote-addr", _remoteIp)
                _headers?.put("http-client-ip", _remoteIp)
            }
            _method = Method.lookup(pre["method"])
            if (_method == null) {
                throw ResponseException(Response.Status.BAD_REQUEST, "BAD REQUEST: Syntax error. HTTP verb ${pre["method"]} unhandled.")
            }
            _uri = pre["uri"]
            _cookies = CookieHandler(_headers)
            val connection = _headers?.get("connection")
            val keepAlive = "HTTP/1.1" == _protocolVersion && (connection == null || !connection.matches("(?i).*close.*".toRegex()))
            r = HTTPServer.instance()?.serve(this)
            if (r == null) {
                throw ResponseException(Response.Status.INTERNAL_ERROR, "SERVER INTERNAL ERROR: Serve() returned a null response.")
            } else {
                val acceptEncoding = _headers?.get("accept-encoding")
                _cookies?.unloadQueue(r)
                r.setRequestMethod(_method)
                r.setGzipEncoding(HTTPServer.useGzipWhenAccepted(r) && acceptEncoding != null && acceptEncoding.contains("gzip"))
                r.setKeepAlive(keepAlive)
                r.send(_outputStream)
            }
            if (!keepAlive || r.isCloseConnection()) {
                throw SocketException("HTTPServer Shutdown")
            }
        } catch (e: SocketException) {
            throw e
        } catch (ste: SocketTimeoutException) {
            throw ste
        } catch (ssle: SSLException) {
            val resp = HTTPServer.newFixedLengthResponse(Response.Status.INTERNAL_ERROR, HTTPServer.MIME_PLAINTEXT, "SSL PROTOCOL FAILURE: ${ssle.message}")
            resp?.send(_outputStream)
            HTTPServer.safeClose(_outputStream)
        } catch (ioe: IOException) {
            val resp = HTTPServer.newFixedLengthResponse(Response.Status.INTERNAL_ERROR, HTTPServer.MIME_PLAINTEXT, "SERVER INTERNAL ERROR: IOException: ${ioe.message}")
            resp?.send(_outputStream)
            HTTPServer.safeClose(_outputStream)
        } catch (re: ResponseException) {
            val resp = HTTPServer.newFixedLengthResponse(re.getStatus(), HTTPServer.MIME_PLAINTEXT, re.message)
            resp?.send(_outputStream)
            HTTPServer.safeClose(_outputStream)
        } finally {
            HTTPServer.safeClose(r)
            _tempFileManager?.clear()
        }
    }

    override fun getCookies(): CookieHandler? = _cookies
    override fun getHeaders(): MutableMap<String?, String?>? = _headers
    override fun getInputStream(): InputStream? = _inputStream
    override fun getMethod(): Method? = _method
    override fun getParms(): MutableMap<String?, String?>? = _parms
    override fun getQueryParameterString(): String? = _queryParameterString
    override fun getUri(): String? = _uri

    fun getBodySize(): Long {
        if (_headers!!.containsKey("content-length")) {
            return _headers!!["content-length"]!!.toLong()
        } else if (_splitbyte < _rlen) {
            return (_rlen - _splitbyte).toLong()
        }
        return 0
    }

    fun getTmpBucket(): RandomAccessFile? {
        try {
            val tempFile = _tempFileManager?.createTempFile(null)
            return RandomAccessFile(tempFile?.getName(), "rw")
        } catch (e: Exception) {
            throw Error(e)
        }
    }

    override fun parseBody(files: MutableMap<String?, String?>?) {
        var randomAccessFile: RandomAccessFile? = null
        try {
            var size = getBodySize()
            var baos: ByteArrayOutputStream? = null
            var requestDataOutput: DataOutput?
            if (size < MEMORY_STORE_LIMIT) {
                baos = ByteArrayOutputStream()
                requestDataOutput = DataOutputStream(baos)
            } else {
                randomAccessFile = getTmpBucket()
                requestDataOutput = randomAccessFile
            }
            val buf = ByteArray(REQUEST_BUFFER_LEN)
            while (_rlen >= 0 && size > 0) {
                _rlen = _inputStream!!.read(buf, 0, Math.min(size, REQUEST_BUFFER_LEN.toLong()).toInt())
                size -= _rlen
                if (_rlen > 0) {
                    requestDataOutput?.write(buf, 0, _rlen)
                }
            }
            var fbuf: ByteBuffer?
            if (baos != null) {
                fbuf = ByteBuffer.wrap(baos.toByteArray(), 0, baos.size())
            } else {
                fbuf = randomAccessFile?.channel?.map(FileChannel.MapMode.READ_ONLY, 0, randomAccessFile.length())
                randomAccessFile?.seek(0)
            }

            if (Method.POST == _method) {
                val contentType = ContentType(_headers?.get("content-type"))
                if (contentType.isMultipart()) {
                    @Suppress("UNUSED_VARIABLE")
                    val boundary = contentType.getBoundary() ?: throw ResponseException(Response.Status.BAD_REQUEST, "BAD REQUEST: Content type is multipart/form-data but boundary missing. Usage: GET /example/file.html")
                    decodeMultipartFormData(contentType, fbuf, _parms, files)
                } else {
                    val postBytes = ByteArray(fbuf!!.remaining())
                    fbuf.get(postBytes)
                    val postLine = String(postBytes, Charset.forName(contentType.getEncoding())).trim()
                    if (contentType.getContentType()?.toLowerCase() == "application/x-www-form-urlencoded") {
                        decodeParms(postLine, _parms)
                    } else if (postLine.length != 0) {
                        files?.put("postData", postLine)
                    }
                }
            } else if (Method.PUT == _method) {
                files?.put("content", saveTmpFile(fbuf, 0, fbuf!!.limit(), null))
            }
        } finally {
            HTTPServer.safeClose(randomAccessFile)
        }
    }

    fun saveTmpFile(b: ByteBuffer?, offset: Int, len: Int, filenameHint: String?): String? {
        var path: String? = ""
        if (len > 0) {
            var fileOutputStream: FileOutputStream? = null
            try {
                val tempFile = _tempFileManager?.createTempFile(filenameHint)
                var src = b?.duplicate()
                fileOutputStream = FileOutputStream(tempFile?.getName())
                val dest = fileOutputStream.channel
                src?.position(offset)?.limit(offset + len)
                dest.write(src?.slice())
                path = tempFile?.getName()
            } catch (e: Exception) {
                throw Error(e)
            } finally {
                HTTPServer.safeClose(fileOutputStream)
            }
        }
        return path
    }


    fun decodeHeader(inr: BufferedReader?, pre: MutableMap<String?, String?>?, parms: MutableMap<String?, String?>?, headers: MutableMap<String?, String?>?) {
        try {
            val inLine = inr?.readLine() ?: return
            val st = StringTokenizer(inLine)
            if (!st.hasMoreTokens()) {
                throw ResponseException(Response.Status.BAD_REQUEST, "BAD REQUEST: Syntax error. Usage: GET /example/file.html")
            }
            pre?.put("method", st.nextToken())
            if (!st.hasMoreTokens()) {
                throw ResponseException(Response.Status.BAD_REQUEST, "BAD REQUEST: Missing URI. Usage: GET /example/file.html")
            }
            var uri = st.nextToken()
            val qmi = uri.indexOf('?')
            if (qmi >= 0) {
                decodeParms(uri.substring(qmi + 1), parms)
                uri = HTTPServer.decodePercent(uri.substring(0, qmi))
            } else {
                uri = HTTPServer.decodePercent(uri)
            }
            if (st.hasMoreTokens()) {
                _protocolVersion = st.nextToken()
            } else {
                _protocolVersion = "HTTP/1.1"
                Log.e("LOG", "no protocol version specified, strange. Assuming HTTP/1.1.")
            }
            var line = inr?.readLine()
            while (line != null && !line.trim().isEmpty()) {
                val p = line.indexOf(':')
                if (p >= 0) {
                    headers?.put(line.substring(0, p).trim().toLowerCase(Locale.US), line.substring(p + 1).trim())
                }
                line = inr?.readLine()
            }

            pre?.put("uri", uri)
        } catch (e: Exception) {
            throw ResponseException(Response.Status.INTERNAL_ERROR, "SERVER INTERNAL ERROR: IOException: ${e.message}", e)
        }
    }

    fun scipOverNewLine(partHeaderBuff: ByteArray?, index: Int): Int {
        var nindex = index
        while (partHeaderBuff!![nindex] != '\n'.toByte()) {
            nindex++
        }
        return ++nindex
    }

    fun decodeParms(parms: String?, p: MutableMap<String?, String?>?) {
        if (parms == null) {
            _queryParameterString = ""
            return
        }
        _queryParameterString = parms
        val st = StringTokenizer(parms, "&")
        while (st.hasMoreTokens()) {
            val e = st.nextToken()
            val sep = e.indexOf('=')
            if (sep >= 0) {
                p?.put(HTTPServer.decodePercent(e.substring(0, sep))?.trim(), HTTPServer.decodePercent(e.substring(sep + 1)))
            } else {
                p?.put(HTTPServer.decodePercent(e)?.trim(), "")
            }
        }
    }

    fun findHeaderEnd(buf: ByteArray?, rlen: Int): Int {
        var splitbyte = 0
        while (splitbyte + 1 < rlen) {
            if (buf!![splitbyte] == '\r'.toByte() && buf[splitbyte + 1] == '\n'.toByte() && splitbyte + 3 < rlen && buf[splitbyte + 2] == '\r'.toByte() && buf[splitbyte + 3] == '\n'.toByte()) {
                return splitbyte + 4
            }
            if (buf[splitbyte] == '\n'.toByte() && buf[splitbyte + 1] == '\n'.toByte()) {
                return splitbyte + 2
            }
            splitbyte++
        }
        return 0
    }

    fun decodeMultipartFormData(contentType: ContentType?, fbuf: ByteBuffer?, parms: MutableMap<String?, String?>?, files: MutableMap<String?, String?>?) {
        var pcount = 0
        try {
            val boundaryIdxs = getBoundaryPositions(fbuf, contentType?.getBoundary()?.toByteArray())
            if (boundaryIdxs!!.size < 2) {
                throw ResponseException(Response.Status.BAD_REQUEST, "BAD REQUEST: Content type is multipart/form-data but contains less than two boundary strings.")
            }
            val partHeaderBuff = ByteArray(MAX_HEADER_SIZE)
            for (boundaryIdx in 0..boundaryIdxs.size - 1 - 1) {
                fbuf?.position(boundaryIdxs[boundaryIdx])
                val len = if (fbuf!!.remaining() < MAX_HEADER_SIZE) fbuf.remaining() else MAX_HEADER_SIZE
                fbuf.get(partHeaderBuff, 0, len)
                val inr = BufferedReader(InputStreamReader(ByteArrayInputStream(partHeaderBuff, 0, len), Charset.forName(contentType?.getEncoding())), len)
                var headerLines = 0
                var mpline = inr.readLine()
                headerLines++
                if (mpline == null || !mpline.contains(contentType?.getBoundary().toString())) {
                    throw ResponseException(Response.Status.BAD_REQUEST, "BAD REQUEST: Content type is multipart/form-data but chunk does not start with boundary.")
                }
                var partName: String? = null
                var fileName: String? = null
                var partContentType: String? = null
                mpline = inr.readLine()
                headerLines++
                while (mpline != null && mpline.trim().length > 0) {
                    var matcher = HTTPServer.CONTENT_DISPOSITION_PATTERN.matcher(mpline)
                    if (matcher.matches()) {
                        val attributeString = matcher.group(2)
                        matcher = HTTPServer.CONTENT_DISPOSITION_ATTRIBUTE_PATTERN.matcher(attributeString)
                        while (matcher.find()) {
                            val key = matcher.group(1)
                            if (key.toLowerCase() == "name") {
                                partName = matcher.group(2)
                            } else if (key.toLowerCase() == "filename") {
                                fileName = matcher.group(2)
                                if (!fileName.isEmpty()) {
                                    if (pcount > 0) {
                                        partName += pcount++.toString()
                                    } else {
                                        pcount++
                                    }
                                }
                            }
                        }
                    }
                    matcher = HTTPServer.CONTENT_TYPE_PATTERN.matcher(mpline)
                    if (matcher.matches()) {
                        partContentType = matcher.group(2).trim()
                    }
                    mpline = inr.readLine()
                    headerLines++
                }
                var partHeaderLength = 0
                while (headerLines-- > 0) {
                    partHeaderLength = scipOverNewLine(partHeaderBuff, partHeaderLength)
                }
                if (partHeaderLength >= len - 4) {
                    throw ResponseException(Response.Status.INTERNAL_ERROR, "Multipart header size exceeds MAX_HEADER_SIZE.")
                }
                var partDataStart = boundaryIdxs[boundaryIdx] + partHeaderLength
                var partDataEnd = boundaryIdxs[boundaryIdx + 1] - 4
                fbuf.position(partDataStart)
                if (partContentType == null) {
                    val dataBytes = ByteArray(partDataEnd - partDataStart)
                    fbuf.get(dataBytes)
                    parms?.put(partName, String(dataBytes, Charset.forName(contentType?.getEncoding())))
                } else {
                    val path = saveTmpFile(fbuf, partDataStart, partDataEnd - partDataStart, fileName)
                    if (!files!!.containsKey(partName)) {
                        files.put(partName, path)
                    } else {
                        var count = 2
                        while (files.containsKey(partName + count)) {
                            count++
                        }
                        files.put(partName + count, path)
                    }
                    parms?.put(partName, fileName)
                }
            }
        } catch (re: ResponseException) {
            throw re
        } catch (e: Exception) {
            throw ResponseException(Response.Status.INTERNAL_ERROR, e.toString())
        }
    }

    fun getBoundaryPositions(b: ByteBuffer?, boundary: ByteArray?): IntArray? {
        var res = IntArray(0)
        if (b!!.remaining() < boundary!!.size) {
            return res
        }
        var searchWindowPos = 0
        val searchWindow = ByteArray(4 * 1024 + boundary.size)
        val firstFill = if (b.remaining() < searchWindow.size) b.remaining() else searchWindow.size
        b.get(searchWindow, 0, firstFill)
        var newBytes = firstFill - boundary.size
        do {
            for (j in 0..newBytes - 1) {
                for (i in 0..boundary.size - 1) {
                    if (searchWindow[j + i] != boundary[i]) {
                        break
                    }
                    if (i == boundary.size - 1) {
                        val newRes = IntArray(res.size + 1)
                        System.arraycopy(res, 0, newRes, 0, res.size)
                        newRes[res.size] = searchWindowPos + j
                        res = newRes
                    }
                }
            }
            searchWindowPos += newBytes
            System.arraycopy(searchWindow, searchWindow.size - boundary.size, searchWindow, 0, boundary.size)
            newBytes = searchWindow.size - boundary.size
            newBytes = if (b.remaining() < newBytes) b.remaining() else newBytes
            b.get(searchWindow, boundary.size, newBytes)
        } while (newBytes > 0)

        return res
    }

    override fun getRemoteIpAddress(): String? = _remoteIp

    override fun getRemoteHostName(): String? = _remoteHostname

}