package com.hujiang.devart.server

import android.util.Log
import java.io.*
import java.text.SimpleDateFormat
import java.util.*
import java.util.zip.GZIPOutputStream

/**
 * Created by rarnu on 4/27/16.
 */
class Response: Closeable {

    interface IStatus {
        fun getDescription(): String?
        fun getRequestStatus(): Int
    }

    enum class Status : IStatus {
        SWITCH_PROTOCOL(101, "Switching Protocols"),
        OK(200, "OK"),
        CREATED(201, "Created"),
        ACCEPTED(202, "Accepted"),
        NO_CONTENT(204, "No Content"),
        PARTIAL_CONTENT(206, "Partial Content"),
        MULTI_STATUS(207, "Multi-Status"),
        REDIRECT(301, "Moved Permanently"),
        REDIRECT_SEE_OTHER(303, "See Other"),
        NOT_MODIFIED(304, "Not Modified"),
        BAD_REQUEST(400, "Bad Request"),
        UNAUTHORIZED(401, "Unauthorized"),
        FORBIDDEN(403, "Forbidden"),
        NOT_FOUND(404, "Not Found"),
        METHOD_NOT_ALLOWED(405, "Method Not Allowed"),
        NOT_ACCEPTABLE(406, "Not Acceptable"),
        REQUEST_TIMEOUT(408, "Request Timeout"),
        CONFLICT(409, "Conflict"),
        RANGE_NOT_SATISFIABLE(416, "Requested Range Not Satisfiable"),
        INTERNAL_ERROR(500, "Internal Server Error"),
        NOT_IMPLEMENTED(501, "Not Implemented"),
        UNSUPPORTED_HTTP_VERSION(505, "HTTP Version Not Supported");

        var _requestStatus = 0
        var _description: String? = null

        constructor(requestStatus: Int, description: String?) {
            _requestStatus = requestStatus
            _description = description
        }

        override fun getDescription(): String? = "$_requestStatus $_description"
        override fun getRequestStatus(): Int = _requestStatus
    }

    class ChunkedOutputStream(out: OutputStream?) : FilterOutputStream(out) {

        override fun write(oneByte: Int) {
            val data = byteArrayOf(oneByte.toByte())
            write(data, 0, 1)
        }

        override fun write(buffer: ByteArray?) {
            write(buffer, 0, buffer!!.size)
        }

        override fun write(buffer: ByteArray?, offset: Int, length: Int) {
            if (length == 0) {
                return
            }
            out.write(String.format("%x\r\n", length).toByteArray())
            out.write(buffer, offset, length)
            out.write("\r\n".toByteArray())
        }

        fun finish() {
            out.write("0\r\n\r\n".toByteArray())
        }

    }

    var _status: IStatus? = null
    var _mimeType: String? = null
    var _data: InputStream? = null
    var _contentLength = 0L
    val _lowerCaseHeader = hashMapOf<String?, String?>()
    val _header = object : HashMap<String?, String?>() {
        override fun put(key: String?, value: String?): String? {
            _lowerCaseHeader.put(if (key == null) key else key.toLowerCase(), value)
            return super.put(key, value)
        }
    }
    var _requestMethod: Method? = null
    var _chunkedTransfer = false
    var _encodeAsGzip = false
    var _keepAlive = false

    constructor(status: IStatus?, mimeType: String?, data: InputStream?, totalBytes: Long) {
        _status = status
        _mimeType = mimeType
        if (data == null) {
            _data = ByteArrayInputStream(ByteArray(0))
            _contentLength = 0L
        } else {
            _data = data
            _contentLength = totalBytes
        }
        _chunkedTransfer = _contentLength < 0
        _keepAlive = true
    }

    override fun close() {
        _data?.close()
    }

    fun addHeader(name: String?, value: String?) = _header.put(name, value)

    fun closeConnection(close: Boolean) =
            if (close) {
                _header.put("connection", "close")
            } else {
                _header.remove("connection")
            }

    fun isCloseConnection(): Boolean = (getHeader("connection")) == "close"
    fun getData(): InputStream? = _data
    fun getHeader(name: String?): String? = _lowerCaseHeader[name?.toLowerCase()]
    fun getMimeType(): String? = _mimeType
    fun getRequestMethod(): Method? = _requestMethod
    fun getStatus(): IStatus? = _status

    fun setGzipEncoding(encodeAsGzip: Boolean) {
        _encodeAsGzip = encodeAsGzip
    }

    fun setKeepAlive(useKeepAlive: Boolean) {
        _keepAlive = useKeepAlive
    }

    fun send(outputStream: OutputStream?) {
        val gmtFrmt = SimpleDateFormat("E, d MMM yyyy HH:mm:ss 'GMT'", Locale.US)
        gmtFrmt.timeZone = TimeZone.getTimeZone("GMT")
        try {
            if (_status == null) {
                throw Error("sendResponse(): Status can't be null.")
            }
            val pw = PrintWriter(BufferedWriter(OutputStreamWriter(outputStream, ContentType(_mimeType).getEncoding())), false)
            pw.append("HTTP/1.1 ").append(_status?.getDescription()).append(" \r\n")
            if (_mimeType != null) {
                printHeader(pw, "Content-Type", _mimeType)
            }
            if (getHeader("date") == null) {
                printHeader(pw, "Date", gmtFrmt.format(Date()))
            }
            for (entry in _header.entries) {
                printHeader(pw, entry.key, entry.value)
            }
            if (getHeader("connection") == null) {
                printHeader(pw, "Connection", (if (_keepAlive) "keep-alive" else "close"))
            }
            if (getHeader("content-length") != null) {
                _encodeAsGzip = false
            }
            if (_encodeAsGzip) {
                printHeader(pw, "Content-Encoding", "gzip")
                setChunkedTransfer(true)
            }
            var pending = if (_data != null) _contentLength else 0
            if (_requestMethod != Method.HEAD && _chunkedTransfer) {
                printHeader(pw, "Transfer-Encoding", "chunked")
            } else if (!_encodeAsGzip) {
                pending = sendContentLengthHeaderIfNotAlreadyPresent(pw, pending)
            }
            pw.append("\r\n")
            pw.flush()
            sendBodyWithCorrectTransferAndEncoding(outputStream, pending)
            outputStream?.flush()
            HTTPServer.safeClose(_data)
        } catch (e: Exception) {
            Log.e("LOG", "Could not send response to the client => ${e.message}")
        }
    }

    fun printHeader(pw: PrintWriter?, key: String?, value: String?) {
        pw?.append(key)?.append(": ")?.append(value)?.append("\r\n")
    }

    fun sendContentLengthHeaderIfNotAlreadyPresent(pw: PrintWriter?, defaultSize: Long): Long {
        val contentLengthString = getHeader("content-length")
        var size = defaultSize
        if (contentLengthString != null) {
            try {
                size = contentLengthString.toLong()
            } catch (e: Exception) {
                Log.e("LOG", "content-length was no number $contentLengthString")
            }
        }
        pw?.print("Content-Length: $size\r\n")
        return size
    }

    fun sendBodyWithCorrectTransferAndEncoding(outputStream: OutputStream?, pending: Long) {
        if (_requestMethod != Method.HEAD && _chunkedTransfer) {
            var chunkedOutputStream = ChunkedOutputStream(outputStream)
            sendBodyWithCorrectEncoding(chunkedOutputStream, -1)
            chunkedOutputStream.finish()
        } else {
            sendBodyWithCorrectEncoding(outputStream, pending)
        }
    }

    fun sendBodyWithCorrectEncoding(outputStream: OutputStream?, pending: Long) {
        if (_encodeAsGzip) {
            val gzipOutputStream = GZIPOutputStream(outputStream)
            sendBody(gzipOutputStream, -1)
            gzipOutputStream.finish()
        } else {
            sendBody(outputStream, pending)
        }
    }

    fun sendBody(outputStream: OutputStream?, pending: Long) {
        var npending = pending
        var BUFFER_SIZE = 16 * 1024 * 1L
        val buff = ByteArray(BUFFER_SIZE.toInt())
        val sendEverything = npending == -1L
        while (npending > 0 || sendEverything) {
            val bytesToRead = if (sendEverything) BUFFER_SIZE else Math.min(pending, BUFFER_SIZE)
            val read = _data!!.read(buff, 0, bytesToRead.toInt())
            if (read <= 0) {
                break
            }
            outputStream?.write(buff, 0, read)
            if (!sendEverything) {
                npending -= read
            }
        }
    }

    fun setChunkedTransfer(chunkedTransfer: Boolean) {
        _chunkedTransfer = chunkedTransfer
    }

    fun setData(data: InputStream?) {
        _data = data
    }

    fun setMimeType(mimeType: String?) {
        _mimeType = mimeType
    }

    fun setRequestMethod(requestMethod: Method?) {
        _requestMethod = requestMethod
    }

    fun setStatus(status: IStatus?) {
        _status = status
    }

}