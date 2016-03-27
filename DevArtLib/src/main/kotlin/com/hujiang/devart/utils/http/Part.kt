package com.hujiang.devart.utils.http

import org.apache.http.util.EncodingUtils
import java.io.ByteArrayOutputStream
import java.io.OutputStream

/**
 * Created by rarnu on 3/25/16.
 */
abstract class Part {

    companion object {
        val BOUNDARY = "----------------314159265358979323846"
        val BOUNDARY_BYTES = EncodingUtils.getAsciiBytes(BOUNDARY)
        val CRLF = "\r\n"
        val CRLF_BYTES = EncodingUtils.getAsciiBytes(CRLF)
        val QUOTE = "\""
        val QUOTE_BYTES = EncodingUtils.getAsciiBytes(QUOTE)
        val EXTRA = "--"
        val EXTRA_BYTES = EncodingUtils.getAsciiBytes(EXTRA)
        val CONTENT_DISPOSITION = "Content-Disposition: form-data; name="
        val CONTENT_DISPOSITION_BYTES = EncodingUtils.getAsciiBytes(CONTENT_DISPOSITION)
        val CONTENT_TYPE = "Content-Type: "
        val CONTENT_TYPE_BYTES = EncodingUtils.getAsciiBytes(CONTENT_TYPE)
        val CHARSET = "; charset="
        val CHARSET_BYTES = EncodingUtils.getAsciiBytes(CHARSET)
        val CONTENT_TRANSFER_ENCODING = "Content-Transfer-Encoding: "
        val CONTENT_TRANSFER_ENCODING_BYTES = EncodingUtils.getAsciiBytes(CONTENT_TRANSFER_ENCODING)
        private val DEFAULT_BOUNDARY_BYTES = BOUNDARY_BYTES

        fun getBoundary(): String = BOUNDARY

        fun sendParts(out: OutputStream?, parts: Array<Part?>?) = sendParts(out, parts, DEFAULT_BOUNDARY_BYTES)

        fun sendParts(out: OutputStream?, parts: Array<Part?>?, partBoundary: ByteArray?) {
            if (parts == null) {
                throw IllegalArgumentException()
            }
            if (partBoundary == null || partBoundary.size == 0) {
                throw IllegalArgumentException()
            }
            for (p in parts) {
                p?.boundaryBytes = partBoundary
                p?.send(out)
            }
            out?.write(EXTRA_BYTES)
            out?.write(partBoundary)
            out?.write(EXTRA_BYTES)
            out?.write(CRLF_BYTES)
        }

        fun getLengthOfParts(parts: Array<Part?>?): Long = getLengthOfParts(parts, DEFAULT_BOUNDARY_BYTES)

        fun getLengthOfParts(parts: Array<Part?>?, partBoundary: ByteArray?): Long {
            if (parts == null) {
                throw IllegalArgumentException()
            }
            var total = 0L
            for (p in parts) {
                p?.boundaryBytes = partBoundary
                val l = p!!.length()
                if (l < 0) {
                    return -1
                }
                total += l
            }
            total += EXTRA_BYTES.size
            total += partBoundary!!.size
            total += EXTRA_BYTES.size
            total += CRLF_BYTES.size
            return total
        }

    }

    private var _boundaryBytes: ByteArray? = null
    var boundaryBytes: ByteArray?
        get() {
            if (_boundaryBytes == null) {
                return DEFAULT_BOUNDARY_BYTES
            } else {
                return _boundaryBytes
            }
        }
        set(value) {
            _boundaryBytes = value
        }

    abstract fun getName(): String?

    abstract fun getContentType(): String?

    abstract fun getCharSet(): String?

    abstract fun getTransferEncoding(): String?

    protected abstract fun sendData(out: OutputStream?)

    protected abstract fun lengthOfData(): Long

    open fun isRepeatable(): Boolean = true

    protected open fun sendStart(out: OutputStream?) {
        out?.write(EXTRA_BYTES)
        out?.write(boundaryBytes)
        out?.write(CRLF_BYTES)
    }

    protected open fun sendDispositionHeader(out: OutputStream?) {
        out?.write(CONTENT_DISPOSITION_BYTES)
        out?.write(QUOTE_BYTES)
        out?.write(EncodingUtils.getAsciiBytes(getName()))
        out?.write(QUOTE_BYTES)
    }

    protected open fun sendContentTypeHeader(out: OutputStream?) {
        val contentType = getContentType()
        if (contentType != null) {
            out?.write(CRLF_BYTES)
            out?.write(CONTENT_TYPE_BYTES)
            out?.write(EncodingUtils.getAsciiBytes(contentType))
            val charSet = getCharSet()
            if (charSet != null) {
                out?.write(CHARSET_BYTES)
                out?.write(EncodingUtils.getAsciiBytes(charSet))
            }
        }
    }

    protected open fun sendTransferEncodingHeader(out: OutputStream?) {
        val transferEncoding = getTransferEncoding()
        if (transferEncoding != null) {
            out?.write(CRLF_BYTES)
            out?.write(CONTENT_TRANSFER_ENCODING_BYTES)
            out?.write(EncodingUtils.getAsciiBytes(transferEncoding))
        }
    }

    protected open fun sendEndOfHeader(out: OutputStream?) {
        out?.write(CRLF_BYTES)
        out?.write(CRLF_BYTES)
    }

    protected open fun sendEnd(out: OutputStream?) = out?.write(CRLF_BYTES)

    open fun send(out: OutputStream?) {
        sendStart(out)
        sendDispositionHeader(out)
        sendContentTypeHeader(out)
        sendTransferEncodingHeader(out)
        sendEndOfHeader(out)
        sendData(out)
        sendEnd(out)
    }

    open fun length(): Long {
        if (lengthOfData() < 0) {
            return -1
        }
        val overhead = ByteArrayOutputStream()
        sendStart(overhead)
        sendDispositionHeader(overhead)
        sendContentTypeHeader(overhead)
        sendTransferEncodingHeader(overhead)
        sendEndOfHeader(overhead)
        sendEnd(overhead)
        return overhead.size() + lengthOfData()
    }

}