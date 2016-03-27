package com.hujiang.devart.utils.http

import org.apache.http.Header
import org.apache.http.entity.AbstractHttpEntity
import org.apache.http.message.BasicHeader
import org.apache.http.params.HttpParams
import org.apache.http.protocol.HTTP
import org.apache.http.util.EncodingUtils
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.util.*

/**
 * Created by rarnu on 3/25/16.
 */
open class MultipartEntity : AbstractHttpEntity {


    companion object {
        val MULTIPART_BOUNDARY = "http.method.multipart.boundary"
        private val MULTIPART_FORM_CONTENT_TYPE = "multipart/form-data"
        private var MULTIPART_CHARS = EncodingUtils.getAsciiBytes("-_1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

        private fun generateMultipartBoundary(): ByteArray? {
            val rand = Random()
            val bytes = ByteArray(rand.nextInt(11) + 30)
            for (i in 0..bytes.size - 1) {
                bytes[i] = MULTIPART_CHARS[rand.nextInt(MULTIPART_CHARS.size)]
            }
            return bytes
        }
    }

    protected var parts: Array<Part?>? = null
    private var _multipartBoundary: ByteArray? = null
    private var _params: HttpParams? = null
    private var _contentConsumed = false

    constructor(parts: Array<Part?>?, params: HttpParams?) {
        if (parts == null) {
            throw IllegalArgumentException()
        }
        if (params == null) {
            throw IllegalArgumentException()
        }
        this.parts = parts
        _params = params
    }

    constructor(parts: Array<Part?>?) {
        setContentType(MULTIPART_FORM_CONTENT_TYPE)
        if (parts == null) {
            throw IllegalArgumentException("parts cannot be null")
        }
        this.parts = parts
        _params = null
    }


    protected fun getMultipartBoundary(): ByteArray? {
        if (_multipartBoundary == null) {
            var temp = _params?.getParameter(MULTIPART_BOUNDARY) as String?
            if (temp != null) {
                _multipartBoundary = EncodingUtils.getAsciiBytes(temp)
            } else {
                _multipartBoundary = generateMultipartBoundary()
            }
        }
        return _multipartBoundary
    }

    override fun isRepeatable(): Boolean {
        for (p in parts!!) {
            if (!p!!.isRepeatable()) {
                return false
            }
        }
        return true
    }

    override fun getContentLength(): Long = try {
        Part.getLengthOfParts(parts, getMultipartBoundary())
    } catch (e: Exception) {
        0
    }


    override fun getContent(): InputStream? {
        if (!isRepeatable && _contentConsumed) {
            throw IllegalStateException()
        }
        _contentConsumed = true
        val baos = ByteArrayOutputStream()
        Part.sendParts(baos, parts, _multipartBoundary)
        val bais = ByteArrayInputStream(baos.toByteArray())
        return bais
    }

    override fun isStreaming(): Boolean = false

    override fun writeTo(out: OutputStream?) = Part.sendParts(out, parts, getMultipartBoundary())

    override fun getContentType(): Header? {
        val buffer = StringBuffer(MULTIPART_FORM_CONTENT_TYPE)
        buffer.append("; boundary=")
        buffer.append(EncodingUtils.getAsciiString(getMultipartBoundary()))
        return BasicHeader(HTTP.CONTENT_TYPE, buffer.toString())
    }


}