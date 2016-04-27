package com.hujiang.devart.server

import java.util.regex.Pattern

/**
 * Created by rarnu on 4/27/16.
 */
class ContentType {

    companion object {
        val ASCII_ENCODING = "US-ASCII"
        val MULTIPART_FORM_DATA_HEADER = "multipart/form-data"
        val CONTENT_REGEX = "[ |\t]*([^/^ ^;^,]+/[^ ^;^,]+)"
        val MIME_PATTERN = Pattern.compile(CONTENT_REGEX, Pattern.CASE_INSENSITIVE)
        val CHARSET_REGEX = "[ |\t]*(charset)[ |\t]*=[ |\t]*['|\"]?([^\"^'^;^,]*)['|\"]?"
        val CHARSET_PATTERN = Pattern.compile(CHARSET_REGEX, Pattern.CASE_INSENSITIVE)
        val BOUNDARY_REGEX = "[ |\t]*(boundary)[ |\t]*=[ |\t]*['|\"]?([^\"^'^;^,]*)['|\"]?"
        val BOUNDARY_PATTERN = Pattern.compile(BOUNDARY_REGEX, Pattern.CASE_INSENSITIVE)
    }

    var _contentTypeHeader: String? = null
    var _contentType: String? = null
    var _encoding: String? = null
    var _boundary: String? = null

    constructor(contentTypeHeader: String?) {
        _contentTypeHeader = contentTypeHeader
        if (contentTypeHeader != null) {
            _contentType = getDetailFromContentHeader(contentTypeHeader, MIME_PATTERN, "", 1)
            _encoding = getDetailFromContentHeader(contentTypeHeader, CHARSET_PATTERN, null, 2)
        } else {
            _contentType = "";
            _encoding = "UTF-8";
        }
        if (MULTIPART_FORM_DATA_HEADER.toLowerCase() == _contentType?.toLowerCase()) {
            _boundary = getDetailFromContentHeader(contentTypeHeader, BOUNDARY_PATTERN, null, 2)
        } else {
            _boundary = null
        }
    }

    fun getDetailFromContentHeader(contentTypeHeader: String?, pattern: Pattern?, defaultValue: String?, group: Int): String? {
        val matcher = pattern?.matcher(contentTypeHeader)
        return if (matcher!!.find()) matcher.group(group) else defaultValue
    }

    fun getContentTypeHeader(): String? = _contentTypeHeader
    fun getContentType(): String? = _contentType
    fun getEncoding(): String? = if (_encoding == null) ASCII_ENCODING else _encoding
    fun getBoundary(): String? = _boundary
    fun isMultipart(): Boolean = MULTIPART_FORM_DATA_HEADER.toLowerCase() == _contentType?.toLowerCase()

    fun tryUTF8(): ContentType? {
        if (_encoding == null) {
            return ContentType("${_contentTypeHeader}; charset=UTF-8")
        }
        return this
    }

}