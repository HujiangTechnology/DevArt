package com.hujiang.devart.utils.http

/**
 * Created by rarnu on 3/25/16.
 */
abstract class PartBase: Part {

    private var _name: String? = null
    override fun getName(): String? {
        return _name
    }
    open fun setName(value: String?) {
        if (value == null) {
            throw IllegalArgumentException()
        }
        _name = value
    }
    private var _contentType: String? = null
    override fun getContentType(): String? {
        return _contentType
    }
    open fun setContentType(contentType: String?) {
        _contentType = contentType
    }
    private var _charSet: String? = null
    override fun getCharSet(): String? {
        return _charSet
    }
    open fun setCharSet(charSet: String?) {
        _charSet = charSet
    }
    private var _transferEncoding: String? = null
    override fun getTransferEncoding(): String? {
        return _transferEncoding
    }
    open fun setTransferEncoding(transferEncoding: String?) {
        _transferEncoding = transferEncoding
    }

    constructor(name: String?, contentType: String?, charSet: String?, transferEncoding: String?) {
        if (name == null) {
            throw IllegalArgumentException()
        }
        _name = name
        _contentType = contentType
        _charSet = charSet
        _transferEncoding = transferEncoding
    }

}