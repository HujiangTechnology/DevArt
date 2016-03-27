package com.hujiang.devart.utils.http

import org.apache.http.util.EncodingUtils
import java.io.OutputStream

/**
 * Created by rarnu on 3/25/16.
 */
class StringPart: PartBase {

    companion object {
        val DEFAULT_CONTENT_TYPE = "text/plain"
        val DEFAULT_CHARSET = "US-ASCII"
        val DEFAULT_TRANSFER_ENCODING = "8bit"
    }

    private var _content: ByteArray? = null
    var content: ByteArray? = null
        get() {
            if (content == null) {
                content = EncodingUtils.getBytes(_value, getCharSet())
            }
            return content
        }
    private var _value: String? = null

    constructor(name: String?, value: String?, charset: String?): super(name, DEFAULT_CONTENT_TYPE, charset ?: DEFAULT_CHARSET, DEFAULT_TRANSFER_ENCODING) {
        if (value == null) {
            throw IllegalArgumentException()
        }
        _value = value
    }

    constructor(name: String?, value: String?): this(name, value, null)

    override fun sendData(out: OutputStream?) {
        out?.write(content)
    }

    override fun lengthOfData(): Long = content!!.size.toLong()

    override fun setCharSet(charSet: String?) {
        super.setCharSet(charSet)
        _content = null
    }

}