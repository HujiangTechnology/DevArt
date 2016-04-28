package com.hujiang.devart.utils.http

import org.apache.http.util.EncodingUtils
import java.io.File
import java.io.OutputStream

/**
 * Created by rarnu on 3/25/16.
 */
class FilePart: PartBase {

    companion object {
        val DEFAULT_CONTENT_TYPE = "application/octet-stream"
        val DEFAULT_CHARSET = "ISO-8859-1"
        val DEFAULT_TRANSFER_ENCODING = "binary"
        protected val FILE_NAME = "; filename="
        private val FILE_NAME_BYTES = EncodingUtils.getAsciiBytes(FILE_NAME)
    }

    private var _source: PartSource? = null
    var source: PartSource? = null
        get() = _source

    constructor(name: String?, partSource: PartSource?, contentType: String?, charset: String?): super(name, contentType ?: DEFAULT_CONTENT_TYPE, charset ?: "ISO-8859-1", DEFAULT_TRANSFER_ENCODING) {
        if (partSource == null) {
            throw IllegalArgumentException()
        }
        _source = partSource
    }

    constructor(name: String?, partSource: PartSource?): this(name, partSource, null, null)

    constructor(name: String?, file: File?): this(name, FilePartSource(file), null, null)

    constructor(name: String?, file: File?, contentType: String?, charset: String?): this(name, FilePartSource(file), contentType, charset)

    constructor(name: String?, fileName: String?, file: File?): this(name, FilePartSource(fileName, file), null, null)

    constructor(name: String?, fileName: String?, file: File?, contentType: String?, charset: String?): this(name, FilePartSource(fileName, file), contentType, charset)

    override fun sendDispositionHeader(out: OutputStream?) {
        super.sendDispositionHeader(out)
        val filename = _source?.getFileName()
        if (filename != null) {
            out?.write(FILE_NAME_BYTES)
            out?.write(QUOTE_BYTES)
            out?.write(EncodingUtils.getAsciiBytes(filename))
            out?.write(QUOTE_BYTES)
        }
    }

    override fun sendData(out: OutputStream?) {
        if (lengthOfData() == 0L) {
            return
        }

        val tmp = ByteArray(4096)
        val instream = _source?.createInputStream()
        try {
            var len: Int
            while (true) {
                len = instream!!.read(tmp)
                if (len >= 0) {
                    out?.write(tmp, 0, len)
                } else {
                    break
                }
            }
        } finally {
            instream?.close()
        }
    }

    override fun lengthOfData(): Long {
        return _source!!.getLength()
    }

}