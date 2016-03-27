package com.hujiang.devart.utils.http

import org.apache.http.params.HttpParams
import java.io.FilterOutputStream
import java.io.OutputStream

/**
 * Created by rarnu on 3/25/16.
 */
class ProgressedMultipartEntity: MultipartEntity {

    interface ProgressListener {
        fun onProgressChanged(bytes: Long)
    }

    private var _listener: ProgressListener? = null

    constructor(parts: Array<Part?>?, listener: ProgressListener?): super(parts) {
        _listener = listener
    }

    constructor(parts: Array<Part?>?, params: HttpParams?, listener: ProgressListener?): super(parts, params) {
        _listener = listener
    }

    override fun writeTo(out: OutputStream?) {
        super.writeTo(CountingOutputStream(out, _listener))
    }

    class CountingOutputStream: FilterOutputStream {

        private var _callback: ProgressListener? = null
        private var _transferred = 0L

        constructor(out: OutputStream?, callback: ProgressListener?): super(out) {
            _callback = callback
            _transferred = 0L
        }

        override fun write(b: ByteArray?, off: Int, len: Int) {
            out.write(b, off, len)
            _transferred += len
            if (_callback != null) {
                _callback!!.onProgressChanged(_transferred)
            }
        }

        override fun write(b: Int) {
            out.write(b)
            _transferred++
            if (_callback != null) {
                _callback!!.onProgressChanged(_transferred)
            }
        }
    }
}