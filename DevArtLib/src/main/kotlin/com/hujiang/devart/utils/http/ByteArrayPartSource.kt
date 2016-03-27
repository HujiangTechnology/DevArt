package com.hujiang.devart.utils.http

import java.io.ByteArrayInputStream
import java.io.InputStream

/**
 * Created by rarnu on 3/25/16.
 */
class ByteArrayPartSource: PartSource {

    private var _fileName: String? = null
    private var _bytes: ByteArray? = null

    constructor(fileName: String?, bytes: ByteArray?) {
        _fileName = fileName
        _bytes = bytes
    }

    override fun getLength(): Long = _bytes!!.size.toLong()

    override fun getFileName(): String? = _fileName

    override fun createInputStream(): InputStream? = ByteArrayInputStream(_bytes)
}