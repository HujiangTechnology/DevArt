package com.hujiang.devart.utils.http

import java.io.InputStream

/**
 * Created by rarnu on 3/25/16.
 */
interface PartSource {

    fun getLength(): Long

    fun getFileName(): String?

    fun createInputStream(): InputStream?

}