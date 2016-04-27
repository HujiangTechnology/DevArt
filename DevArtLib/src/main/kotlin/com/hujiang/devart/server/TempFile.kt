package com.hujiang.devart.server

import java.io.OutputStream

/**
 * Created by rarnu on 4/27/16.
 */
interface TempFile {
    fun delete()
    fun getName(): String?
    fun open(): OutputStream?
}