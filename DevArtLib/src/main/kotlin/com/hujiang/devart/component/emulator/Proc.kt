package com.hujiang.devart.component.emulator

import java.io.FileDescriptor

/**
 * Created by rarnu on 4/10/16.
 */
object Proc {

    init {
        try {
            System.loadLibrary("term")
        } catch(e: Exception) {

        }
    }

    external fun createSubprocess(cmd: String?, args: Array<String?>?, envVars: Array<String?>?, processId: IntArray?): FileDescriptor?

    external fun setPtyWindowSize(fd: FileDescriptor?, row: Int, col: Int, xpixel: Int, ypixel: Int)

    external fun setPtyUTF8Mode(fd: FileDescriptor?, utf8Mode: Boolean)

    external  fun waitFor(processId: Int): Int

    external fun close(fd: FileDescriptor?)

    external fun hangupProcessGroup(processId: Int)


}