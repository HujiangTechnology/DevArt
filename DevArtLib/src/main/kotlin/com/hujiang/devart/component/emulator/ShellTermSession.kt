package com.hujiang.devart.component.emulator

import android.os.Handler
import android.os.Message
import java.io.*
import java.nio.charset.Charset

/**
 * Created by rarnu on 4/11/16.
 */
class ShellTermSession: TermSession {

    companion object {
        val PROCESS_EXIT_FINISHES_SESSION = 0
        val PROCESS_EXIT_DISPLAYS_MESSAGE = 1
        private val VTTEST_MODE = false
        private val PROCESS_EXITED = 1
    }

    private var _procId = 0
    private var _termFd: FileDescriptor? = null
    private var _watcherThread: Thread? = null
    private var _handle: String? = null
    private var _initialCommand: String? = null
    private var _processExitMessage: String? = null
    private var _msgHandler = object : Handler() {
        override fun handleMessage(msg: Message?) {
            if (!isRunning()) {
                return
            }
            if (msg!!.what == PROCESS_EXITED) {
                onProcessExit(msg.obj as Int)
            }
        }
    }
    private var _UTF8ModeNotify = object : UpdateCallback {
        override fun onUpdate() {
            Proc.setPtyUTF8Mode(_termFd, getUTF8Mode())
        }
    }

    constructor(initialCommand: String?): super() {
        initializeSession()
        _initialCommand = initialCommand
        _watcherThread = Thread() {
            run {
                val result = Proc.waitFor(_procId)
                _msgHandler.sendMessage(_msgHandler.obtainMessage(PROCESS_EXITED, result))
            }
        }
        _watcherThread?.name = "Process watcher"
    }


    private fun initializeSession() {
        val processId = IntArray(1)
        val env = arrayOfNulls<String>(2)
        env[0] = "TERM=screen"
        env[1] = "PATH=" + System.getenv("PATH")
        createSubprocess(processId, "/system/bin/sh -", env)
        _procId = processId[0]
        setTermOut(FileOutputStream(_termFd))
        setTermIn(FileInputStream(_termFd))
    }

    private fun createSubprocess(processId: IntArray?, shell: String?, env: Array<String?>?) {
        var argList = parse(shell)
        var arg0: String
        var args: Array<String?>
        try {
            arg0 = argList!![0]!!
            val file = File(arg0)
            if (!file.exists()) {
                throw FileNotFoundException(arg0)
            } else if (!file.canExecute()) {
                throw FileNotFoundException(arg0)
            }

            args = argList.toTypedArray()
        } catch (e: Exception) {
            argList = parse("/system/bin/sh -")
            arg0 = argList!![0]!!
            args = argList.toTypedArray()
        }

        _termFd = Proc.createSubprocess(arg0, args, env, processId)
    }

    private fun parse(cmd: String?): MutableList<String?>? {
        val PLAIN = 0
        val WHITESPACE = 1
        val INQUOTE = 2
        var state = WHITESPACE
        val result = arrayListOf<String?>()
        val cmdLen = cmd!!.length
        val builder = StringBuilder()
        var i = 0
        while (i < cmdLen) {
            val c = cmd[i]
            if (state == PLAIN) {
                if (Character.isWhitespace(c)) {
                    result.add(builder.toString())
                    builder.delete(0, builder.length)
                    state = WHITESPACE
                } else if (c == '"') {
                    state = INQUOTE
                } else {
                    builder.append(c)
                }
            } else if (state == WHITESPACE) {
                if (Character.isWhitespace(c)) {

                } else if (c == '"') {
                    state = INQUOTE
                } else {
                    state = PLAIN
                    builder.append(c)
                }
            } else if (state == INQUOTE) {
                if (c == '\\') {
                    if (i + 1 < cmdLen) {
                        i += 1
                        builder.append(cmd[i])
                    }
                } else if (c == '"') {
                    state = PLAIN
                } else {
                    builder.append(c)
                }
            }
            ++i
        }
        if (builder.length > 0) {
            result.add(builder.toString())
        }
        return result
    }

    override fun initializeEmulator(columns: Int, rows: Int) {
        var ncolumns = columns
        var nrows = rows
        if (VTTEST_MODE) {
            ncolumns = 80
            nrows = 24
        }
        super.initializeEmulator(ncolumns, nrows)
        Proc.setPtyUTF8Mode(_termFd, getUTF8Mode())
        setUTF8ModeUpdateCallback(_UTF8ModeNotify)
        _watcherThread?.start()
        sendInitialCommand(_initialCommand)
    }

    private fun sendInitialCommand(initialCommand: String?) {
        if (initialCommand!!.length > 0) {
            write(initialCommand + '\r')
        }
    }

    private fun onProcessExit(@Suppress("UNUSED_PARAMETER") result: Int) {
        if (_processExitMessage != null) {
            try {
                val msg = ("\r\n[$_processExitMessage]").toByteArray(Charset.forName("UTF-8"))
                appendToEmulator(msg, 0, msg.size)
                notifyUpdate()
            } catch (e: UnsupportedEncodingException) {

            }
        }
    }

    override fun updateSize(columns: Int, rows: Int) {
        var ncolumns = columns
        var nrows = rows
        if (VTTEST_MODE) {
            ncolumns = 80
            nrows = 24
        }
        Proc.setPtyWindowSize(_termFd, nrows, ncolumns, 0, 0)
        super.updateSize(ncolumns, nrows)
    }

    fun setProcessExitMessage(message: String?) {
        _processExitMessage = message
    }

    override fun finish() {
        Proc.hangupProcessGroup(_procId)
        Proc.close(_termFd)
        super.finish()
    }

    fun getHandle(): String? = _handle

    fun setHandle(handle: String?) {
        if (_handle != null) {
            throw IllegalStateException("Cannot change handle once set")
        }
        _handle = handle
    }

    fun getTitle(defaultTitle: String?): String? {
        var title = super.getTitle()
        if (title != null && title.length > 0) {
            return title
        } else {
            return defaultTitle
        }
    }
}