package com.hujiang.devart.component.emulator

import android.os.Handler
import android.os.Looper
import android.os.Message
import java.io.InputStream
import java.io.OutputStream
import java.io.UnsupportedEncodingException
import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.Charset
import java.nio.charset.CharsetEncoder
import java.nio.charset.CodingErrorAction

/**
 * Created by rarnu on 4/10/16.
 */
open class TermSession {

    companion object {
        private val TRANSCRIPT_ROWS = 10000
        private val NEW_INPUT = 1
        private val NEW_OUTPUT = 2
        private val FINISH = 3
    }

    private var _colorScheme = BaseTextRenderer.defaultColorScheme
    private var _notify: UpdateCallback? = null
    private var _dataCallback: ReturnDataCallback? = null
    private var _termOut: OutputStream? = null
    private var _termIn: InputStream? = null
    private var _title: String? = null
    private var _transcriptScreen: TranscriptScreen? = null
    private var _emulator: TerminalEmulator? = null
    private var _defaultUTF8Mode = false
    private var _readerThread: Thread? = null
    private var _byteQueue: ByteQueue? = null
    private var _receiveBuffer: ByteArray? = null
    private var _writerThread: Thread? = null
    private var _writeQueue: ByteQueue? = null
    private var _writerHandler: Handler? = null
    private var _writeCharBuffer: CharBuffer? = null
    private var _writeByteBuffer: ByteBuffer? = null
    private var _UTF8Encoder: CharsetEncoder? = null
    private var _finishCallback: FinishCallback? = null
    private var _titleChangedListener: UpdateCallback? = null
    private var _isRunning = false
    private val _msgHandler = object : Handler() {
        override fun handleMessage(msg: Message?) {
            if (!_isRunning) {
                return
            }
            if (msg!!.what == NEW_INPUT) {
                readFromProcess()
            }
        }
    }

    constructor() {
        _writeCharBuffer = CharBuffer.allocate(2)
        _writeByteBuffer = ByteBuffer.allocate(4)
        _UTF8Encoder = Charset.forName("UTF-8").newEncoder()
        _UTF8Encoder?.onMalformedInput(CodingErrorAction.REPLACE)
        _UTF8Encoder?.onUnmappableCharacter(CodingErrorAction.REPLACE)
        _receiveBuffer = ByteArray(4 * 1024)
        _byteQueue = ByteQueue(4 * 1024)
        _readerThread = Thread() {
            val _buffer = ByteArray(4096)
            run {
                try {
                    while (true) {
                        var read = _termIn!!.read(_buffer)
                        if (read == -1) {
                            return@run
                        }
                        var offset = 0
                        while (read > 0) {
                            val written = _byteQueue!!.write(_buffer, offset, read)
                            offset += written
                            read -= written
                            _msgHandler.sendMessage(_msgHandler.obtainMessage(NEW_INPUT))
                        }
                    }
                } catch (e: Exception) {
                }
            }
        }
        _readerThread?.name = "TermSession input reader"
        _writeQueue = ByteQueue(4096)
        _writerThread = Thread() {
            val _buffer = ByteArray(4096)
            fun writeToOutput() {
                val writeQueue = _writeQueue
                val buffer = _buffer
                val termOut = _termOut
                val bytesAvailable = writeQueue!!.getBytesAvailable()
                val bytesToWrite = Math.min(bytesAvailable, buffer.size)
                if (bytesToWrite == 0) {
                    return
                }
                try {
                    writeQueue.read(buffer, 0, bytesToWrite)
                    termOut?.write(buffer, 0, bytesToWrite)
                    termOut?.flush()
                } catch (e: Exception) {
                }
            }
            run {
                Looper.prepare()
                _writerHandler = object : Handler() {
                    override fun handleMessage(msg: Message?) {
                        if (msg!!.what == NEW_OUTPUT) {
                            writeToOutput()
                        } else if (msg.what == FINISH) {
                            Looper.myLooper().quit()
                        }
                    }
                }
                writeToOutput()
                Looper.loop()
            }
        }
        _writerThread?.name = "TermSession output writer"
    }

    open fun initializeEmulator(columns: Int, rows: Int) {
        _transcriptScreen = TranscriptScreen(columns, TRANSCRIPT_ROWS, rows, _colorScheme)
        _emulator = TerminalEmulator(this, _transcriptScreen, columns, rows, _colorScheme)
        _emulator?.setDefaultUTF8Mode(_defaultUTF8Mode)
        _isRunning = true
        _readerThread?.start()
        _writerThread?.start()
    }

    fun setReturnDataCallback(callback: ReturnDataCallback?) {
        _dataCallback = callback
    }

    fun write(data: ByteArray?, offset: Int, count: Int) {
        var noffset = offset
        var ncount = count
        try {
            while (ncount > 0) {
                val written = _writeQueue!!.write(data, noffset, ncount)
                noffset += written
                ncount -= written
                notifyNewOutput()
            }
        } catch (e: InterruptedException) {
        }
    }

    fun write(data: String) {
        try {
            val bytes = data.toByteArray(charset("UTF-8"))
            write(bytes, 0, bytes.size)
        } catch (e: UnsupportedEncodingException) {
        }
    }

    fun write(codePoint: Int) {
        val charBuf = _writeCharBuffer
        val byteBuf = _writeByteBuffer
        val encoder = _UTF8Encoder
        charBuf?.clear()
        byteBuf?.clear()
        Character.toChars(codePoint, charBuf?.array(), 0)
        encoder?.reset()
        encoder?.encode(charBuf, byteBuf, true)
        encoder?.flush(byteBuf)
        write(byteBuf?.array(), 0, byteBuf!!.position() - 1)
    }

    private fun notifyNewOutput() = _writerHandler?.sendEmptyMessage(NEW_OUTPUT)

    fun getTermOut(): OutputStream? = _termOut

    fun setTermOut(termOut: OutputStream?) {
        _termOut = termOut
    }

    fun getTermIn(): InputStream? = _termIn

    fun setTermIn(termIn: InputStream?) {
        _termIn = termIn
    }

    fun isRunning(): Boolean = _isRunning

    fun getTranscriptScreen(): TranscriptScreen? = _transcriptScreen

    fun getEmulator(): TerminalEmulator? = _emulator

    fun setUpdateCallback(notify: UpdateCallback?) {
        _notify = notify
    }

    protected fun notifyUpdate() = _notify?.onUpdate()

    fun getTitle(): String? = _title

    fun setTitle(title: String?) {
        _title = title
        notifyTitleChanged()
    }

    fun setTitleChangedListener(listener: UpdateCallback?) {
        _titleChangedListener = listener
    }

    protected fun notifyTitleChanged() = _titleChangedListener?.onUpdate()

    open fun updateSize(columns: Int, rows: Int) {
        if (_emulator == null) {
            initializeEmulator(columns, rows)
        } else {
            _emulator?.updateSize(columns, rows)
        }
    }

    fun getTranscriptText(): String? = _transcriptScreen?.getTranscriptText()

    private fun readFromProcess() {
        val bytesAvailable = _byteQueue!!.getBytesAvailable()
        val bytesToRead = Math.min(bytesAvailable, _receiveBuffer!!.size)
        var bytesRead: Int
        try {
            bytesRead = _byteQueue!!.read(_receiveBuffer, 0, bytesToRead)
        } catch (e: InterruptedException) {
            return
        }
        processInput(_receiveBuffer, 0, bytesRead)
        notifyUpdate()
    }

    protected fun processInput(data: ByteArray?, offset: Int, count: Int) {
        _dataCallback?.onReceiveData(processInputString(data, offset, count))
        _emulator?.append(data, offset, count)
    }

    private fun processInputString(data: ByteArray?, offset: Int, count: Int): String {
        var str = ""
        for (i in 0..count - 1) {
            val b = data!![offset + i]
            try {
                str += b.toChar()
            } catch (e: Exception) {
            }
        }
        return str
    }

    protected fun appendToEmulator(data: ByteArray?, offset: Int, count: Int) {
        _emulator?.append(data, offset, count)
    }

    fun setColorScheme(scheme: ColorScheme?) {
        var nscheme = scheme
        if (nscheme == null) {
            nscheme = BaseTextRenderer.defaultColorScheme
        }
        _colorScheme = nscheme
        if (_emulator == null) {
            return
        }
        _emulator?.setColorScheme(scheme)
        _transcriptScreen?.setColorScheme(scheme)
    }

    fun setDefaultUTF8Mode(utf8ByDefault: Boolean) {
        _defaultUTF8Mode = utf8ByDefault
        if (_emulator == null) {
            return
        }
        _emulator?.setDefaultUTF8Mode(utf8ByDefault)
    }

    fun getUTF8Mode(): Boolean {
        if (_emulator == null) {
            return _defaultUTF8Mode
        } else {
            return _emulator!!.getUTF8Mode()
        }
    }

    fun setUTF8ModeUpdateCallback(utf8ModeNotify: UpdateCallback?) {
        _emulator?.setUTF8ModeUpdateCallback(utf8ModeNotify)
    }

    fun reset() {
        _emulator?.reset()
        notifyUpdate()
    }

    fun setFinishCallback(callback: FinishCallback?) {
        _finishCallback = callback
    }

    open fun finish() {
        _isRunning = false
        _transcriptScreen?.finish()
        _writerHandler?.sendEmptyMessage(FINISH)
        try {
            _termIn?.close()
            _termOut?.close()
        } catch (e: Exception) {

        }
        _finishCallback?.onSessionFinish(this)
    }
}