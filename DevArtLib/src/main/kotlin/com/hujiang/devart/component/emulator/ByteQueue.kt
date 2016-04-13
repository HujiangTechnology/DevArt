package com.hujiang.devart.component.emulator

/**
 * Created by rarnu on 4/10/16.
 */
class ByteQueue {

    private var _buffer: ByteArray? = null
    private var _head = 0
    private var _storedBytes = 0

    constructor(size: Int) {
        _buffer = ByteArray(size)
    }

    fun getBytesAvailable(): Int {
        synchronized (this) {
            return _storedBytes
        }
    }

    fun read(buffer: ByteArray?, offset: Int, length: Int): Int {
        if (length + offset > buffer!!.size) {
            throw IllegalArgumentException("length + offset > buffer.length")
        }
        if (length < 0) {
            throw IllegalArgumentException("length < 0")
        }
        if (length == 0) {
            return 0
        }
        var nlength = length
        var noffset = offset
        synchronized (this) {
            while (_storedBytes == 0) {
                (this as Object).wait()
            }
            var totalRead = 0;
            var bufferLength = _buffer!!.size
            val wasFull = bufferLength == _storedBytes
            while (nlength > 0 && _storedBytes > 0) {
                val oneRun = Math.min(bufferLength - _head, _storedBytes)
                val bytesToCopy = Math.min(nlength, oneRun)
                System.arraycopy(_buffer, _head, buffer, noffset, bytesToCopy)
                _head += bytesToCopy
                if (_head >= bufferLength) {
                    _head = 0
                }
                _storedBytes -= bytesToCopy
                nlength -= bytesToCopy
                noffset += bytesToCopy
                totalRead += bytesToCopy
            }
            if (wasFull) {
                (this as Object).notify()
            }
            return totalRead
        }
    }

    fun write(buffer: ByteArray?, offset: Int, length: Int): Int {
        if (length + offset > buffer!!.size) {
            throw IllegalArgumentException("length + offset > buffer.length")
        }
        if (length < 0) {
            throw IllegalArgumentException("length < 0")
        }
        if (length == 0) {
            return 0
        }
        var noffset = offset
        synchronized (this) {
            var bufferLength = _buffer!!.size
            val wasEmpty = _storedBytes == 0
            while (bufferLength == _storedBytes) {
                (this as Object).wait()
            }
            var tail = _head + _storedBytes
            var oneRun: Int
            if (tail >= bufferLength) {
                tail -= bufferLength
                oneRun = _head - tail
            } else {
                oneRun = bufferLength - tail
            }
            val bytesToCopy = Math.min(oneRun, length)
            System.arraycopy(buffer, noffset, _buffer, tail, bytesToCopy)
            noffset += bytesToCopy
            _storedBytes += bytesToCopy
            if (wasEmpty) {
                (this as Object).notify()
            }
            return bytesToCopy
        }
    }

}