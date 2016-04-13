package com.hujiang.devart.component.emulator

/**
 * Created by rarnu on 4/10/16.
 */
class GrowableIntArray {

    private var _data: IntArray? = null
    private var _length = 0

    constructor(initalCapacity: Int) {
        _data = IntArray(initalCapacity)
        _length = 0
    }

    fun append(i: Int) {
        if (_length + 1 > _data!!.size) {
            val newLength = Math.max((_data!!.size * 3) shr 1, 16)
            val temp = IntArray(newLength)
            System.arraycopy(_data, 0, temp, 0, _length)
            _data = temp
        }
        _data!![_length++] = i
    }

    fun length(): Int = _length

    fun at(index: Int): Int = _data!![index]

}