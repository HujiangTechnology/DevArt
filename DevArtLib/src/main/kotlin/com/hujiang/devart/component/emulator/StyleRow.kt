package com.hujiang.devart.component.emulator

/**
 * Created by rarnu on 4/11/16.
 */
class StyleRow {

    private var _style = 0
    private var _columns = 0
    private var _data: ByteArray? = null

    constructor(style: Int, columns: Int) {
        _style = style
        _columns = columns
    }

    fun set(column: Int, style: Int) {
        if (style == _style && _data == null) {
            return
        }
        ensureData()
        setStyle(column, style)
    }

    fun get(column: Int): Int {
        if (_data == null) {
            return _style
        }
        return getStyle(column)
    }

    fun isSolidStyle(): Boolean = _data == null

    fun getSolidStyle(): Int {
        if (_data != null) {
            throw IllegalArgumentException("Not a solid style")
        }
        return _style
    }

    fun copy(start: Int, dst: StyleRow?, offset: Int, len: Int) {
        if (_data == null && dst!!._data == null && start == 0 && offset == 0 && len == _columns) {
            dst._style = _style
            return
        }
        ensureData()
        dst?.ensureData()
        System.arraycopy(_data, 3 * start, dst?._data, 3 * offset, 3 * len)
    }

    fun ensureData() {
        if (_data == null) {
            allocate()
        }
    }

    private fun allocate() {
        _data = ByteArray(3 * _columns)
        for (i in 0.._columns - 1) {
            setStyle(i, _style)
        }
    }

    private fun getStyle(column: Int): Int {
        val index = 3 * column
        val line = _data
        return line!![index].toInt() and 0xff or (line[index + 1].toInt() and 0xff) shl 8 or (line[index + 2].toInt() and 0xff) shl 16
    }

    private fun setStyle(column: Int, value: Int) {
        val index = 3 * column
        val line = _data
        line!![index] = (value and 0xff).toByte()
        line[index + 1] = ((value shr 8) and 0xff).toByte()
        line[index + 2] = ((value shr 16) and 0xff).toByte()
    }


}