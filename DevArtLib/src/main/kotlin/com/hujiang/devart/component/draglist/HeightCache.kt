package com.hujiang.devart.component.draglist

import android.util.SparseIntArray
import java.util.*

/**
 * Created by rarnu on 4/1/16.
 */
class HeightCache {

    private var _map: SparseIntArray? = null
    private var _order: MutableList<Int>? = null
    private var _maxSize = 0

    constructor(size: Int) {
        _map = SparseIntArray(size)
        _order = ArrayList<Int>(size)
        _maxSize = size
    }

    fun add(position: Int, height: Int) {
        val currHeight = _map!!.get(position, -1)
        if (currHeight != height) {
            if (currHeight == -1) {
                if (_map!!.size() == _maxSize) {
                    _map!!.delete(_order!!.removeAt(0))
                }
            } else {
                _order!!.remove(position)
            }
            _map!!.put(position, height)
            _order!!.add(position)
        }
    }

    fun get(position: Int): Int = _map!!.get(position, -1)

    fun clear() {
        _map?.clear()
        _order?.clear()
    }

}