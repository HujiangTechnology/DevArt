package com.hujiang.devart.component.draglist

import android.content.Context
import android.database.Cursor
import android.util.SparseIntArray
import android.view.View
import android.view.ViewGroup
import android.widget.CursorAdapter

/**
 * Created by rarnu on 4/1/16.
 */
abstract class BaseDragCursorAdapter: CursorAdapter, CombinedDragListener {

    companion object {
        val REMOVED = -1
    }

    private var _listMapping = SparseIntArray()
    private var _removedCursorPositions = arrayListOf<Int>()

    constructor(context: Context, c: Cursor?): super(context, c, 0)

    constructor(context: Context, c: Cursor?, autoRequery: Boolean): super(context, c, autoRequery)

    constructor(context: Context, c: Cursor?, flags: Int): super(context, c, flags)

    override fun swapCursor(newCursor: Cursor?): Cursor? {
        val old = super.swapCursor(newCursor)
        resetMappings()
        return old
    }

    override fun changeCursor(cursor: Cursor?) {
        super.changeCursor(cursor)
        resetMappings()
    }

    fun reset() {
        resetMappings()
        notifyDataSetChanged()
    }

    private fun resetMappings() {
        _listMapping.clear()
        _removedCursorPositions.clear()
    }

    override fun getItem(position: Int): Any? = super.getItem(_listMapping.get(position, position))

    override fun getItemId(position: Int): Long = super.getItemId(_listMapping.get(position, position))

    override fun getDropDownView(position: Int, convertView: View?, parent: ViewGroup?): View? =
            super.getDropDownView(_listMapping.get(position, position), convertView, parent)

    override fun getView(position: Int, convertView: View?, parent: ViewGroup?): View? =
            super.getView(_listMapping.get(position, position), convertView, parent)

    override fun drop(from: Int, to: Int) {
        if (from != to) {
            val cursorFrom = _listMapping.get(from, from)
            if (from > to) {
                for (i in from downTo to + 1) {
                    _listMapping.put(i, _listMapping.get(i - 1, i - 1))
                }
            } else {
                for (i in from..to - 1) {
                    _listMapping.put(i, _listMapping.get(i + 1, i + 1))
                }
            }
            _listMapping.put(to, cursorFrom)
            cleanMapping()
            notifyDataSetChanged()
        }
    }

    override fun remove(which: Int) {
        val cursorPos = _listMapping.get(which, which)
        if (!_removedCursorPositions.contains(cursorPos)) {
            _removedCursorPositions.add(cursorPos)
        }
        val newCount = count
        for (i in which..newCount - 1) {
            _listMapping.put(i, _listMapping.get(i + 1, i + 1))
        }
        _listMapping.delete(newCount)
        cleanMapping()
        notifyDataSetChanged()
    }

    override fun drag(from: Int, to: Int) { }

    private fun cleanMapping() {
        val toRemove = arrayListOf<Int>()
        val size = _listMapping.size()
        for (i in 0..size-1) {
            if (_listMapping.keyAt(i) == _listMapping.valueAt(i)) {
                toRemove.add(_listMapping.keyAt(i))
            }
        }
        for (ri in toRemove) {
            _listMapping.delete(ri)
        }
    }

    override fun getCount(): Int = super.getCount() - _removedCursorPositions.size

    fun getCursorPosition(position: Int): Int = _listMapping.get(position, position)

    fun getCursorPositions(): MutableList<Int> {
        val result = arrayListOf<Int>()
        for (i in 0..count -1) {
            result.add(_listMapping.get(i, i))
        }
        return result
    }

    fun getListPosition(cursorPosition: Int): Int {
        if (_removedCursorPositions.contains(cursorPosition)) {
            return REMOVED
        }
        val index = _listMapping.indexOfValue(cursorPosition)
        if (index < 0) {
            return cursorPosition
        } else {
            return _listMapping.keyAt(index)
        }
    }



}
