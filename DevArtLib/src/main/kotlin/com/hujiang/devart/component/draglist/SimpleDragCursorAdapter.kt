package com.hujiang.devart.component.draglist

import android.content.Context
import android.database.Cursor
import android.net.Uri
import android.view.View
import android.widget.ImageView
import android.widget.SimpleCursorAdapter
import android.widget.TextView

/**
 * Created by rarnu on 4/1/16.
 */
class SimpleDragCursorAdapter: ResourceDragCursorAdapter {

    protected var from: IntArray? = null
    protected var to: IntArray? = null
    private var _originalFrom: Array<String>? = null
    private var _stringConversionColumn = -1
    private var _cursorToStringConverter: SimpleCursorAdapter.CursorToStringConverter? = null
    private var _viewBinder: SimpleCursorAdapter.ViewBinder? = null

    constructor(context: Context, layout: Int, c: Cursor?, from: Array<String>?, to: IntArray?, flags: Int): super(context, layout, c, flags) {
        this.to = to
        _originalFrom = from
        findColumns(c, from)
    }

    override fun bindView(view: View?, context: Context?, cursor: Cursor?) {
        val binder = _viewBinder
        val count = to!!.size
        val from = this.from
        val to = this.to
        for (i in 0..count - 1) {
            val v = view?.findViewById(to!![i])
            if (v != null) {
                var bound = false
                if (binder != null) {
                    bound = binder.setViewValue(v, cursor, from!![i])
                }
                if (!bound) {
                    var text = cursor?.getString(from!![i])
                    if (text == null) {
                        text = ""
                    }
                    if (v is TextView) {
                        setViewText(v, text)
                    } else if (v is ImageView) {
                        setViewImage(v, text)
                    } else {
                        throw IllegalStateException("${v.javaClass.name} is not a view that can be bounds by this SimpleCursorAdapter")
                    }
                }
            }
        }
    }

    fun getViewBinder(): SimpleCursorAdapter.ViewBinder? = _viewBinder

    fun setViewBinder(viewBinder: SimpleCursorAdapter.ViewBinder?) {
        _viewBinder = viewBinder
    }

    fun setViewImage(v: ImageView?, value: String?) {
        try {
            v?.setImageResource(value!!.toInt())
        } catch (e: NumberFormatException) {
            v?.setImageURI(Uri.parse(value))
        }
    }

    fun setViewText(v: TextView?, text: String?) {
        v?.text = text
    }

    private fun findColumns(c: Cursor?, from: Array<String>?) {
        if (c != null) {
            if (this.from == null) {
                this.from = IntArray(count)
            }
            val count = from!!.size
            if (this.from!!.size != count && this.from!!.size != 0) {
                this.from = IntArray(count)
            }
            for (i in 0..count - 1) {
                this.from!![i] = c.getColumnIndexOrThrow(from[i])
            }
        } else {
            this.from = null
        }
    }

    override fun convertToString(cursor: Cursor?): CharSequence? {
        if (_cursorToStringConverter != null) {
            return _cursorToStringConverter?.convertToString(cursor)
        } else if (_stringConversionColumn > -1) {
            return cursor?.getString(_stringConversionColumn)
        }
        return super.convertToString(cursor)
    }


    override fun swapCursor(newCursor: Cursor?): Cursor? {
        findColumns(newCursor, _originalFrom)
        return super.swapCursor(newCursor)
    }

    fun getStringConversionColumn(): Int = _stringConversionColumn

    fun setStringConversionColumn(stringConversionColumn: Int) {
        _stringConversionColumn = stringConversionColumn
    }

    fun getCursorToStringConverter(): SimpleCursorAdapter.CursorToStringConverter? = _cursorToStringConverter

    fun setCursorToStringConverter(cursorToStringConverter: SimpleCursorAdapter.CursorToStringConverter?) {
        _cursorToStringConverter = cursorToStringConverter
    }

    fun changeCursorAndColumns(c: Cursor?, from: Array<String>?, to: IntArray?) {
        _originalFrom = from
        this.to = to
        findColumns(c, _originalFrom)
        super.changeCursor(c)
    }
}