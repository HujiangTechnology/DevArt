package com.hujiang.devart.component.recycler

import android.view.View
import android.view.ViewGroup

/**
 * Created by rarnu on 4/20/16.
 */
class ChildHelper {

    private var _callback: Callback? = null
    private var _bucket: Bucket? = null
    private var _hiddenViews: MutableList<View?>? = null

    constructor(callback: Callback?) {
        _callback = callback
        _bucket = Bucket()
        _hiddenViews = arrayListOf()

    }

    private fun hideViewInternal(child: View?) {
        _hiddenViews?.add(child)
        _callback?.onEnteredHiddenState(child)
    }

    private fun unhideViewInternal(child: View?): Boolean {
        if (_hiddenViews!!.remove(child)) {
            _callback?.onLeftHiddenState(child)
            return true
        } else {
            return false
        }
    }

    fun addView(child: View?, hidden: Boolean) = addView(child, -1, hidden)

    fun addView(child: View?, index: Int, hidden: Boolean) {
        var offset: Int
        if (index < 0) {
            offset = _callback!!.getChildCount()
        } else {
            offset = getOffset(index)
        }
        _bucket?.insert(offset, hidden)
        if (hidden) {
            hideViewInternal(child)
        }
        _callback?.addView(child, offset)
    }

    private fun getOffset(index: Int): Int {
        if (index < 0) {
            return -1
        }
        val limit = _callback!!.getChildCount()
        var offset = index
        while (offset < limit) {
            val removedBefore = _bucket!!.countOnesBefore(offset)
            val diff = index - (offset - removedBefore)
            if (diff == 0) {
                while (_bucket!!.get(offset)) {
                    offset ++
                }
                return offset
            } else {
                offset += diff
            }
        }
        return -1
    }

    fun removeView(view: View?) {
        val index = _callback!!.indexOfChild(view)
        if (index < 0) {
            return
        }
        if (_bucket!!.remove(index)) {
            unhideViewInternal(view)
        }
        _callback?.removeViewAt(index)
    }

    fun removeViewAt(index: Int) {
        val offset = getOffset(index)
        val view = _callback?.getChildAt(offset) ?: return
        if (_bucket!!.remove(offset)) {
            unhideViewInternal(view)
        }
        _callback?.removeViewAt(offset)
    }

    fun getChildAt(index: Int): View? {
        val offset = getOffset(index)
        return _callback?.getChildAt(offset)
    }

    fun removeAllViewsUnfiltered() {
        _bucket?.reset()
        for (i in _hiddenViews!!.size - 1 downTo 0) {
            _callback?.onLeftHiddenState(_hiddenViews!![i])
            _hiddenViews?.removeAt(i)
        }
        _callback?.removeAllViews()
    }

    fun findHiddenNonRemovedView(position: Int, type: Int): View? {
        val count = _hiddenViews!!.size
        for (i in 0..count - 1) {
            val view = _hiddenViews?.get(i)
            val holder = _callback?.getChildViewHolder(view)
            if (holder!!.getLayoutPosition() == position && !holder.isInvalid() && (type == RecyclerView.INVALID_TYPE || holder.getItemViewType() == type)) {
                return view
            }
        }
        return null
    }

    fun attachViewToParent(child: View?, index: Int, layoutParams: ViewGroup.LayoutParams?, hidden: Boolean) {
        var offset: Int
        if (index < 0) {
            offset = _callback!!.getChildCount()
        } else {
            offset = getOffset(index)
        }
        _bucket?.insert(offset, hidden)
        if (hidden) {
            hideViewInternal(child)
        }
        _callback?.attachViewToParent(child, offset, layoutParams)
    }

    fun getChildCount(): Int = _callback!!.getChildCount() - _hiddenViews!!.size

    fun getUnfilteredChildCount(): Int = _callback!!.getChildCount()

    fun getUnfilteredChildAt(index: Int): View? = _callback?.getChildAt(index)

    fun detachViewFromParent(index: Int) {
        val offset = getOffset(index)
        _bucket?.remove(offset)
        _callback?.detachViewFromParent(offset)
    }

    fun indexOfChild(child: View?): Int {
        val index = _callback!!.indexOfChild(child)
        if (index == -1) {
            return -1
        }
        if (_bucket!!.get(index)) {
            return -1
        }
        return index - _bucket!!.countOnesBefore(index)
    }

    fun isHidden(view: View?): Boolean = _hiddenViews!!.contains(view)

    fun hide(view: View?) {
        var offset = _callback!!.indexOfChild(view)
        if (offset < 0) {
            throw IllegalArgumentException("view is not a child, cannot hide " + view)
        }
        _bucket?.set(offset)
        hideViewInternal(view)
    }

    override fun toString(): String = _bucket.toString() + ", hidden list:" + _hiddenViews!!.size

    fun removeViewIfHidden(view: View?): Boolean {
        val index = _callback!!.indexOfChild(view)
        if (index == -1) {
            unhideViewInternal(view)
            return true
        }
        if (_bucket!!.get(index)) {
            _bucket?.remove(index)
            unhideViewInternal(view)
            _callback?.removeViewAt(index)
            return true
        }
        return false
    }

    class Bucket {

        companion object {
            val BITS_PER_WORD = 64 /* Long.SIZE */
            val LAST_BIT = 1L shl (64 /* Long.SIZE */ - 1)
        }
        private var _data = 0L
        private var _next: Bucket? = null

        fun set(index: Int) {
            if (index >= BITS_PER_WORD) {
                ensureNext()
                _next?.set(index - BITS_PER_WORD)
            } else {
                _data = _data or (1L shl index)
            }
        }

        private fun ensureNext() {
            if (_next == null) {
                _next = Bucket()
            }
        }

        fun clear(index: Int) {
            if (index >= BITS_PER_WORD) {
                _next?.clear(index - BITS_PER_WORD)
            } else {
                _data = _data and (1L shl index).inv()
            }
        }

        fun get(index: Int): Boolean {
            if (index >= BITS_PER_WORD) {
                ensureNext()
                return _next!!.get(index - BITS_PER_WORD)
            } else {
                return (_data and (1L shl index)) != 0L
            }
        }

        fun reset() {
            _data = 0
            _next?.reset()
        }

        fun insert(index: Int, value: Boolean) {
            if (index >= BITS_PER_WORD) {
                ensureNext()
                _next?.insert(index - BITS_PER_WORD, value)
            } else {
                val lastBit = (_data and LAST_BIT) != 0L
                val mask = (1L shl index) - 1
                val before = _data and mask
                val after = ((_data and mask.inv())) shl 1
                _data = before or after
                if (value) {
                    set(index)
                } else {
                    clear(index)
                }
                if (lastBit || _next != null) {
                    ensureNext()
                    _next?.insert(0, lastBit)
                }
            }
        }

        fun remove(index: Int): Boolean {
            if (index >= BITS_PER_WORD) {
                ensureNext()
                return _next!!.remove(index - BITS_PER_WORD)
            } else {
                var mask = (1L shl index)
                val value = (_data and mask) != 0L
                _data = _data and mask.inv()
                mask -= 1
                val before = _data and mask
                val after = java.lang.Long.rotateRight(_data and mask.inv(), 1)
                _data = before or after
                if (_next != null) {
                    if (_next!!.get(0)) {
                        set(BITS_PER_WORD - 1)
                    }
                    _next?.remove(0)
                }
                return value
            }
        }

        fun countOnesBefore(index: Int): Int {
            if (_next == null) {
                if (index >= BITS_PER_WORD) {
                    return java.lang.Long.bitCount(_data)
                }
                return java.lang.Long.bitCount(_data and ((1L shl index) - 1))
            }
            if (index < BITS_PER_WORD) {
                return java.lang.Long.bitCount(_data and ((1L shl index) - 1))
            } else {
                return _next!!.countOnesBefore(index - BITS_PER_WORD) + java.lang.Long.bitCount(_data)
            }
        }

        override fun toString(): String = if (_next == null) java.lang.Long.toBinaryString(_data) else _next.toString() + "xx" + java.lang.Long.toBinaryString(_data)
    }

    interface Callback {
        fun getChildCount(): Int
        fun addView(child: View?, index: Int)
        fun indexOfChild(view: View?): Int
        fun removeViewAt(index: Int)
        fun getChildAt(offset: Int): View?
        fun removeAllViews()
        fun getChildViewHolder(view: View?): RecyclerView.ViewHolder?
        fun attachViewToParent(child: View?, index: Int, layoutParams: ViewGroup.LayoutParams?)
        fun detachViewFromParent(offset: Int)
        fun onEnteredHiddenState(child: View?)
        fun onLeftHiddenState(child: View?)
    }

}