package com.hujiang.devart.component.swipe

import android.view.View
import android.widget.BaseAdapter

/**
 * Created by rarnu on 3/31/16.
 */
abstract class SwipeItemManager: SwipeItemManageIntf {

    private var _mode = SwipeLayout.Mode.Single
    val INVALID_POSITION = -1
    protected var _openPosition = INVALID_POSITION
    protected var _openPositions = hashSetOf<Int>()
    protected var _shownLayouts = hashSetOf<SwipeLayout>()
    protected var baseAdapter: BaseAdapter? = null

    abstract fun initialize(target: View?, position: Int)

    abstract fun updateConvertView(target: View?, position: Int)

    abstract fun bindView(target: View?, position: Int)

    constructor(adapter: BaseAdapter?) {
        if (adapter == null) {
            throw IllegalArgumentException("Adapter can not be null")
        }
        if (adapter !is SwipeItemManageIntf) {
            throw IllegalArgumentException("adapter should implement the SwipeAdapterInterface")
        }
        this.baseAdapter = adapter
    }

    override fun getMode(): SwipeLayout.Mode = _mode

    override fun setMode(mode: SwipeLayout.Mode) {
        _mode = mode
        _openPositions.clear()
        _shownLayouts.clear()
        _openPosition = INVALID_POSITION
    }


    fun getSwipeLayoutId(position: Int): Int {
        if (baseAdapter != null) {
            return (baseAdapter as SwipeAdapterInterface).getSwipeLayoutResourceId(position)
        } else {
            return -1
        }
    }

    override fun openItem(position: Int) {
        if (_mode == SwipeLayout.Mode.Multiple) {
            if (!_openPositions.contains(position))
                _openPositions.add(position)
        } else {
            _openPosition = position
        }
        baseAdapter?.notifyDataSetChanged()
    }

    override fun closeItem(position: Int) {
        if (_mode == SwipeLayout.Mode.Multiple) {
            _openPositions.remove(position)
        } else {
            if (_openPosition == position)
                _openPosition = INVALID_POSITION
        }
        baseAdapter?.notifyDataSetChanged()
    }

    override fun closeAllExcept(layout: SwipeLayout?) {
        for (s in _shownLayouts) {
            if (s != layout) {
                s.close()
            }
        }
    }

    override fun closeAllItems() {
        if (_mode == SwipeLayout.Mode.Multiple) {
            _openPositions.clear()
        } else {
            _openPosition = INVALID_POSITION
        }
        for (s in _shownLayouts) {
            s.close()
        }
    }

    override fun removeShownLayouts(layout: SwipeLayout?) {
        _shownLayouts.remove(layout)
    }

    override fun getOpenItems(): MutableList<Int>? {
        if (_mode == SwipeLayout.Mode.Multiple) {
            val ret = arrayListOf<Int>()
            ret.addAll(_openPositions)
            return ret
        } else {
            return arrayListOf(_openPosition)
        }
    }

    override fun getOpenLayouts(): MutableList<SwipeLayout>? {
        val ret = arrayListOf<SwipeLayout>()
        ret.addAll(_shownLayouts)
        return ret
    }

    override fun isOpen(position: Int): Boolean {
        if (_mode == SwipeLayout.Mode.Multiple) {
            return _openPositions.contains(position)
        } else {
            return _openPosition == position
        }
    }

    class ValueBox {
        var onLayoutListener: OnLayoutListener? = null
        var swipeMemory: SwipeMemory? = null
        var position = -1

        constructor(position: Int, memory: SwipeMemory?, listener: OnLayoutListener?) {
            swipeMemory = memory
            onLayoutListener = listener
            this.position = position
        }
    }

    inner class OnLayoutListener: OnLayout {

        private var _position = -1

        constructor(position: Int) {
            _position = position
        }

        fun setPosition(position: Int) {
            _position = position
        }

        override fun onLayout(v: SwipeLayout?) {
            if (isOpen(_position)) {
                v?.open(false, false)
            } else {
                v?.close(false, false)
            }
        }
    }

    inner class SwipeMemory: SimpleSwipeListener {

        private var _position = -1

        constructor(position: Int) {
            _position = position
        }

        override fun onClose(layout: SwipeLayout?) {
            if (_mode == SwipeLayout.Mode.Multiple) {
                _openPositions.remove(_position)
            } else {
                _openPosition = INVALID_POSITION
            }
        }

        override fun onStartOpen(layout: SwipeLayout?) {
            if (_mode == SwipeLayout.Mode.Single) {
                closeAllExcept(layout)
            }
        }

        override fun onOpen(layout: SwipeLayout?) {
            if (_mode == SwipeLayout.Mode.Multiple)
                _openPositions.add(_position)
            else {
                closeAllExcept(layout)
                _openPosition = _position
            }
        }

        fun setPosition(position: Int) {
            _position = position
        }
    }

}