package com.hujiang.devart.component.recycler

import android.support.v4.util.Pools
import java.util.*

/**
 * Created by rarnu on 4/21/16.
 */
class AdapterHelper: OpReorderer.Callback {

    companion object {
        val POSITION_TYPE_INVISIBLE = 0
        val POSITION_TYPE_NEW_OR_LAID_OUT = 1
    }

    var _updateOpPool = Pools.SimplePool<UpdateOp?>(UpdateOp.POOL_SIZE)
    var _pendingUpdates = arrayListOf<UpdateOp?>()
    var _postponedList = arrayListOf<UpdateOp?>()
    var _callback: Callback? = null
    var _onItemProcessedCallback: Runnable? = null
    var _disableRecycler = false
    var _opReorderer: OpReorderer? = null

    constructor(callback: Callback?): this(callback, false)
    constructor(callback: Callback?, disableRecycler: Boolean) {
        _callback = callback
        _disableRecycler = disableRecycler
        _opReorderer = OpReorderer(this)
    }

    fun addUpdateOp(vararg ops: UpdateOp?): AdapterHelper? {
        for (o in ops) {
            Collections.addAll(_pendingUpdates, o)
        }
        return this
    }

    fun reset() {
        recycleUpdateOpsAndClearList(_pendingUpdates)
        recycleUpdateOpsAndClearList(_postponedList)
    }

    fun preProcess() {
        _opReorderer?.reorderOps(_pendingUpdates)
        val count = _pendingUpdates.size
        for (i in 0..count - 1) {
            val op = _pendingUpdates[i]
            when (op!!._cmd) {
                UpdateOp.ADD -> applyAdd(op)
                UpdateOp.REMOVE -> applyRemove(op)
                UpdateOp.UPDATE -> applyUpdate(op)
                UpdateOp.MOVE -> applyMove(op)
            }
            _onItemProcessedCallback?.run()
        }
        _pendingUpdates.clear()
    }

    fun canFindInPreLayout(position: Int): Boolean {
        val count = _postponedList.size
        for (i in 0..count - 1) {
            val op = _postponedList[i]
            if (op!!._cmd == UpdateOp.MOVE) {
                if (findPositionOffset(op._itemCount, i + 1) == position) {
                    return true
                }
            } else if (op._cmd == UpdateOp.ADD) {
                val end = op._positionStart + op._itemCount
                for (pos in op._positionStart..end - 1) {
                    if (findPositionOffset(pos, i + 1) == position) {
                        return true
                    }
                }
            }
        }
        return false
    }

    fun findPositionOffset(position: Int): Int = findPositionOffset(position, 0)

    fun findPositionOffset(position: Int, firstPostponedItem: Int): Int {
        var nposition = position
        var count = _postponedList.size
        for (i in firstPostponedItem..count - 1) {
            val op = _postponedList[i]
            if (op!!._cmd == UpdateOp.MOVE) {
                if (op._positionStart == nposition) {
                    nposition = op._itemCount
                } else {
                    if (op._positionStart < nposition) {
                        nposition--
                    }
                    if (op._itemCount <= nposition) {
                        nposition++
                    }
                }
            } else if (op._positionStart <= nposition) {
                if (op._cmd == UpdateOp.REMOVE) {
                    if (nposition < op._positionStart + op._itemCount) {
                        return -1
                    }
                    nposition -= op._itemCount
                } else if (op._cmd == UpdateOp.ADD) {
                    nposition += op._itemCount
                }
            }
        }
        return nposition
    }

    private fun applyAdd(op: UpdateOp?) = postponeAndUpdateViewHolders(op)

    private fun applyUpdate(op: UpdateOp?) {
        var nop = op
        var tmpStart = nop!!._positionStart
        var tmpCount = 0
        var tmpEnd = nop._positionStart + nop._itemCount
        var type = -1
        for (position in nop._positionStart..tmpEnd - 1) {
            val vh = _callback?.findViewHolder(position)
            if (vh != null || canFindInPreLayout(position)) {
                if (type == POSITION_TYPE_INVISIBLE) {
                    val newOp = obtainUpdateOp(UpdateOp.UPDATE, tmpStart, tmpCount, nop._payload)
                    dispatchAndUpdateViewHolders(newOp)
                    tmpCount = 0
                    tmpStart = position
                }
                type = POSITION_TYPE_NEW_OR_LAID_OUT
            } else {
                if (type == POSITION_TYPE_NEW_OR_LAID_OUT) {
                    val newOp = obtainUpdateOp(UpdateOp.UPDATE, tmpStart, tmpCount, nop._payload)
                    postponeAndUpdateViewHolders(newOp)
                    tmpCount = 0
                    tmpStart = position
                }
                type = POSITION_TYPE_INVISIBLE
            }
            tmpCount++
        }
        if (tmpCount != nop._itemCount) {
            val payload = nop._payload
            recycleUpdateOp(nop)
            nop = obtainUpdateOp(UpdateOp.UPDATE, tmpStart, tmpCount, payload)
        }
        if (type == POSITION_TYPE_INVISIBLE) {
            dispatchAndUpdateViewHolders(nop)
        } else {
            postponeAndUpdateViewHolders(nop)
        }
    }

    private fun dispatchAndUpdateViewHolders(op: UpdateOp?) {
        if (op!!._cmd == UpdateOp.ADD || op._cmd == UpdateOp.MOVE) {
            throw IllegalArgumentException("should not dispatch add or move for pre layout")
        }
        var tmpStart = updatePositionWithPostponed(op._positionStart, op._cmd)
        var tmpCnt = 1
        var offsetPositionForPartial = op._positionStart
        var positionMultiplier: Int
        when (op._cmd) {
            UpdateOp.UPDATE -> positionMultiplier = 1
            UpdateOp.REMOVE -> positionMultiplier = 0
            else -> throw IllegalArgumentException("op should be remove or update.${op}")
        }
        for (p in 1..op._itemCount - 1) {
            val pos = op._positionStart + (positionMultiplier * p)
            var updatedPos = updatePositionWithPostponed(pos, op._cmd)
            var continuous = false
            when (op._cmd) {
                UpdateOp.UPDATE -> continuous = updatedPos == tmpStart + 1
                UpdateOp.REMOVE -> continuous = updatedPos == tmpStart
            }
            if (continuous) {
                tmpCnt++
            } else {
                val tmp = obtainUpdateOp(op._cmd, tmpStart, tmpCnt, op._payload)
                dispatchFirstPassAndUpdateViewHolders(tmp, offsetPositionForPartial)
                recycleUpdateOp(tmp)
                if (op._cmd == UpdateOp.UPDATE) {
                    offsetPositionForPartial += tmpCnt
                }
                tmpStart = updatedPos
                tmpCnt = 1
            }
        }
        var payload = op._payload
        recycleUpdateOp(op)
        if (tmpCnt > 0) {
            val tmp = obtainUpdateOp(op._cmd, tmpStart, tmpCnt, payload)
            dispatchFirstPassAndUpdateViewHolders(tmp, offsetPositionForPartial)
            recycleUpdateOp(tmp)
        }
    }

    private fun updatePositionWithPostponed(pos: Int, cmd: Int): Int {
        var npos = pos
        val count = _postponedList.size
        for (i in count - 1 downTo 0) {
            val postponed = _postponedList[i]
            if (postponed!!._cmd == UpdateOp.MOVE) {
                var start: Int
                var end: Int
                if (postponed._positionStart < postponed._itemCount) {
                    start = postponed._positionStart
                    end = postponed._itemCount
                } else {
                    start = postponed._itemCount
                    end = postponed._positionStart
                }
                if (npos >= start && npos <= end) {
                    if (start == postponed._positionStart) {
                        if (cmd == UpdateOp.ADD) {
                            postponed._itemCount++
                        } else if (cmd == UpdateOp.REMOVE) {
                            postponed._itemCount--
                        }
                        npos++
                    } else {
                        if (cmd == UpdateOp.ADD) {
                            postponed._positionStart++
                        } else if (cmd == UpdateOp.REMOVE) {
                            postponed._positionStart--
                        }
                        npos--
                    }
                } else if (npos < postponed._positionStart) {
                    if (cmd == UpdateOp.ADD) {
                        postponed._positionStart++
                        postponed._itemCount++
                    } else if (cmd == UpdateOp.REMOVE) {
                        postponed._positionStart--
                        postponed._itemCount--
                    }
                }
            } else {
                if (postponed._positionStart <= npos) {
                    if (postponed._cmd == UpdateOp.ADD) {
                        npos -= postponed._itemCount
                    } else if (postponed._cmd == UpdateOp.REMOVE) {
                        npos += postponed._itemCount
                    }
                } else {
                    if (cmd == UpdateOp.ADD) {
                        postponed._positionStart++
                    } else if (cmd == UpdateOp.REMOVE) {
                        postponed._positionStart--
                    }
                }
            }
        }
        for (i in _postponedList.size - 1 downTo 0) {
            val op = _postponedList[i]
            if (op!!._cmd == UpdateOp.MOVE) {
                if (op._itemCount == op._positionStart || op._itemCount < 0) {
                    _postponedList.removeAt(i)
                    recycleUpdateOp(op)
                }
            } else if (op._itemCount <= 0) {
                _postponedList.removeAt(i)
                recycleUpdateOp(op)
            }
        }
        return npos
    }


    fun dispatchFirstPassAndUpdateViewHolders(op: UpdateOp?, offsetStart: Int) {
        _callback?.onDispatchFirstPass(op)
        when (op!!._cmd) {
            UpdateOp.REMOVE -> _callback?.offsetPositionsForRemovingInvisible(offsetStart, op._itemCount)
            UpdateOp.UPDATE -> _callback?.markViewHoldersUpdated(offsetStart, op._itemCount, op._payload)
            else -> throw IllegalArgumentException("only remove and update ops can be dispatched in first pass")
        }
    }


    private fun applyRemove(op: UpdateOp?) {
        var nop = op
        var tmpStart = nop!!._positionStart
        var tmpCount = 0
        var tmpEnd = nop._positionStart + nop._itemCount
        var type = -1
        var position = nop._positionStart
        while (position < tmpEnd) {
            var typeChanged = false
            val vh = _callback?.findViewHolder(position)
            if (vh != null || canFindInPreLayout(position)) {
                if (type == POSITION_TYPE_INVISIBLE) {
                    val newOp = obtainUpdateOp(UpdateOp.REMOVE, tmpStart, tmpCount, null)
                    dispatchAndUpdateViewHolders(newOp)
                    typeChanged = true
                }
                type = POSITION_TYPE_NEW_OR_LAID_OUT
            } else {
                if (type == POSITION_TYPE_NEW_OR_LAID_OUT) {
                    val newOp = obtainUpdateOp(UpdateOp.REMOVE, tmpStart, tmpCount, null)
                    postponeAndUpdateViewHolders(newOp)
                    typeChanged = true
                }
                type = POSITION_TYPE_INVISIBLE
            }
            if (typeChanged) {
                position -= tmpCount
                tmpEnd -= tmpCount
                tmpCount = 1
            } else {
                tmpCount++
            }
            ++position
        }
        if (tmpCount != nop._itemCount) {
            recycleUpdateOp(op)
            nop = obtainUpdateOp(UpdateOp.REMOVE, tmpStart, tmpCount, null)
        }
        if (type == POSITION_TYPE_INVISIBLE) {
            dispatchAndUpdateViewHolders(nop)
        } else {
            postponeAndUpdateViewHolders(nop)
        }
    }

    fun consumePostponedUpdates() {
        val count = _postponedList.size
        for (i in 0..count - 1) {
            _callback?.onDispatchSecondPass(_postponedList[i])
        }
        recycleUpdateOpsAndClearList(_postponedList)
    }

    private fun applyMove(op: UpdateOp?) = postponeAndUpdateViewHolders(op)

    private fun postponeAndUpdateViewHolders(op: UpdateOp?) {
        _postponedList.add(op)
        when (op!!._cmd) {
            UpdateOp.ADD -> _callback?.offsetPositionsForAdd(op._positionStart, op._itemCount)
            UpdateOp.MOVE -> _callback?.offsetPositionsForMove(op._positionStart, op._itemCount)
            UpdateOp.REMOVE -> _callback?.offsetPositionsForRemovingLaidOutOrNewView(op._positionStart, op._itemCount)
            UpdateOp.UPDATE -> _callback?.markViewHoldersUpdated(op._positionStart, op._itemCount, op._payload)
            else -> throw IllegalArgumentException("Unknown update op type for ${op}")
        }
    }

    fun recycleUpdateOpsAndClearList(ops: MutableList<UpdateOp?>?) {
        val count = ops!!.size
        for (i in 0..count - 1) {
            recycleUpdateOp(ops[i])
        }
        ops.clear()
    }

    fun applyPendingUpdatesToPosition(position: Int): Int {
        var nposition = position
        val size = _pendingUpdates.size
        for (i in 0..size - 1) {
            val op = _pendingUpdates[i]
            when (op!!._cmd) {
                UpdateOp.ADD -> if (op._positionStart <= nposition) { nposition += op._itemCount }
                UpdateOp.REMOVE -> if (op._positionStart <= nposition) {
                    val end = op._positionStart + op._itemCount
                    if (end > nposition) {
                        return RecyclerView.NO_POSITION
                    }
                    nposition -= op._itemCount
                }
                UpdateOp.MOVE -> if (op._positionStart == nposition) {
                    nposition = op._itemCount
                } else {
                    if (op._positionStart < nposition) {
                        nposition -= 1
                    }
                    if (op._itemCount <= nposition) {
                        nposition += 1
                    }
                }
            }
        }
        return nposition
    }

    fun consumeUpdatesInOnePass() {
        consumePostponedUpdates()
        val count = _pendingUpdates.size
        for (i in 0..count - 1) {
            val op = _pendingUpdates[i]
            when (op!!._cmd) {
                UpdateOp.ADD -> {
                    _callback?.onDispatchSecondPass(op)
                    _callback?.offsetPositionsForAdd(op._positionStart, op._itemCount)
                }
                UpdateOp.REMOVE -> {
                    _callback?.onDispatchSecondPass(op)
                    _callback?.offsetPositionsForRemovingInvisible(op._positionStart, op._itemCount)
                }
                UpdateOp.UPDATE -> {
                    _callback?.onDispatchSecondPass(op)
                    _callback?.markViewHoldersUpdated(op._positionStart, op._itemCount, op._payload)
                }
                UpdateOp.MOVE -> {
                    _callback?.onDispatchSecondPass(op)
                    _callback?.offsetPositionsForMove(op._positionStart, op._itemCount)
                }
            }
            _onItemProcessedCallback?.run()
        }
        recycleUpdateOpsAndClearList(_pendingUpdates)
    }

    fun hasPendingUpdates(): Boolean = _pendingUpdates.size > 0

    fun onItemRangeChanged(positionStart: Int, itemCount: Int, payload: Any?): Boolean {
        _pendingUpdates.add(obtainUpdateOp(UpdateOp.UPDATE, positionStart, itemCount, payload))
        return _pendingUpdates.size == 1
    }

    fun onItemRangeInserted(positionStart: Int, itemCount: Int): Boolean {
        _pendingUpdates.add(obtainUpdateOp(UpdateOp.ADD, positionStart, itemCount, null))
        return _pendingUpdates.size == 1
    }

    fun onItemRangeRemoved(positionStart: Int, itemCount: Int): Boolean {
        _pendingUpdates.add(obtainUpdateOp(UpdateOp.REMOVE, positionStart, itemCount, null))
        return _pendingUpdates.size == 1
    }
    fun onItemRangeMoved(from: Int, to: Int, itemCount: Int): Boolean {
        if (from == to) {
            return false
        }
        if (itemCount != 1) {
            throw IllegalArgumentException("Moving more than 1 item is not supported yet")
        }
        _pendingUpdates.add(obtainUpdateOp(UpdateOp.MOVE, from, to, null))
        return _pendingUpdates.size == 1
    }


    class UpdateOp {

        companion object {
            val ADD = 0
            val REMOVE = 1
            val UPDATE = 2
            val MOVE = 3
            val POOL_SIZE = 30
        }
        var _cmd = 0
        var _positionStart = 0
        var _payload: Any? = null
        var _itemCount = 0

        constructor(cmd: Int, positionStart: Int, itemCount: Int, payload: Any?) {
            _cmd = cmd
            _positionStart = positionStart
            _itemCount = itemCount
            _payload = payload
        }

        fun cmdToString(): String = when (_cmd) {
                ADD -> "add"
                REMOVE -> "rm"
                UPDATE -> "up"
                MOVE -> "mv"
                else -> "??"
            }

        override fun toString(): String = Integer.toHexString(System.identityHashCode(this))+ "[${cmdToString()},s:${_positionStart},c:${_itemCount},p:${_payload}]"

        override fun equals(o: Any?): Boolean {
            if (this == o) {
                return true
            }
            if (o == null || javaClass != o.javaClass) {
                return false
            }
            val op = o as UpdateOp
            if (_cmd != op._cmd) {
                return false
            }
            if (_cmd == MOVE && Math.abs(_itemCount - _positionStart) == 1) {
                if (_itemCount == op._positionStart && _positionStart == op._itemCount) {
                    return true
                }
            }
            if (_itemCount != op._itemCount) {
                return false
            }
            if (_positionStart != op._positionStart) {
                return false
            }
            if (_payload != null) {
                if (!_payload!!.equals(op._payload)) {
                    return false
                }
            } else if (op._payload != null) {
                return false
            }
            return true
        }

        override fun hashCode(): Int {
            var result = _cmd
            result = 31 * result + _positionStart
            result = 31 * result + _itemCount
            return result
        }
    }

    override fun obtainUpdateOp(cmd: Int, startPosition: Int, itemCount: Int, payload: Any?): UpdateOp? {
        var op = _updateOpPool.acquire()
        if (op == null) {
            op = UpdateOp(cmd, startPosition, itemCount, payload)
        } else {
            op._cmd = cmd
            op._positionStart = startPosition
            op._itemCount = itemCount
            op._payload = payload
        }
        return op
    }

    override fun recycleUpdateOp(op: UpdateOp?) {
        if (!_disableRecycler) {
            op?._payload = null
            _updateOpPool.release(op)
        }
    }

    interface Callback {

        fun findViewHolder(position: Int): RecyclerView.ViewHolder?
        fun offsetPositionsForRemovingInvisible(positionStart: Int, itemCount: Int)
        fun offsetPositionsForRemovingLaidOutOrNewView(positionStart: Int, itemCount: Int)
        fun markViewHoldersUpdated(positionStart: Int, itemCount: Int, payload: Any?)
        fun onDispatchFirstPass(updateOp: UpdateOp?)
        fun onDispatchSecondPass(updateOp: UpdateOp?)
        fun offsetPositionsForAdd(positionStart: Int, itemCount: Int)
        fun offsetPositionsForMove(from: Int, to: Int)
    }

}