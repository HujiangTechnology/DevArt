package com.hujiang.devart.component.recycler

/**
 * Created by rarnu on 4/21/16.
 */
class OpReorderer {

    var _callback: Callback? = null

    constructor(callback: Callback?) {
        _callback = callback
    }

    fun reorderOps(ops: MutableList<AdapterHelper.UpdateOp?>?) {
        var badMove = getLastMoveOutOfOrder(ops)
        while (badMove != -1) {
            swapMoveOp(ops, badMove, badMove + 1)
            badMove = getLastMoveOutOfOrder(ops)
        }
    }

    private fun swapMoveOp(list: MutableList<AdapterHelper.UpdateOp?>?, badMove: Int, next: Int) {
        val moveOp = list!![badMove]
        val nextOp = list[next]
        when (nextOp!!._cmd) {
            AdapterHelper.UpdateOp.REMOVE -> swapMoveRemove(list, badMove, moveOp, next, nextOp)
            AdapterHelper.UpdateOp.ADD ->  swapMoveAdd(list, badMove, moveOp, next, nextOp)
            AdapterHelper.UpdateOp.UPDATE -> swapMoveUpdate(list, badMove, moveOp, next, nextOp)
        }
    }

    fun swapMoveUpdate(list: MutableList<AdapterHelper.UpdateOp?>?, move: Int, moveOp: AdapterHelper.UpdateOp?, update: Int, updateOp: AdapterHelper.UpdateOp?) {
        var extraUp1: AdapterHelper.UpdateOp? = null
        var extraUp2: AdapterHelper.UpdateOp? = null
        if (moveOp!!._itemCount < updateOp!!._positionStart) {
            updateOp._positionStart--
        } else if (moveOp._itemCount < updateOp._positionStart + updateOp._itemCount) {
            updateOp._itemCount--
            extraUp1 = _callback?.obtainUpdateOp(AdapterHelper.UpdateOp.UPDATE, moveOp._positionStart, 1, updateOp._payload)
        }
        if (moveOp._positionStart <= updateOp._positionStart) {
            updateOp._positionStart++
        } else if (moveOp._positionStart < updateOp._positionStart + updateOp._itemCount) {
            val remaining = updateOp._positionStart + updateOp._itemCount - moveOp._positionStart
            extraUp2 = _callback?.obtainUpdateOp(AdapterHelper.UpdateOp.UPDATE, moveOp._positionStart + 1, remaining, updateOp._payload)
            updateOp._itemCount -= remaining
        }
        list?.set(update, moveOp)
        if (updateOp._itemCount > 0) {
            list?.set(move, updateOp)
        } else {
            list?.removeAt(move)
            _callback?.recycleUpdateOp(updateOp)
        }
        if (extraUp1 != null) {
            list?.add(move, extraUp1)
        }
        if (extraUp2 != null) {
            list?.add(move, extraUp2)
        }
    }

    private fun swapMoveAdd(list: MutableList<AdapterHelper.UpdateOp?>?, move: Int, moveOp: AdapterHelper.UpdateOp?, add: Int, addOp: AdapterHelper.UpdateOp?) {
        var offset = 0
        if (moveOp!!._itemCount < addOp!!._positionStart) {
            offset--
        }
        if (moveOp._positionStart < addOp._positionStart) {
            offset++
        }
        if (addOp._positionStart <= moveOp._positionStart) {
            moveOp._positionStart += addOp._itemCount
        }
        if (addOp._positionStart <= moveOp._itemCount) {
            moveOp._itemCount += addOp._itemCount
        }
        addOp._positionStart += offset
        list?.set(move, addOp)
        list?.set(add, moveOp)
    }

    fun swapMoveRemove(list: MutableList<AdapterHelper.UpdateOp?>?, movePos: Int, moveOp: AdapterHelper.UpdateOp?, removePos: Int, removeOp: AdapterHelper.UpdateOp?) {
        var extraRm: AdapterHelper.UpdateOp? = null
        var revertedMove = false
        var moveIsBackwards: Boolean
        if (moveOp!!._positionStart < moveOp._itemCount) {
            moveIsBackwards = false
            if (removeOp!!._positionStart == moveOp._positionStart && removeOp._itemCount == moveOp._itemCount - moveOp._positionStart) {
                revertedMove = true
            }
        } else {
            moveIsBackwards = true
            if (removeOp!!._positionStart == moveOp._itemCount + 1 && removeOp._itemCount == moveOp._positionStart - moveOp._itemCount) {
                revertedMove = true
            }
        }

        if (moveOp._itemCount < removeOp._positionStart) {
            removeOp._positionStart--
        } else if (moveOp._itemCount < removeOp._positionStart + removeOp._itemCount) {
            removeOp._itemCount --
            moveOp._cmd = AdapterHelper.UpdateOp.REMOVE
            moveOp._itemCount = 1
            if (removeOp._itemCount == 0) {
                list?.removeAt(removePos)
                _callback?.recycleUpdateOp(removeOp)
            }
            return
        }
        if (moveOp._positionStart <= removeOp._positionStart) {
            removeOp._positionStart++
        } else if (moveOp._positionStart < removeOp._positionStart + removeOp._itemCount) {
            val remaining = removeOp._positionStart + removeOp._itemCount - moveOp._positionStart
            extraRm = _callback!!.obtainUpdateOp(AdapterHelper.UpdateOp.REMOVE, moveOp._positionStart + 1, remaining, null)
            removeOp._itemCount = moveOp._positionStart - removeOp._positionStart
        }
        if (revertedMove) {
            list?.set(movePos, removeOp)
            list?.removeAt(removePos)
            _callback?.recycleUpdateOp(moveOp)
            return
        }
        if (moveIsBackwards) {
            if (extraRm != null) {
                if (moveOp._positionStart > extraRm._positionStart) {
                    moveOp._positionStart -= extraRm._itemCount
                }
                if (moveOp._itemCount > extraRm._positionStart) {
                    moveOp._itemCount -= extraRm._itemCount
                }
            }
            if (moveOp._positionStart > removeOp._positionStart) {
                moveOp._positionStart -= removeOp._itemCount
            }
            if (moveOp._itemCount > removeOp._positionStart) {
                moveOp._itemCount -= removeOp._itemCount
            }
        } else {
            if (extraRm != null) {
                if (moveOp._positionStart >= extraRm._positionStart) {
                    moveOp._positionStart -= extraRm._itemCount
                }
                if (moveOp._itemCount >= extraRm._positionStart) {
                    moveOp._itemCount -= extraRm._itemCount
                }
            }
            if (moveOp._positionStart >= removeOp._positionStart) {
                moveOp._positionStart -= removeOp._itemCount
            }
            if (moveOp._itemCount >= removeOp._positionStart) {
                moveOp._itemCount -= removeOp._itemCount
            }
        }
        list?.set(movePos, removeOp)
        if (moveOp._positionStart != moveOp._itemCount) {
            list?.set(removePos, moveOp)
        } else {
            list?.removeAt(removePos)
        }
        if (extraRm != null) {
            list?.add(movePos, extraRm)
        }
    }

    private fun getLastMoveOutOfOrder(list: MutableList<AdapterHelper.UpdateOp?>?): Int {
        var foundNonMove = false
        for (i in list!!.size - 1 downTo 0) {
            val op1 = list[i]
            if (op1?._cmd == AdapterHelper.UpdateOp.MOVE) {
                if (foundNonMove) {
                    return i
                }
            } else {
                foundNonMove = true
            }
        }
        return -1
    }

    interface Callback {
        fun obtainUpdateOp(cmd: Int, startPosition: Int, itemCount: Int, payload: Any?): AdapterHelper.UpdateOp?
        fun recycleUpdateOp(op: AdapterHelper.UpdateOp?)
    }

}