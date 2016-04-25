package com.hujiang.devart.component.recycler

import android.support.v4.animation.AnimatorCompatHelper
import android.support.v4.view.ViewCompat
import android.support.v4.view.ViewPropertyAnimatorListener
import android.view.View

/**
 * Created by rarnu on 4/23/16.
 */
class DefaultItemAnimator : RecyclerView.ItemAnimator() {


    private val _pendingRemovals = arrayListOf<RecyclerView.ViewHolder?>()
    private val _pendingAdditions = arrayListOf<RecyclerView.ViewHolder?>()
    private val _pendingMoves = arrayListOf<MoveInfo?>()
    private val _pendingChanges = arrayListOf<ChangeInfo?>()
    private val _additionsList = arrayListOf<MutableList<RecyclerView.ViewHolder?>?>()
    private val _movesList = arrayListOf<MutableList<MoveInfo?>?>()
    private val _changesList = arrayListOf<MutableList<ChangeInfo?>?>()

    private val _addAnimations = arrayListOf<RecyclerView.ViewHolder?>()
    private val _moveAnimations = arrayListOf<RecyclerView.ViewHolder?>()
    private val _removeAnimations = arrayListOf<RecyclerView.ViewHolder?>()
    private val _changeAnimations = arrayListOf<RecyclerView.ViewHolder?>()

    class MoveInfo {
        var _holder: RecyclerView.ViewHolder? = null
        var _fromX = 0
        var _fromY = 0
        var _toX = 0
        var _toY = 0

        constructor(holder: RecyclerView.ViewHolder?, fromX: Int, fromY: Int, toX: Int, toY: Int) {
            _holder = holder
            _fromX = fromX
            _fromY = fromY
            _toX = toX
            _toY = toY
        }
    }

    class ChangeInfo {
        var _oldHolder: RecyclerView.ViewHolder? = null
        var _newHolder: RecyclerView.ViewHolder? = null
        var _fromX = 0
        var _fromY = 0
        var _toX = 0
        var _toY = 0

        constructor(oldHolder: RecyclerView.ViewHolder?, newHolder: RecyclerView.ViewHolder?) {
            _oldHolder = oldHolder
            _newHolder = newHolder
        }

        constructor(oldHolder: RecyclerView.ViewHolder?, newHolder: RecyclerView.ViewHolder?, fromX: Int, fromY: Int, toX: Int, toY: Int) : this(oldHolder, newHolder) {
            _fromX = fromX
            _fromY = fromY
            _toX = toX
            _toY = toY
        }

        override fun toString(): String = "ChangeInfo{oldHolder=${_oldHolder}, newHolder=${_newHolder}, fromX=${_fromX}, fromY=${_fromY}, toX=${_toX}, toY=${_toY}}"
    }

    open class VpaListenerAdapter : ViewPropertyAnimatorListener {
        override fun onAnimationStart(p0: View?) {
        }

        override fun onAnimationEnd(p0: View?) {
        }

        override fun onAnimationCancel(p0: View?) {
        }
    }

    private fun resetAnimation(holder: RecyclerView.ViewHolder?) {
        AnimatorCompatHelper.clearInterpolator(holder?._itemView)
        endAnimation(holder)
    }

    override fun animateRemove(holder: RecyclerView.ViewHolder?): Boolean {
        resetAnimation(holder)
        _pendingRemovals.add(holder)
        return true
    }

    override fun animateAdd(holder: RecyclerView.ViewHolder?): Boolean {
        resetAnimation(holder)
        ViewCompat.setAlpha(holder?._itemView, 0.0f)
        _pendingAdditions.add(holder)
        return true
    }

    override fun animateMove(holder: RecyclerView.ViewHolder?, fromX: Int, fromY: Int, toX: Int, toY: Int): Boolean {
        var nfromX = fromX
        var nfromY = fromY
        val view = holder?._itemView
        nfromX += ViewCompat.getTranslationX(holder?._itemView).toInt()
        nfromY += ViewCompat.getTranslationY(holder?._itemView).toInt()
        resetAnimation(holder)
        var deltaX = toX - nfromX
        var deltaY = toY - nfromY
        if (deltaX == 0 && deltaY == 0) {
            dispatchMoveFinished(holder)
            return false
        }
        if (deltaX != 0) {
            ViewCompat.setTranslationX(view, -deltaX.toFloat())
        }
        if (deltaY != 0) {
            ViewCompat.setTranslationY(view, -deltaY.toFloat())
        }
        _pendingMoves.add(MoveInfo(holder, nfromX, nfromY, toX, toY))
        return true
    }

    override fun animateChange(oldHolder: RecyclerView.ViewHolder?, newHolder: RecyclerView.ViewHolder?, fromLeft: Int, fromTop: Int, toLeft: Int, toTop: Int): Boolean {
        val prevTranslationX = ViewCompat.getTranslationX(oldHolder?._itemView)
        val prevTranslationY = ViewCompat.getTranslationY(oldHolder?._itemView)
        val prevAlpha = ViewCompat.getAlpha(oldHolder?._itemView)
        resetAnimation(oldHolder)
        val deltaX = (toLeft - fromLeft - prevTranslationX).toInt()
        val deltaY = (toTop - fromTop - prevTranslationY).toInt()
        ViewCompat.setTranslationX(oldHolder?._itemView, prevTranslationX)
        ViewCompat.setTranslationY(oldHolder?._itemView, prevTranslationY)
        ViewCompat.setAlpha(oldHolder?._itemView, prevAlpha)
        if (newHolder != null && newHolder._itemView != null) {
            resetAnimation(newHolder)
            ViewCompat.setTranslationX(newHolder._itemView, -deltaX.toFloat())
            ViewCompat.setTranslationY(newHolder._itemView, -deltaY.toFloat())
            ViewCompat.setAlpha(newHolder._itemView, 0.0f)
        }
        _pendingChanges.add(ChangeInfo(oldHolder, newHolder, fromLeft, fromTop, toLeft, toTop))
        return true
    }

    private fun endChangeAnimationIfNecessary(changeInfo: ChangeInfo?) {
        if (changeInfo?._oldHolder != null) {
            endChangeAnimationIfNecessary(changeInfo, changeInfo?._oldHolder)
        }
        if (changeInfo?._newHolder != null) {
            endChangeAnimationIfNecessary(changeInfo, changeInfo?._newHolder)
        }
    }

    private fun endChangeAnimationIfNecessary(changeInfo: ChangeInfo?, item: RecyclerView.ViewHolder?): Boolean {
        var oldItem = false
        if (changeInfo?._newHolder == item) {
            changeInfo?._newHolder = null
        } else if (changeInfo?._oldHolder == item) {
            changeInfo?._oldHolder = null
            oldItem = true
        } else {
            return false
        }
        ViewCompat.setAlpha(item?._itemView, 1.0f)
        ViewCompat.setTranslationX(item?._itemView, 0.0f)
        ViewCompat.setTranslationY(item?._itemView, 0.0f)
        dispatchChangeFinished(item, oldItem)
        return true
    }

    private fun endChangeAnimation(infoList: MutableList<ChangeInfo?>?, item: RecyclerView.ViewHolder?) {
        for (i in infoList!!.size - 1 downTo 0) {
            val changeInfo = infoList.get(i)
            if (endChangeAnimationIfNecessary(changeInfo, item)) {
                if (changeInfo?._oldHolder == null && changeInfo?._newHolder == null) {
                    infoList.remove(changeInfo)
                }
            }
        }
    }

    override fun endAnimation(item: RecyclerView.ViewHolder?) {
        val view = item?._itemView
        ViewCompat.animate(view).cancel()
        for (i in _pendingMoves.size - 1 downTo 0) {
            val moveInfo = _pendingMoves.get(i)
            if (moveInfo?._holder == item) {
                ViewCompat.setTranslationY(view, 0.0f)
                ViewCompat.setTranslationX(view, 0.0f)
                dispatchMoveFinished(item)
                _pendingMoves.removeAt(i)
            }
        }
        endChangeAnimation(_pendingChanges, item)
        if (_pendingRemovals.remove(item)) {
            ViewCompat.setAlpha(view, 1.0f)
            dispatchRemoveFinished(item)
        }
        if (_pendingAdditions.remove(item)) {
            ViewCompat.setAlpha(view, 1.0f)
            dispatchAddFinished(item)
        }

        for (i in _changesList.size - 1 downTo 0) {
            val changes = _changesList.get(i)
            endChangeAnimation(changes, item)
            if (changes!!.isEmpty()) {
                _changesList.removeAt(i)
            }
        }
        for (i in _movesList.size - 1 downTo 0) {
            val moves = _movesList.get(i)
            for (j in moves!!.size - 1 downTo 0) {
                val moveInfo = moves.get(j)
                if (moveInfo?._holder == item) {
                    ViewCompat.setTranslationY(view, 0.0f)
                    ViewCompat.setTranslationX(view, 0.0f)
                    dispatchMoveFinished(item)
                    moves.removeAt(j)
                    if (moves.isEmpty()) {
                        _movesList.removeAt(i)
                    }
                    break
                }
            }
        }
        for (i in _additionsList.size - 1 downTo 0) {
            val additions = _additionsList.get(i)
            if (additions!!.remove(item)) {
                ViewCompat.setAlpha(view, 1.0f)
                dispatchAddFinished(item)
                if (additions.isEmpty()) {
                    _additionsList.removeAt(i)
                }
            }
        }
        _removeAnimations.remove(item)
        _addAnimations.remove(item)
        _changeAnimations.remove(item)
        _moveAnimations.remove(item)
        dispatchFinishedWhenDone()
    }

    private fun dispatchFinishedWhenDone() {
        if (!isRunning()) {
            dispatchAnimationsFinished()
        }
    }

    override fun endAnimations() {
        var count = _pendingMoves.size
        for (i in count - 1 downTo 0) {
            val item = _pendingMoves.get(i)
            val view = item?._holder?._itemView
            ViewCompat.setTranslationY(view, 0.0f)
            ViewCompat.setTranslationX(view, 0.0f)
            dispatchMoveFinished(item?._holder)
            _pendingMoves.removeAt(i)
        }
        count = _pendingRemovals.size
        for (i in count - 1 downTo 0) {
            val item = _pendingRemovals.get(i)
            dispatchRemoveFinished(item)
            _pendingRemovals.removeAt(i)
        }
        count = _pendingAdditions.size
        for (i in count - 1 downTo 0) {
            val item = _pendingAdditions.get(i)
            val view = item?._itemView
            ViewCompat.setAlpha(view, 1.0f)
            dispatchAddFinished(item)
            _pendingAdditions.removeAt(i)
        }
        count = _pendingChanges.size
        for (i in count - 1 downTo 0) {
            endChangeAnimationIfNecessary(_pendingChanges.get(i))
        }
        _pendingChanges.clear()
        if (!isRunning()) {
            return
        }
        var listCount = _movesList.size
        for (i in listCount - 1 downTo 0) {
            val moves = _movesList.get(i)
            count = moves!!.size
            for (j in count - 1 downTo 0) {
                val moveInfo = moves.get(j)
                val item = moveInfo?._holder
                val view = item?._itemView
                ViewCompat.setTranslationY(view, 0.0f)
                ViewCompat.setTranslationX(view, 0.0f)
                dispatchMoveFinished(moveInfo?._holder)
                moves.removeAt(j)
                if (moves.isEmpty()) {
                    _movesList.remove(moves)
                }
            }
        }
        listCount = _additionsList.size
        for (i in listCount - 1 downTo 0) {
            val additions = _additionsList.get(i)
            count = additions!!.size
            for (j in count - 1 downTo 0) {
                val item = additions.get(j)
                val view = item?._itemView
                ViewCompat.setAlpha(view, 1.0f)
                dispatchAddFinished(item)
                additions.removeAt(j)
                if (additions.isEmpty()) {
                    _additionsList.remove(additions)
                }
            }
        }
        listCount = _changesList.size
        for (i in listCount - 1 downTo 0) {
            val changes = _changesList.get(i)
            count = changes!!.size
            for (j in count - 1 downTo 0) {
                endChangeAnimationIfNecessary(changes.get(j))
                if (changes.isEmpty()) {
                    _changesList.remove(changes)
                }
            }
        }
        cancelAll(_removeAnimations)
        cancelAll(_moveAnimations)
        cancelAll(_addAnimations)
        cancelAll(_changeAnimations)
        dispatchAnimationsFinished()
    }

    fun cancelAll(viewHolders: MutableList<RecyclerView.ViewHolder?>?) {
        for (i in viewHolders!!.size - 1 downTo 0) {
            ViewCompat.animate(viewHolders.get(i)?._itemView).cancel()
        }
    }

    override fun isRunning(): Boolean =
            !_pendingAdditions.isEmpty() ||
                    !_pendingChanges.isEmpty() ||
                    !_pendingMoves.isEmpty() ||
                    !_pendingRemovals.isEmpty() ||
                    !_moveAnimations.isEmpty() ||
                    !_removeAnimations.isEmpty() ||
                    !_addAnimations.isEmpty() ||
                    !_changeAnimations.isEmpty() ||
                    !_movesList.isEmpty() ||
                    !_additionsList.isEmpty() ||
                    !_changesList.isEmpty()


    private fun animateRemoveImpl(holder: RecyclerView.ViewHolder?) {
        val view = holder?._itemView
        val animation = ViewCompat.animate(view)
        _removeAnimations.add(holder)
        animation.setDuration(getRemoveDuration()).alpha(0.0f).setListener(object : VpaListenerAdapter() {

            override fun onAnimationStart(v: View?) {
                dispatchRemoveStarting(holder)
            }

            override fun onAnimationEnd(v: View?) {
                animation.setListener(null)
                ViewCompat.setAlpha(view, 1.0f)
                dispatchRemoveFinished(holder)
                _removeAnimations.remove(holder)
                dispatchFinishedWhenDone()
            }
        }).start()
    }

    private fun animateMoveImpl(holder: RecyclerView.ViewHolder?, fromX: Int, fromY: Int, toX: Int, toY: Int) {
        val view = holder?._itemView
        val deltaX = toX - fromX
        val deltaY = toY - fromY
        if (deltaX != 0) {
            ViewCompat.animate(view).translationX(0.0f)
        }
        if (deltaY != 0) {
            ViewCompat.animate(view).translationY(0.0f)
        }
        val animation = ViewCompat.animate(view)
        _moveAnimations.add(holder)
        animation.setDuration(getMoveDuration()).setListener(object: VpaListenerAdapter() {

            override fun onAnimationStart(v: View?) {
                dispatchMoveStarting(holder)
            }

            override fun onAnimationCancel(v: View?) {
                if (deltaX != 0) {
                    ViewCompat.setTranslationX(view, 0.0f)
                }
                if (deltaY != 0) {
                    ViewCompat.setTranslationY(view, 0.0f)
                }
            }

            override fun onAnimationEnd(v: View?) {
                animation.setListener(null)
                dispatchMoveFinished(holder)
                _moveAnimations.remove(holder)
                dispatchFinishedWhenDone()
            }
        }).start()
    }

    private fun animateChangeImpl(changeInfo: ChangeInfo?) {
        val holder = changeInfo?._oldHolder
        val view = if (holder == null) null else holder._itemView
        val newHolder = changeInfo?._newHolder
        val newView = if (newHolder != null) newHolder._itemView else null
        if (view != null) {
            val oldViewAnim = ViewCompat.animate(view).setDuration(getChangeDuration())
            _changeAnimations.add(changeInfo?._oldHolder)
            oldViewAnim.translationX((changeInfo!!._toX - changeInfo._fromX).toFloat())
            oldViewAnim.translationY((changeInfo._toY - changeInfo._fromY).toFloat())
            oldViewAnim.alpha(0.0f).setListener(object: VpaListenerAdapter() {

                override fun onAnimationStart(p0: View?) {
                    dispatchChangeStarting(changeInfo._oldHolder, true)
                }

                override fun onAnimationEnd(p0: View?) {
                    oldViewAnim.setListener(null)
                    ViewCompat.setAlpha(view, 1.0f)
                    ViewCompat.setTranslationX(view, 0.0f)
                    ViewCompat.setTranslationY(view, 0.0f)
                    dispatchChangeFinished(changeInfo._oldHolder, true)
                    _changeAnimations.remove(changeInfo._oldHolder)
                    dispatchFinishedWhenDone()
                }
            }).start()
        }
        if (newView != null) {
            val newViewAnimation = ViewCompat.animate(newView)
            _changeAnimations.add(changeInfo?._newHolder)
            newViewAnimation.translationX(0.0f).translationY(0.0f).setDuration(getChangeDuration()).alpha(1.0f).setListener(object: VpaListenerAdapter() {
                override fun onAnimationStart(p0: View?) {
                    dispatchChangeStarting(changeInfo?._newHolder, false)
                }

                override fun onAnimationEnd(p0: View?) {
                    newViewAnimation.setListener(null)
                    ViewCompat.setAlpha(newView, 1.0f)
                    ViewCompat.setTranslationX(newView, 0.0f)
                    ViewCompat.setTranslationY(newView, 0.0f)
                    dispatchChangeFinished(changeInfo?._newHolder, false)
                    _changeAnimations.remove(changeInfo?._newHolder)
                    dispatchFinishedWhenDone()
                }
            }).start()
        }
    }

    override fun runPendingAnimations() {
        var removalsPending = !_pendingRemovals.isEmpty()
        var movesPending = !_pendingMoves.isEmpty()
        var changesPending = !_pendingChanges.isEmpty()
        var additionsPending = !_pendingAdditions.isEmpty()
        if (!removalsPending && !movesPending && !additionsPending && !changesPending) {
            return
        }
        for (holder in _pendingRemovals) {
            animateRemoveImpl(holder)
        }
        _pendingRemovals.clear()
        if (movesPending) {
            val moves = arrayListOf<MoveInfo?>()
            moves.addAll(_pendingMoves)
            _movesList.add(moves)
            _pendingMoves.clear()
            val mover = Runnable {
                for (moveInfo in moves) {
                    animateMoveImpl(moveInfo!!._holder, moveInfo._fromX, moveInfo._fromY, moveInfo._toX, moveInfo._toY)
                }
                moves.clear()
                _movesList.remove(moves)
            }
            if (removalsPending) {
                val view = moves.get(0)?._holder?._itemView
                ViewCompat.postOnAnimationDelayed(view, mover, getRemoveDuration())
            } else {
                mover.run()
            }
        }
        if (changesPending) {
            val changes = arrayListOf<ChangeInfo?>()
            changes.addAll(_pendingChanges)
            _changesList.add(changes)
            _pendingChanges.clear()
            val changer = Runnable {
                for (change in changes) {
                    animateChangeImpl(change)
                }
                changes.clear()
                _changesList.remove(changes)
            }
            if (removalsPending) {
                val holder = changes.get(0)?._oldHolder
                ViewCompat.postOnAnimationDelayed(holder?._itemView, changer, getRemoveDuration())
            } else {
                changer.run()
            }
        }
        if (additionsPending) {
            val additions = arrayListOf<RecyclerView.ViewHolder?>()
            additions.addAll(_pendingAdditions)
            _additionsList.add(additions)
            _pendingAdditions.clear()
            val adder = Runnable {
                for (holder in additions) {
                    animateAddImpl(holder)
                }
                additions.clear()
                _additionsList.remove(additions)
            }
            if (removalsPending || movesPending || changesPending) {
                val removeDuration = if (removalsPending) getRemoveDuration() else 0
                val moveDuration = if (movesPending) getMoveDuration() else 0
                val changeDuration = if (changesPending) getChangeDuration() else 0
                val totalDelay = removeDuration + Math.max(moveDuration, changeDuration)
                val view = additions.get(0)?._itemView
                ViewCompat.postOnAnimationDelayed(view, adder, totalDelay)
            } else {
                adder.run()
            }
        }
    }

    private fun animateAddImpl(holder: RecyclerView.ViewHolder?) {
        val view = holder?._itemView
        val animation = ViewCompat.animate(view)
        _addAnimations.add(holder)
        animation.alpha(1.0f).setDuration(getAddDuration()).setListener(object: VpaListenerAdapter() {

            override fun onAnimationStart(v: View?) {
                dispatchAddStarting(holder)
            }

            override fun onAnimationCancel(v: View?) {
                ViewCompat.setAlpha(view, 1.0f)
            }

            override fun onAnimationEnd(v: View?) {
                animation.setListener(null)
                dispatchAddFinished(holder)
                _addAnimations.remove(holder)
                dispatchFinishedWhenDone()
            }
        }).start()
    }

}