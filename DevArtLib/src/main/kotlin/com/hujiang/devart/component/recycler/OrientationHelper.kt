package com.hujiang.devart.component.recycler

import android.view.View
import android.widget.LinearLayout

/**
 * Created by rarnu on 4/24/16.
 */
abstract class OrientationHelper {

    companion object {
        val INVALID_SIZE = Integer.MIN_VALUE
        val HORIZONTAL = LinearLayout.HORIZONTAL
        val VERTICAL = LinearLayout.VERTICAL

        fun createOrientationHelper(layoutManager: RecyclerView.LayoutManager?, orientation: Int): OrientationHelper {
            when (orientation) {
                HORIZONTAL -> return createHorizontalHelper(layoutManager)
                VERTICAL -> return createVerticalHelper(layoutManager)
            }
            throw IllegalArgumentException("invalid orientation")
        }

        fun createVerticalHelper(layoutManager: RecyclerView.LayoutManager?): OrientationHelper = object: OrientationHelper(layoutManager) {
            override fun getEndAfterPadding(): Int = _layoutManager!!.getHeight() - _layoutManager!!.getPaddingBottom()
            override fun getEnd(): Int = _layoutManager!!.getHeight()
            override fun offsetChildren(amount: Int) {
                _layoutManager?.offsetChildrenVertical(amount)
            }
            override fun getStartAfterPadding(): Int = _layoutManager!!.getPaddingTop()
            override fun getDecoratedMeasurement(view: View?): Int {
                val params = view?.getLayoutParams() as RecyclerView.LayoutParams?
                return _layoutManager!!.getDecoratedMeasuredHeight(view) + params!!.topMargin + params.bottomMargin
            }

            override fun getDecoratedMeasurementInOther(view: View?): Int {
                val params = view?.getLayoutParams() as RecyclerView.LayoutParams?
                return _layoutManager!!.getDecoratedMeasuredWidth(view) + params!!.leftMargin + params.rightMargin
            }

            override fun getDecoratedEnd(view: View?): Int {
                val params = view?.getLayoutParams() as RecyclerView.LayoutParams?
                return _layoutManager!!.getDecoratedBottom(view) + params!!.bottomMargin
            }

            override fun getDecoratedStart(view: View?): Int {
                val params = view?.getLayoutParams() as RecyclerView.LayoutParams?
                return _layoutManager!!.getDecoratedTop(view) - params!!.topMargin
            }

            override fun getTotalSpace(): Int = _layoutManager!!.getHeight() - _layoutManager!!.getPaddingTop()- _layoutManager!!.getPaddingBottom()

            override fun offsetChild(view: View?, offset: Int) {
                view?.offsetTopAndBottom(offset)
            }

            override fun getEndPadding(): Int = _layoutManager!!.getPaddingBottom()
        }


        fun createHorizontalHelper(layoutManager: RecyclerView.LayoutManager?): OrientationHelper = object : OrientationHelper(layoutManager) {
            override fun getEndAfterPadding(): Int = _layoutManager!!.getWidth() - _layoutManager!!.getPaddingRight()
            override fun getEnd(): Int = _layoutManager!!.getWidth()
            override fun offsetChildren(amount: Int) {
                _layoutManager?.offsetChildrenHorizontal(amount)
            }

            override fun getStartAfterPadding(): Int = _layoutManager!!.getPaddingLeft()
            override fun getDecoratedMeasurement(view: View?): Int {
                val params = view?.getLayoutParams() as RecyclerView.LayoutParams?
                return _layoutManager!!.getDecoratedMeasuredWidth(view) + params!!.leftMargin + params.rightMargin
            }

            override fun getDecoratedMeasurementInOther(view: View?): Int {
                val params = view?.getLayoutParams() as RecyclerView.LayoutParams?
                return _layoutManager!!.getDecoratedMeasuredHeight(view) + params!!.topMargin + params.bottomMargin
            }

            override fun getDecoratedEnd(view: View?): Int {
                val params = view?.getLayoutParams() as RecyclerView.LayoutParams?
                return _layoutManager!!.getDecoratedRight(view) + params!!.rightMargin
            }

            override fun getDecoratedStart(view: View?): Int {
                val params = view?.getLayoutParams() as RecyclerView.LayoutParams?
                return _layoutManager!!.getDecoratedLeft(view) - params!!.leftMargin
            }

            override fun getTotalSpace(): Int = _layoutManager!!.getWidth() - _layoutManager!!.getPaddingLeft() - _layoutManager!!.getPaddingRight()

            override fun offsetChild(view: View?, offset: Int) {
                view?.offsetLeftAndRight(offset)
            }

            override fun getEndPadding(): Int = _layoutManager!!.getPaddingRight()
        }
    }

    var _layoutManager: RecyclerView.LayoutManager? = null
    private var _lastTotalSpace = INVALID_SIZE

    constructor(layoutManager: RecyclerView.LayoutManager?) {
        _layoutManager = layoutManager
    }

    fun onLayoutComplete() {
        _lastTotalSpace = getTotalSpace()
    }

    fun getTotalSpaceChange(): Int {
        return if (INVALID_SIZE == _lastTotalSpace) 0 else getTotalSpace() - _lastTotalSpace
    }

    abstract fun getDecoratedStart(view: View?): Int

    abstract fun getDecoratedEnd(view: View?): Int

    abstract fun getDecoratedMeasurement(view: View?): Int

    abstract fun getDecoratedMeasurementInOther(view: View?): Int

    abstract fun getStartAfterPadding(): Int

    abstract fun getEndAfterPadding(): Int

    abstract fun getEnd(): Int

    abstract fun offsetChildren(amount: Int)

    abstract fun getTotalSpace(): Int

    abstract fun offsetChild(view: View?, offset: Int)

    abstract fun getEndPadding(): Int



}