package com.hujiang.devart.component.recycler

import android.view.View

/**
 * Created by rarnu on 4/25/16.
 */
object ScrollbarHelper {

    fun computeScrollOffset(state: RecyclerView.State?, orientation: OrientationHelper?, startChild: View?, endChild: View?, lm: RecyclerView.LayoutManager?, smoothScrollbarEnabled: Boolean, reverseLayout: Boolean): Int {
        if (lm!!.getChildCount() == 0 || state!!.getItemCount() == 0 || startChild == null || endChild == null) {
            return 0
        }
        val minPosition = Math.min(lm.getPosition(startChild), lm.getPosition(endChild))
        val maxPosition = Math.max(lm.getPosition(startChild), lm.getPosition(endChild))
        val itemsBefore = if (reverseLayout) Math.max(0, state.getItemCount() - maxPosition - 1) else Math.max(0, minPosition)
        if (!smoothScrollbarEnabled) {
            return itemsBefore
        }
        val laidOutArea = Math.abs(orientation!!.getDecoratedEnd(endChild) - orientation.getDecoratedStart(startChild))
        val itemRange = Math.abs(lm.getPosition(startChild) - lm.getPosition(endChild)) + 1
        val avgSizePerRow = laidOutArea * 1.0f / itemRange
        return Math.round(itemsBefore * avgSizePerRow + (orientation.getStartAfterPadding() - orientation.getDecoratedStart(startChild)))
    }

    fun computeScrollExtent(state: RecyclerView.State?, orientation: OrientationHelper?, startChild: View?, endChild: View?, lm: RecyclerView.LayoutManager?, smoothScrollbarEnabled: Boolean): Int {
        if (lm!!.getChildCount() == 0 || state!!.getItemCount() == 0 || startChild == null || endChild == null) {
            return 0
        }
        if (!smoothScrollbarEnabled) {
            return Math.abs(lm.getPosition(startChild) - lm.getPosition(endChild)) + 1
        }
        val extend = orientation!!.getDecoratedEnd(endChild)- orientation!!.getDecoratedStart(startChild)
        return Math.min(orientation.getTotalSpace(), extend)
    }

    fun computeScrollRange(state: RecyclerView.State?, orientation: OrientationHelper?, startChild: View?, endChild: View?, lm: RecyclerView.LayoutManager?, smoothScrollbarEnabled: Boolean): Int {
        if (lm!!.getChildCount() == 0 || state!!.getItemCount() == 0 || startChild == null || endChild == null) {
            return 0
        }
        if (!smoothScrollbarEnabled) {
            return state.getItemCount()
        }
        val laidOutArea = orientation!!.getDecoratedEnd(endChild) - orientation.getDecoratedStart(startChild)
        val laidOutRange = Math.abs(lm.getPosition(startChild) - lm.getPosition(endChild)) + 1
        return (laidOutArea * 1.0f / laidOutRange * state.getItemCount()).toInt()
    }

}