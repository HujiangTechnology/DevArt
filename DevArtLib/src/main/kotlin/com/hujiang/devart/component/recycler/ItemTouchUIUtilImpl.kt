package com.hujiang.devart.component.recycler

import android.graphics.Canvas
import android.support.v4.view.ViewCompat
import android.view.View
import com.hujiang.devart.R

/**
 * Created by rarnu on 4/25/16.
 */
class ItemTouchUIUtilImpl {

    open class Honeycomb: ItemTouchUIUtil {
        override fun clearView(view: View?) {
            ViewCompat.setTranslationX(view, 0.0f)
            ViewCompat.setTranslationY(view, 0.0f)
        }
        override fun onSelected(view: View?) { }
        override fun onDraw(c: Canvas?, recyclerView: RecyclerView?, view: View?, dX: Float, dY: Float, actionState: Int, isCurrentlyActive: Boolean) {
            ViewCompat.setTranslationX(view, dX)
            ViewCompat.setTranslationY(view, dY)
        }
        override fun onDrawOver(c: Canvas?, recyclerView: RecyclerView?, view: View?, dX: Float, dY: Float, actionState: Int, isCurrentlyActive: Boolean) { }
    }

    class Gingerbread: ItemTouchUIUtil {

        override fun clearView(view: View?) {
            view?.setVisibility(View.VISIBLE)
        }

        override fun onSelected(view: View?) {
            view?.setVisibility(View.INVISIBLE)
        }

        override fun onDraw(c: Canvas?, recyclerView: RecyclerView?, view: View?, dX: Float, dY: Float, actionState: Int, isCurrentlyActive: Boolean) {
            if (actionState != ItemTouchHelper.ACTION_STATE_DRAG) {
                draw(c, recyclerView, view, dX, dY)
            }
        }

        override fun onDrawOver(c: Canvas?, recyclerView: RecyclerView?, view: View?, dX: Float, dY: Float, actionState: Int, isCurrentlyActive: Boolean) {
            if (actionState == ItemTouchHelper.ACTION_STATE_DRAG) {
                draw(c, recyclerView, view, dX, dY)
            }
        }
        private fun draw(c: Canvas?, parent: RecyclerView?, view: View?, dX: Float, dY: Float) {
            c?.save()
            c?.translate(dX, dY)
            parent?.drawChild(c, view, 0)
            c?.restore()
        }
    }

    class Lollipop: Honeycomb() {

        override fun onDraw(c: Canvas?, recyclerView: RecyclerView?, view: View?, dX: Float, dY: Float, actionState: Int, isCurrentlyActive: Boolean) {
            if (isCurrentlyActive) {
                var originalElevation = view?.getTag(R.id.item_touch_helper_previous_elevation)
                if (originalElevation == null) {
                    originalElevation = ViewCompat.getElevation(view)
                    val newElevation = 1.0f + findMaxElevation(recyclerView, view)
                    ViewCompat.setElevation(view, newElevation)
                    view?.setTag(R.id.item_touch_helper_previous_elevation, originalElevation)
                }
            }
            super.onDraw(c, recyclerView, view, dX, dY, actionState, isCurrentlyActive)
        }

        fun findMaxElevation(recyclerView: RecyclerView?, itemView: View?): Float {
            val childCount = recyclerView!!.getChildCount()
            var max = 0.0f
            for (i in 0..childCount - 1) {
            val child = recyclerView.getChildAt(i)
            if (child == itemView) {
                continue
            }
            val elevation = ViewCompat.getElevation(child)
            if (elevation > max) {
                max = elevation
            }
        }
            return max
        }

        override fun clearView(view: View?) {
            val tag = view?.getTag(R.id.item_touch_helper_previous_elevation)
            if (tag != null && tag is Float) {
                ViewCompat.setElevation(view, tag)
            }
            view?.setTag(R.id.item_touch_helper_previous_elevation, null)
            super.clearView(view)
        }

    }

}