package com.hujiang.devart.component.coverflow

import android.content.Context
import android.view.GestureDetector
import android.view.MotionEvent
import android.view.View
import com.hujiang.devart.component.recycler.RecyclerView

/**
 * Created by rarnu on 4/19/16.
 */
open class RecyclerItemClickListener: RecyclerView.OnItemTouchListener {

    interface OnItemClickListener {
        fun onItemClick(view: View?, position: Int)
    }

    private var _listener: OnItemClickListener? = null
    private var _gestureDetector: GestureDetector? = null

    constructor(context: Context, listener: OnItemClickListener?) {
        _listener = listener
        _gestureDetector = GestureDetector(context, object: GestureDetector.SimpleOnGestureListener() {
            override fun onSingleTapUp(e: MotionEvent?): Boolean {
                return true
            }
        })
    }

    override fun onInterceptTouchEvent(view: RecyclerView?, e: MotionEvent?): Boolean {
        val childView = view?.findChildViewUnder(e!!.x, e.y)
        if (childView != null && _listener != null && _gestureDetector!!.onTouchEvent(e)) {
            _listener?.onItemClick(childView, view!!.getChildAdapterPosition(childView))
        }
        return false
    }

    override fun onTouchEvent(view: RecyclerView?, e: MotionEvent?) {

    }

    override fun onRequestDisallowInterceptTouchEvent(disallowIntercept: Boolean) {

    }

}