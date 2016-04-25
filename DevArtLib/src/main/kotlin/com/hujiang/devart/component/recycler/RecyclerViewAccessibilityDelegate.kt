package com.hujiang.devart.component.recycler

import android.os.Bundle
import android.support.v4.view.AccessibilityDelegateCompat
import android.support.v4.view.accessibility.AccessibilityNodeInfoCompat
import android.view.View
import android.view.accessibility.AccessibilityEvent

/**
 * Created by rarnu on 4/21/16.
 */
class RecyclerViewAccessibilityDelegate: AccessibilityDelegateCompat {

    var _recyclerView: RecyclerView? = null

    constructor(recyclerView: RecyclerView?) {
        _recyclerView = recyclerView
    }

    override fun performAccessibilityAction(host: View?, action: Int, args: Bundle?): Boolean {
        if (super.performAccessibilityAction(host, action, args)) {
            return true
        }
        if (!shouldIgnore() && _recyclerView?.getLayoutManager() != null) {
            return _recyclerView!!.getLayoutManager()!!.performAccessibilityAction(action, args)
        }
        return false
    }

    private fun shouldIgnore(): Boolean = _recyclerView!!.hasPendingAdapterUpdates()


    override fun onInitializeAccessibilityEvent(host: View?, event: AccessibilityEvent?) {
        super.onInitializeAccessibilityEvent(host, event)
        event?.className = RecyclerView.javaClass.name
        if (host is RecyclerView && !shouldIgnore()) {
            val rv = host
            rv.getLayoutManager()?.onInitializeAccessibilityEvent(event)
        }
    }

    override fun onInitializeAccessibilityNodeInfo(host: View?, info: AccessibilityNodeInfoCompat?) {
        super.onInitializeAccessibilityNodeInfo(host, info)
        info?.className = RecyclerView.javaClass.name
        if (!shouldIgnore() && _recyclerView?.getLayoutManager() != null) {
            _recyclerView?.getLayoutManager()?.onInitializeAccessibilityNodeInfo(info)
        }
    }

    fun getItemDelegate(): AccessibilityDelegateCompat? = _itemDelegate

    val _itemDelegate = object:AccessibilityDelegateCompat() {

        override fun onInitializeAccessibilityNodeInfo(host: View?, info: AccessibilityNodeInfoCompat?) {
            super.onInitializeAccessibilityNodeInfo(host, info);
            if (!shouldIgnore() && _recyclerView?.getLayoutManager() != null) {
                _recyclerView!!.getLayoutManager()!!.onInitializeAccessibilityNodeInfoForItem(host, info)
            }
        }

        override fun performAccessibilityAction(host: View?, action: Int, args: Bundle?): Boolean {
            if (super.performAccessibilityAction(host, action, args)) {
                return true
            }
            if (!shouldIgnore() && _recyclerView?.getLayoutManager() != null) {
                return _recyclerView!!.getLayoutManager()!!.performAccessibilityActionForItem(host, action, args)
            }
            return false
        }
    }
}