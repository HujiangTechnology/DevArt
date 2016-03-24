package com.hujiang.devart.base

import android.app.Fragment
import android.app.FragmentManager
import android.app.FragmentTransaction
import android.os.Parcelable
import android.support.v4.view.PagerAdapter
import android.view.View
import android.view.ViewGroup

/**
 * Created by rarnu on 3/24/16.
 */
abstract class FragmentPagerAdapter: PagerAdapter {

    private var _fragmentManager: FragmentManager? = null
    private var _curTransaction: FragmentTransaction? = null
    private var _currentPrimaryItem: Fragment? = null

    constructor(fm: FragmentManager?) {
        _fragmentManager = fm
    }

    abstract fun getItem(position: Int): Fragment?

    override fun startUpdate(container: ViewGroup?) { }

    override fun instantiateItem(container: ViewGroup?, position: Int): Any? {
        if (_curTransaction == null) {
            _curTransaction = _fragmentManager?.beginTransaction()
        }

        val itemId = getItemId(position)
        val name = makeFragmentName(container!!.id, itemId)
        var fragment = _fragmentManager?.findFragmentByTag(name)
        if (fragment != null) {
            _curTransaction?.attach(fragment)
        } else {
            fragment = getItem(position)
            _curTransaction?.add(container.id, fragment, makeFragmentName(container.id, itemId))
        }
        if (fragment != _currentPrimaryItem) {
            fragment?.setMenuVisibility(false)
            fragment?.userVisibleHint = false
        }

        return fragment
    }

    override fun destroyItem(container: ViewGroup?, position: Int, obj: Any?) {
        if (_curTransaction == null) {
            _curTransaction = _fragmentManager?.beginTransaction()
        }
        _curTransaction?.detach(obj as Fragment)
    }

    override fun setPrimaryItem(container: ViewGroup?, position: Int, obj: Any?) {
        val fragment = obj as Fragment?
        if (fragment != _currentPrimaryItem) {
            if (_currentPrimaryItem != null) {
                _currentPrimaryItem?.setMenuVisibility(false)
                _currentPrimaryItem?.userVisibleHint = false
            }
            if (fragment != null) {
                fragment.setMenuVisibility(true)
                fragment.userVisibleHint = true
            }
            _currentPrimaryItem = fragment
        }
    }

    override fun finishUpdate(container: ViewGroup?) {
        if (_curTransaction != null) {
            try {
                _curTransaction?.commitAllowingStateLoss()
                _curTransaction = null
                _fragmentManager?.executePendingTransactions()
            } catch (e: Exception) {

            }
        }
    }

    override fun isViewFromObject(v: View?, obj: Any?): Boolean = (obj as Fragment).view == v

    override fun saveState(): Parcelable? = null

    override fun restoreState(state: Parcelable?, loader: ClassLoader?) { }

    fun getItemId(position: Int): Long = position.toLong()

    fun makeFragmentName(viewId: Int, id: Long): String = "android:switcher:${viewId}:${id}"

}