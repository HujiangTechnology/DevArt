package com.hujiang.devart.base

import android.app.Fragment
import android.app.FragmentManager
import android.app.FragmentTransaction
import android.os.Bundle
import android.os.Parcelable
import android.support.v4.view.PagerAdapter
import android.view.View
import android.view.ViewGroup

/**
 * Created by rarnu on 3/24/16.
 */
abstract class FragmentStatePagerAdapter: PagerAdapter {

    private var _fragmentManager: FragmentManager? = null
    private var _curTransaction: FragmentTransaction? = null
    private var _savedState = arrayListOf<Fragment.SavedState?>()
    private var _fragments = arrayListOf<Fragment?>()
    private var _currentPrimaryItem: Fragment? = null

    constructor(fm: FragmentManager?) {
        _fragmentManager = fm
    }

    abstract fun getItem(position: Int): Fragment?

    override fun startUpdate(container: ViewGroup?) { }

    override fun instantiateItem(container: ViewGroup?, position: Int): Any? {
        if (_fragments.size > position) {
            val f= _fragments[position]
            if (f != null) {
                return f
            }
        }
        if (_curTransaction == null) {
            _curTransaction = _fragmentManager?.beginTransaction()
        }
        val fragment = getItem(position)

        if (_savedState.size > position) {
            val fss = _savedState[position]
            if (fss != null) {
                fragment?.setInitialSavedState(fss)
            }
        }
        while (_fragments.size <= position) {
            _fragments.add(null)
        }
        fragment?.setMenuVisibility(false)
        fragment?.userVisibleHint = false
        _fragments[position] = fragment
        _curTransaction?.add(container!!.id, fragment)
        return fragment
    }

    override fun destroyItem(container: ViewGroup?, position: Int, obj: Any?) {
        val fragment = obj as Fragment?
        if (_curTransaction == null) {
            _curTransaction = _fragmentManager?.beginTransaction()
        }
        while (_savedState.size <= position) {
            _savedState.add(null)
        }
        try {
            _savedState[position] = _fragmentManager?.saveFragmentInstanceState(fragment)
        } catch (e: Exception) {

        }
        _fragments[position] = null
        _curTransaction?.remove(fragment)
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

    override fun saveState(): Parcelable? {
        var state: Bundle? = null
        if (_savedState.size > 0) {
            state = Bundle()
            val fss = arrayOfNulls<Fragment.SavedState>(_savedState.size)
            _savedState.toArray(fss)
            state.putParcelableArray("states", fss)
        }
        for (i in 0.._fragments.size - 1) {
            val f = _fragments[i]
            if (f != null) {
                if (state == null) {
                    state = Bundle()
                }
                val key  = "f${i}"
                _fragmentManager?.putFragment(state, key, f)
            }
        }
        return state
    }

    override fun restoreState(state: Parcelable?, loader: ClassLoader?) {
        if (state != null) {
            val bundle = state as Bundle?
            bundle?.classLoader = loader
            val fss = bundle?.getParcelableArray("states")
            _savedState.clear()
            _fragments.clear()
            if (fss != null) {
                for (i in 0..fss.size - 1) {
                    _savedState.add(fss[i] as Fragment.SavedState?)
                }
            }
            val keys = bundle?.keySet()!!
            for (key in keys) {
                if (key.startsWith("f")) {
                    val index = key.substring(1).toInt()
                    val f = _fragmentManager?.getFragment(bundle, key)
                    if (f != null) {
                        while (_fragments.size <= index) {
                            _fragments.add(null)
                        }
                        f.setMenuVisibility(false)
                        _fragments[index] = f
                    }
                }
            }
        }
    }
}

