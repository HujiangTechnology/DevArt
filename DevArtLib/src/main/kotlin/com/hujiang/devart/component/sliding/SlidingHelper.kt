package com.hujiang.devart.component.sliding

import android.app.Activity
import android.os.Bundle
import android.os.Handler
import android.view.KeyEvent
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/24/16.
 */
class SlidingHelper {

    private var _activity: Activity
    private var _slidingMenu: SlidingMenu? = null
    var slidingMenu: SlidingMenu? = null
        get() = _slidingMenu
    private var _viewAbove: View? = null
    private var _viewBehind: View? = null
    private var _broadcasting = false
    private var _onPostCreateCalled = false
    private var _enableSlide = true

    constructor(activity: Activity) {
        _activity = activity
    }

    fun onCreate(savedInstanceState: Bundle?) {
        _slidingMenu = LayoutInflater.from(_activity).inflate(R.layout.layout_sliding_menu, null) as SlidingMenu
    }

    fun onPostCreate(savedInstanceState: Bundle?) {
        if (_viewBehind == null || _viewAbove == null) {
            throw IllegalStateException("Both setBehindContentView must be called in onCreate in addition to setContentView.")
        }
        _onPostCreateCalled = true

        _slidingMenu?.attachToActivity(_activity, if (_enableSlide) {
            SlidingMenu.SLIDING_WINDOW
        } else {
            SlidingMenu.SLIDING_CONTENT
        })

        var open: Boolean
        var secondary: Boolean
        if (savedInstanceState != null) {
            open = savedInstanceState.getBoolean("SlidingActivityHelper.open")
            secondary = savedInstanceState.getBoolean("SlidingActivityHelper.secondary")
        } else {
            open = false
            secondary = false
        }
        Handler().post {
            if (open) {
                if (secondary) {
                    _slidingMenu?.showSecondaryMenu(false)
                } else {
                    _slidingMenu?.showMenu(false)
                }
            } else {
                _slidingMenu?.showContent(false)
            }
        }
    }

    fun setSlidingActionBarEnabled(slidingActionBarEnabled: Boolean) {
        if (_onPostCreateCalled) {
            throw IllegalStateException("enableSlidingActionBar must be called in onCreate.")
        }
        _enableSlide = slidingActionBarEnabled
    }

    fun findViewById(id: Int): View? = _slidingMenu?.findViewById(id)

    fun onSaveInstanceState(outState: Bundle?) {
        outState?.putBoolean("SlidingActivityHelper.open", _slidingMenu!!.isMenuShowing)
        outState?.putBoolean("SlidingActivityHelper.secondary", _slidingMenu!!.isSecondaryMenuShowing)
    }

    fun registerAboveContentView(v: View?, params: ViewGroup.LayoutParams?) {
        if (!_broadcasting) {
            _viewAbove = v
        }
    }

    fun setContentView(v: View?) {
        _broadcasting = true
        _activity.setContentView(v)
    }

    fun setBehindContentView(v: View?, layoutParams: ViewGroup.LayoutParams?) {
        _viewBehind = v
        _slidingMenu!!.behindMenu = _viewBehind
    }

    fun toggle() = _slidingMenu!!.toggle()

    fun showContent() = _slidingMenu!!.showContent()

    fun showMenu() = _slidingMenu!!.showMenu()

    fun showSecondaryMenu() = _slidingMenu!!.showSecondaryMenu()

    fun onKeyUp(keyCode: Int, event: KeyEvent?): Boolean {
        if (keyCode == KeyEvent.KEYCODE_BACK && _slidingMenu!!.isMenuShowing) {
            showContent()
            return true
        }
        return false
    }
}