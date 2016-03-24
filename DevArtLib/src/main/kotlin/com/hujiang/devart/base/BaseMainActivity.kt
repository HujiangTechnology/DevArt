package com.hujiang.devart.base

import android.app.ActionBar
import android.app.Activity
import android.app.Fragment
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.os.Bundle
import android.view.Menu
import android.view.View
import android.view.Window
import com.hujiang.devart.R
import com.hujiang.devart.base.inner.IFragments
import com.hujiang.devart.base.inner.UIInstance
import com.hujiang.devart.utils.DrawableUtils
import com.hujiang.devart.utils.UIUtils

/**
 * Created by rarnu on 3/23/16.
 */
abstract class BaseMainActivity: Activity(), IFragments {

    companion object {
        protected var oneTimeRun = false
        val SYSTEM_REASON = "reason"
        val SYSTEM_HOME_KEY = "homekey"
        val SYSTEM_RECENT_APPS = "recentapps"
    }

    val receiverHome = HomeReceiver()
    val filterHome = IntentFilter(Intent.ACTION_CLOSE_SYSTEM_DIALOGS)

    override fun onCreate(savedInstanceState: Bundle?) {
        if (customTheme() != 0) {
            setTheme(customTheme())
        }
        window.requestFeature(Window.FEATURE_ACTION_BAR)
        super.onCreate(savedInstanceState)
        registerReceiver(receiverHome, filterHome)

        loadFragments()

        if (!oneTimeRun) {
            oneTimeRun = true
            initOneTime()
        }
        loadUI()
    }

    abstract fun initOnce()

    abstract fun getBarTitle(): String?

    abstract fun customTheme(): Int

    abstract fun getFragment(currentFragment: Int): Fragment?

    abstract fun getIndexFragment(): Fragment?

    abstract fun initMenu(menu: Menu?)

    abstract fun onHomeClick()

    abstract fun onRecentAppClick()

    override fun onResume() {
        super.onResume()
        if (!UIInstance.dualPane) {
            actionBar?.navigationMode = ActionBar.NAVIGATION_MODE_STANDARD
        }
    }

    override fun onDestroy() {
        unregisterReceiver(receiverHome)
        releaseFragments()
        oneTimeRun = false
        super.onDestroy()
    }

    private fun initOneTime() {
        initOnce()
    }

    private fun loadUI() {
        setContentView(R.layout.layout_main)

        replaceIndexFragment()
        val vDetail = findViewById(R.id.fragmentDetail)
        UIInstance.dualPane = (vDetail != null && vDetail.visibility == View.VISIBLE)

        val dSysBackground = DrawableUtils.getSystemAttrDrawable(this, DrawableUtils.DETAILS_ELEMENT_BACKGROUND)
        val dBackground = if (UIUtils.isFollowSystemBackground) { dSysBackground } else { null }
        if (UIInstance.dualPane) {
            findViewById(R.id.fragmentMain).background = dBackground
            findViewById(R.id.fragmentDetail).background = dBackground
        } else {
            findViewById(R.id.layoutMain).background = dBackground
        }

        actionBar.title = getBarTitle()
        setDualPane()
    }


    private fun setDualPane() {
        if (UIInstance.dualPane) {
            replaceDetailFragment(getFragment(UIInstance.currentFragment))
        }
    }

    private fun replaceIndexFragment() {
        val fIndex = getIndexFragment()
        fragmentManager.beginTransaction().replace(R.id.fragmentMain, fIndex).commit()
    }

    private fun replaceDetailFragment(f: Fragment?) {
        fragmentManager.beginTransaction().replace(R.id.fragmentDetail, f).commit()
    }

    override fun onCreateOptionsMenu(menu: Menu?): Boolean {
        initMenu(menu)
        return true
    }

    inner class HomeReceiver: BroadcastReceiver() {

        override fun onReceive(context: Context?, intent: Intent?) {
            val action = intent?.action
            if (action != null && action == Intent.ACTION_CLOSE_SYSTEM_DIALOGS) {
                val reason = intent?.getStringExtra(SYSTEM_REASON)
                if (reason != null) {
                    if (reason == SYSTEM_HOME_KEY) {
                        onHomeClick()
                        oneTimeRun = false
                    } else if (reason == SYSTEM_RECENT_APPS) {
                        onRecentAppClick()
                    }
                }
            }
        }

    }
}