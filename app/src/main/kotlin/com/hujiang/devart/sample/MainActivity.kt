package com.hujiang.devart.sample

import android.app.Fragment
import android.content.Intent
import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.widget.ShareActionProvider
import com.hujiang.devart.base.BaseMainActivity
import com.hujiang.devart.sample.fragment.IndexFragment
import com.hujiang.devart.sample.fragment.IntroFragment
import com.hujiang.devart.sample.service.DaemonService1
import com.hujiang.devart.utils.NetworkUtils
import com.hujiang.devart.utils.UIUtils

class MainActivity : BaseMainActivity() {

    private var _itemShare: MenuItem? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        UIUtils.initDisplayMetrics(this, windowManager, false)
        super.onCreate(savedInstanceState)
        startService(Intent(this, DaemonService1::class.java))
    }

    override fun initOnce() {
        NetworkUtils.doGetNetworkInfoT(this)
    }

    override fun getBarTitle(): String? = getString(R.string.app_name)

    override fun customTheme(): Int = 0

    override fun getFragment(currentFragment: Int): Fragment? {
        return when(currentFragment) {
            1 -> Fragments.argFragment
            2 -> Fragments.adapterFragment
            3 -> Fragments.hscrollFragment
            4 -> Fragments.vscrollFragment
            5 -> Fragments.pullToRefreshScrollFragment
            6 -> Fragments.pullToRefreshListFragment
            7 -> Fragments.deviceFragment
            8 -> Fragments.downloadFragment
            9 -> Fragments.fileFragment
            10 -> Fragments.httpFragment
            11 -> Fragments.imageFragment
            12 -> Fragments.networkFragment
            13 -> Fragments.notificationFragment
            16 -> Fragments.floatFragment
            17 -> Fragments.tabFragment
            18 -> Fragments.dragListFragment
            21 -> Fragments.jsonFragment
            22 -> Fragments.gifFragment
            23 -> Fragments.swipeFragment
            24 -> Fragments.zipFragment
            25 -> Fragments.blackTechFragment
            26 -> Fragments.algorithmFragment
            27 -> Fragments.calendarFragment
            28 -> Fragments.lockViewFragment
            29 -> Fragments.flipViewFragment
            30 -> Fragments.terminalFragment
            31 -> Fragments.glassBarFragment
            32 -> Fragments.dragGridFragment
            33 -> Fragments.arcMenuFragment
            else -> Fragments.introFragment
        }
    }

    override fun getIndexFragment(): Fragment? = Fragments.indexFragment

    override fun initMenu(menu: Menu?) {
        _itemShare = menu?.add(0, 1, 99, "Share")
        _itemShare?.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM)
        _itemShare?.setIcon(android.R.drawable.ic_menu_share)
        val actionProvider = ShareActionProvider(this)
        _itemShare?.actionProvider = actionProvider
        actionProvider.setShareHistoryFileName(ShareActionProvider.DEFAULT_SHARE_HISTORY_FILE_NAME)
        actionProvider.setShareIntent(createShareIntent())
    }

    private fun createShareIntent(): Intent? {
        val shareIntent = Intent(Intent.ACTION_SEND)
        shareIntent.type = "text/*"
        shareIntent.putExtra(Intent.EXTRA_TEXT, "share body")
        shareIntent.putExtra(Intent.EXTRA_SUBJECT, "share title")
        return shareIntent
    }

    override fun onHomeClick() { }

    override fun onRecentAppClick() { }

    override fun loadFragments() { }

    override fun releaseFragments() { }


}
