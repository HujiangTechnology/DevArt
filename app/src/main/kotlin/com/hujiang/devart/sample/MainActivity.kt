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
import com.hujiang.devart.utils.UIUtils

class MainActivity : BaseMainActivity() {

    private var _itemShare: MenuItem? = null


    override fun onCreate(savedInstanceState: Bundle?) {
        UIUtils.initDisplayMetrics(this, windowManager, false)
        super.onCreate(savedInstanceState)
    }

    override fun initOnce() {

    }

    override fun getBarTitle(): String? = getString(R.string.app_name)

    override fun customTheme(): Int = 0

    override fun getFragment(currentFragment: Int): Fragment? {
        return when(currentFragment) {
            1 -> Fragments.argFragment
            2 -> Fragments.adapterFragment
            17 -> Fragments.tabFragment
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
