package com.hujiang.devart.sample.fragment

import android.content.Intent
import android.os.Bundle
import android.preference.Preference
import android.view.Menu
import com.hujiang.devart.base.BasePreferenceFragment
import com.hujiang.devart.base.common.FragmentStarter
import com.hujiang.devart.base.inner.UIInstance
import com.hujiang.devart.component.mutax.MutaxReceiver
import com.hujiang.devart.component.mutax.OnMutaxMessage
import com.hujiang.devart.sample.*
import com.hujiang.devart.sample.service.DemoService

/**
 * Created by rarnu on 3/25/16.
 */
class IndexFragment : BasePreferenceFragment(), Preference.OnPreferenceClickListener, OnMutaxMessage {

    private var _mutax: MutaxReceiver? = null

    private var _p1Arg1: Preference? = null
    private var _p1Arg2: Preference? = null
    private var _p1Arg3: Preference? = null

    private var _p2Adapter: Preference? = null
    private var _p2Dialog: Preference? = null
    private var _p2Popup: Preference? = null
    private var _p2Mutax: Preference? = null

    private var _p3HScroll: Preference? = null
    private var _p3VScroll: Preference? = null
    private var _p3Gif: Preference? = null
    private var _p3Slide: Preference? = null
    private var _p3Tab: Preference? = null
    private var _p3Float: Preference? = null
    private var _p3PullToRefreshScroll: Preference? = null
    private var _p3PullToRefreshList: Preference? = null
    private var _p3Swipe: Preference? = null
    private var _p3DragList: Preference? = null
    private var _p3Calendar: Preference? = null
    private var _p3LockView: Preference? = null
    private var _p3FlipView: Preference? = null
    private var _p3GlassBar: Preference? = null
    private var _p3DragGrid: Preference? = null
    private var _p3ArcMenu: Preference? = null

    private var _p4Device: Preference? = null
    private var _p4Download: Preference? = null
    private var _p4File: Preference? = null
    private var _p4Json: Preference? = null
    private var _p4Http: Preference? = null
    private var _p4Image: Preference? = null
    private var _p4Network: Preference? = null
    private var _p4Notification: Preference? = null
    private var _p4Zip: Preference? = null
    private var _p4Algorithm: Preference? = null

    private var _p5BlackTech: Preference? = null
//    private var _p5Terminal: Preference? = null

    override fun getBarTitle(): Int = R.string.app_name

    override fun getBarTitleWithPath(): Int = R.string.app_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {

        _mutax = MutaxReceiver(DemoService.DEMO_SERVICE_ACTION, null, null)

        _p1Arg1 = findPreference(getString(R.string.id_item_1_1))
        _p1Arg2 = findPreference(getString(R.string.id_item_1_2))
        _p1Arg3 = findPreference(getString(R.string.id_item_1_3))

        _p2Adapter = findPreference(getString(R.string.id_item_2_1))
        _p2Dialog = findPreference(getString(R.string.id_item_2_2))
        _p2Popup = findPreference(getString(R.string.id_item_2_3))
        _p2Mutax = findPreference(getString(R.string.id_item_2_4))

        _p3HScroll = findPreference(getString(R.string.id_item_3_1))
        _p3VScroll = findPreference(getString(R.string.id_item_3_2))
        _p3Gif = findPreference(getString(R.string.id_item_3_2_1))
        _p3PullToRefreshScroll = findPreference(getString(R.string.id_item_3_3))
        _p3PullToRefreshList = findPreference(getString(R.string.id_item_3_4))
        _p3DragList = findPreference(getString(R.string.id_item_3_4_1))
        _p3Slide = findPreference(getString(R.string.id_item_3_5))
        _p3Tab = findPreference(getString(R.string.id_item_3_7))
        _p3Float = findPreference(getString(R.string.id_item_3_6))
        _p3Swipe = findPreference(getString(R.string.id_item_3_4_4))
        _p3Calendar = findPreference(getString(R.string.id_item_3_8))
        _p3LockView = findPreference(getString(R.string.id_item_3_9))
        _p3FlipView = findPreference(getString(R.string.id_item_3_4_3))
        _p3GlassBar = findPreference(getString(R.string.id_item_3_10))
        _p3DragGrid = findPreference(getString(R.string.id_item_3_11))
        _p3ArcMenu = findPreference(getString(R.string.id_item_3_12))

        _p4Device = findPreference(getString(R.string.id_item_4_1))
        _p4Download = findPreference(getString(R.string.id_item_4_2))
        _p4File = findPreference(getString(R.string.id_item_4_3))
        _p4Json = findPreference(getString(R.string.id_item_4_3_1))
        _p4Http = findPreference(getString(R.string.id_item_4_4))
        _p4Image = findPreference(getString(R.string.id_item_4_5))
        _p4Network = findPreference(getString(R.string.id_item_4_6))
        _p4Notification = findPreference(getString(R.string.id_item_4_7))
        _p4Zip = findPreference(getString(R.string.id_item_4_9))
        _p4Algorithm = findPreference(getString(R.string.id_item_4_10))

        _p5BlackTech = findPreference(getString(R.string.id_item_5_2))
//        _p5Terminal = findPreference(getString(R.string.id_item_5_1))

    }

    override fun initEvents() {

        _mutax?.onReceive = this

        _p1Arg1?.onPreferenceClickListener = this
        _p1Arg2?.onPreferenceClickListener = this
        _p1Arg3?.onPreferenceClickListener = this

        _p2Adapter?.onPreferenceClickListener = this
        _p2Dialog?.onPreferenceClickListener = this
        _p2Popup?.onPreferenceClickListener = this
        _p2Mutax?.onPreferenceClickListener = this

        _p3HScroll?.onPreferenceClickListener = this
        _p3VScroll?.onPreferenceClickListener = this
        _p3Gif?.onPreferenceClickListener = this
        _p3PullToRefreshScroll?.onPreferenceClickListener = this
        _p3PullToRefreshList?.onPreferenceClickListener = this
        _p3DragList?.onPreferenceClickListener = this
        _p3Slide?.onPreferenceClickListener = this
        _p3Tab?.onPreferenceClickListener = this
        _p3Float?.onPreferenceClickListener = this
        _p3Swipe?.onPreferenceClickListener = this
        _p3Calendar?.onPreferenceClickListener = this
        _p3LockView?.onPreferenceClickListener = this
        _p3FlipView?.onPreferenceClickListener = this
        _p3GlassBar?.onPreferenceClickListener = this
        _p3DragGrid?.onPreferenceClickListener = this
        _p3ArcMenu?.onPreferenceClickListener = this

        _p4Device?.onPreferenceClickListener = this
        _p4Download?.onPreferenceClickListener = this
        _p4File?.onPreferenceClickListener = this
        _p4Json?.onPreferenceClickListener = this
        _p4Http?.onPreferenceClickListener = this
        _p4Image?.onPreferenceClickListener = this
        _p4Network?.onPreferenceClickListener = this
        _p4Notification?.onPreferenceClickListener = this
        _p4Zip?.onPreferenceClickListener = this
        _p4Algorithm?.onPreferenceClickListener = this

        _p5BlackTech?.onPreferenceClickListener = this
//        _p5Terminal?.onPreferenceClickListener = this

    }

    override fun initLogic() {

    }

    override fun getFragmentLayoutResId(): Int = R.xml.main

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) {

    }

    override fun onGetNewArguments(bn: Bundle?) {
    }

    override fun getFragmentState(): Bundle? = null

    override fun onPreferenceClick(preference: Preference?): Boolean {
        val key = preference!!.key
        when (key) {
            getString(R.string.id_item_1_1),
            getString(R.string.id_item_1_2),
            getString(R.string.id_item_1_3) -> {
                UIInstance.currentFragment = 1
                val bn = Bundle()
                bn.putString("key", preference.key)
                if (!Fragments.argFragment.isAdded) {
                    FragmentStarter.showContent(activity, ArgumentActivity::class.java, Fragments.argFragment)
                }
                Fragments.argFragment.setNewArguments(bn)
            }
            getString(R.string.id_item_2_1) -> {
                UIInstance.currentFragment = 2
                FragmentStarter.showContent(activity, AdapterActivity::class.java, Fragments.adapterFragment)
            }
            getString(R.string.id_item_2_2) -> startActivity(Intent(activity, DialogActivity::class.java))
            getString(R.string.id_item_2_3) -> startActivity(Intent(activity, PopupActivity::class.java))
            getString(R.string.id_item_2_4) -> {
                val inService = Intent(activity, DemoService::class.java)
                inService.putExtra("command", "service")
                inService.putExtra("id", 101)
                inService.putExtra("title", R.string.service_name)
                inService.putExtra("desc", R.string.service_finished)
                inService.putExtra("procId", 201)
                inService.putExtra("procTitle", R.string.service_name)
                inService.putExtra("procDesc", R.string.service_detail)
                activity.startService(inService)
            }
            getString(R.string.id_item_3_1) -> {
                UIInstance.currentFragment = 3
                FragmentStarter.showContent(activity, HScrollActivity::class.java, Fragments.hscrollFragment)
            }
            getString(R.string.id_item_3_2) -> {
                UIInstance.currentFragment = 4
                FragmentStarter.showContent(activity, VScrollActivity::class.java, Fragments.vscrollFragment)
            }
            getString(R.string.id_item_3_2_1) -> {
                UIInstance.currentFragment = 22
                FragmentStarter.showContent(activity, GifActivity::class.java, Fragments.gifFragment)
            }
            getString(R.string.id_item_3_3) -> {
                UIInstance.currentFragment = 5
                FragmentStarter.showContent(activity, PullToRefreshScrollActivity::class.java, Fragments.pullToRefreshScrollFragment)
            }
            getString(R.string.id_item_3_4) -> {
                UIInstance.currentFragment = 6
                FragmentStarter.showContent(activity, PullToRefreshListActivity::class.java, Fragments.pullToRefreshListFragment)
            }
            getString(R.string.id_item_3_4_1) -> {
                UIInstance.currentFragment = 18
                FragmentStarter.showContent(activity, DragListActivity::class.java, Fragments.dragListFragment)
            }
            getString(R.string.id_item_3_4_3) -> {
                UIInstance.currentFragment = 29
                FragmentStarter.showContent(activity, FlipViewActivity::class.java, Fragments.flipViewFragment)
            }
            getString(R.string.id_item_3_4_4) -> {
                UIInstance.currentFragment = 23
                FragmentStarter.showContent(activity, SwipeActivity::class.java, Fragments.swipeFragment)
            }
            getString(R.string.id_item_3_5) -> startActivity(Intent(activity, SlideActivity::class.java))
            getString(R.string.id_item_3_6) -> {
                UIInstance.currentFragment = 16
                FragmentStarter.showContent(activity, FloatWindowActivity::class.java, Fragments.floatFragment)
            }
            getString(R.string.id_item_3_7) -> {
                UIInstance.currentFragment = 17
                FragmentStarter.showContent(activity, TabActivity::class.java, Fragments.tabFragment)
            }
            getString(R.string.id_item_3_8) -> {
                UIInstance.currentFragment = 27
                FragmentStarter.showContent(activity, CalendarActivity::class.java, Fragments.calendarFragment)
            }
            getString(R.string.id_item_3_9) -> {
                UIInstance.currentFragment = 28
                FragmentStarter.showContent(activity, LockViewActivity::class.java, Fragments.lockViewFragment)
            }
            getString(R.string.id_item_3_10) -> {
                UIInstance.currentFragment = 31
                FragmentStarter.showContent(activity, GlassBarActivity::class.java, Fragments.glassBarFragment)
            }
            getString(R.string.id_item_3_11) -> {
                UIInstance.currentFragment = 32
                FragmentStarter.showContent(activity, DragGridActivity::class.java, Fragments.dragGridFragment)
            }
            getString(R.string.id_item_3_12) -> {
                UIInstance.currentFragment = 33
                FragmentStarter.showContent(activity, ArcMenuActivity::class.java, Fragments.arcMenuFragment)
            }
            getString(R.string.id_item_4_1) -> {
                UIInstance.currentFragment = 7
                FragmentStarter.showContent(activity, DeviceActivity::class.java, Fragments.deviceFragment)
            }
            getString(R.string.id_item_4_2) -> {
                UIInstance.currentFragment = 8
                FragmentStarter.showContent(activity, DownloadActivity::class.java, Fragments.downloadFragment)
            }
            getString(R.string.id_item_4_3) -> {
                UIInstance.currentFragment = 9
                FragmentStarter.showContent(activity, FileActivity::class.java, Fragments.fileFragment)
            }
            getString(R.string.id_item_4_3_1) -> {
                UIInstance.currentFragment = 21
                FragmentStarter.showContent(activity, JsonActivity::class.java, Fragments.jsonFragment)
            }
            getString(R.string.id_item_4_4) -> {
                UIInstance.currentFragment = 10
                FragmentStarter.showContent(activity, HttpActivity::class.java, Fragments.httpFragment)
            }
            getString(R.string.id_item_4_5) -> {
                UIInstance.currentFragment = 11
                FragmentStarter.showContent(activity, ImageActivity::class.java, Fragments.imageFragment)
            }
            getString(R.string.id_item_4_6) -> {
                UIInstance.currentFragment = 12
                FragmentStarter.showContent(activity, NetworkActivity::class.java, Fragments.networkFragment)
            }
            getString(R.string.id_item_4_7) -> {
                UIInstance.currentFragment = 13
                FragmentStarter.showContent(activity, NotificationActivity::class.java, Fragments.notificationFragment)
            }
            getString(R.string.id_item_4_9) -> {
                UIInstance.currentFragment = 24
                FragmentStarter.showContent(activity, ZipActivity::class.java, Fragments.zipFragment)
            }
            getString(R.string.id_item_4_10) -> {
                UIInstance.currentFragment = 26
                FragmentStarter.showContent(activity, AlgorithmActivity::class.java, Fragments.algorithmFragment)
            }
            getString(R.string.id_item_5_1) -> {
                UIInstance.currentFragment = 30
                FragmentStarter.showContent(activity, TerminalActivity::class.java, Fragments.terminalFragment)
            }
            getString(R.string.id_item_5_2) -> {
                UIInstance.currentFragment = 25
                FragmentStarter.showContent(activity, BlackTechActivity::class.java, Fragments.blackTechFragment)
            }
        }
        return true
    }

    override fun onResume() {
        super.onResume()
        _mutax?.register(activity)
    }

    override fun onPause() {
        _mutax?.unregister(activity)
        super.onPause()
    }

    override fun onMutaxStateChange(operating: Boolean) {
        if (!operating) {
            val inCleanBackupService = Intent(activity, DemoService::class.java)
            activity.stopService(inCleanBackupService)
        }
        _p2Mutax?.isEnabled = !operating
    }

    override fun onMutaxProgress(name: String, position: Int, total: Int) { }

    override fun onMutaxMessage(operating: Boolean) { }
}