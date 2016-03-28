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

    private var _p3Slide: Preference? = null
    private var _p3Tab: Preference? = null

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

        _p3Slide = findPreference(getString(R.string.id_item_3_5))
        _p3Tab = findPreference(getString(R.string.id_item_3_7))
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

        _p3Slide?.onPreferenceClickListener = this
        _p3Tab?.onPreferenceClickListener = this
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
            getString(R.string.id_item_3_5) -> startActivity(Intent(activity, SlideActivity::class.java))
            getString(R.string.id_item_3_7) -> {
                UIInstance.currentFragment = 17
                FragmentStarter.showContent(activity, TabActivity::class.java, Fragments.tabFragment)
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