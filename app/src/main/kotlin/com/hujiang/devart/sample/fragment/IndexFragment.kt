package com.hujiang.devart.sample.fragment

import android.content.Intent
import android.os.Bundle
import android.preference.Preference
import android.view.Menu
import com.hujiang.devart.base.BasePreferenceFragment
import com.hujiang.devart.base.common.FragmentStarter
import com.hujiang.devart.base.inner.UIInstance
import com.hujiang.devart.sample.*

/**
 * Created by rarnu on 3/25/16.
 */
class IndexFragment: BasePreferenceFragment(), Preference.OnPreferenceClickListener {

    private var p3Slide: Preference? = null
    private var p3Tab: Preference? = null

    override fun getBarTitle(): Int = R.string.app_name

    override fun getBarTitleWithPath(): Int = R.string.app_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        p3Slide = findPreference(getString(R.string.id_item_3_5))
        p3Tab = findPreference(getString(R.string.id_item_3_7))
    }

    override fun initEvents() {
        p3Slide?.onPreferenceClickListener = this
        p3Tab?.onPreferenceClickListener = this
    }

    override fun initLogic() {

    }

    override fun getFragmentLayoutResId(): Int = R.xml.main

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) {

    }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onPreferenceClick(preference: Preference?): Boolean {
        val key = preference!!.key
        when(key) {
            getString(R.string.id_item_3_5) -> startActivity(Intent(activity, SlideActivity::class.java))
            getString(R.string.id_item_3_7) -> {
                UIInstance.currentFragment = 17
                FragmentStarter.showContent(activity, TabActivity::class.java, Fragments.tabFragment)
            }
        }
        return true
    }
}