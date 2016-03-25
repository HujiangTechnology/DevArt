package com.hujiang.devart.sample.fragment

import android.content.Intent
import android.os.Bundle
import android.preference.Preference
import android.view.Menu
import com.hujiang.devart.sample.R
import com.hujiang.devart.base.BasePreferenceFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.SlideActivity

/**
 * Created by rarnu on 3/25/16.
 */
class IndexFragment: BasePreferenceFragment(), Preference.OnPreferenceClickListener {


    private var p_3_5: Preference? = null

    override fun getBarTitle(): Int = R.string.app_name

    override fun getBarTitleWithPath(): Int = R.string.app_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        p_3_5 = findPreference(getString(R.string.id_item_3_5))

    }

    override fun initEvents() {
        p_3_5?.onPreferenceClickListener = this
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
        }
        return true
    }
}