package com.hujiang.devart.base.inner

import android.os.Bundle
import android.preference.PreferenceFragment
import android.view.Menu
import android.view.MenuInflater

/**
 * Created by rarnu on 3/23/16.
 */
abstract class InnerPreferenceFragment: PreferenceFragment, InnerIntf {

    var tabTitle: String? = null
    protected var innerBundle: Bundle? = null

    constructor(): super()
    constructor(tabTitle: String): super() {
        this.tabTitle = tabTitle
    }

    override fun onActivityCreated(savedInstanceState: Bundle?) {
        super.onActivityCreated(savedInstanceState)
        innerBundle = arguments
        initComponents()
        initEvents()
        initLogic()
    }

    open fun setNewArguments(bn: Bundle?) {
        innerBundle = arguments
        onGetNewArguments(bn)
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setHasOptionsMenu(true)
        addPreferencesFromResource(getFragmentLayoutResId())

        if (activity.actionBar != null) {
            if (getCustomTitle() == null || getCustomTitle().equals("")) {
                if (UIInstance.dualPane) {
                    activity.actionBar.setTitle(getBarTitleWithPath())
                } else {
                    activity.actionBar.setTitle(getBarTitle())
                }
            } else {
                activity.actionBar.title = getCustomTitle()
            }
        }
    }

    override fun onCreateOptionsMenu(menu: Menu?, inflater: MenuInflater?) {
        if (activity == null) {
            return
        }
        if (activity.javaClass.name == getMainActivityName() && !UIInstance.dualPane) {
            return
        }
        initMenu(menu)
    }
}