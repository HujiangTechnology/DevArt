package com.hujiang.devart.base.inner

import android.os.Bundle
import android.view.Menu

/**
 * Created by rarnu on 3/23/16.
 */
interface InnerIntf {
    fun getBarTitle(): Int
    fun getBarTitleWithPath(): Int
    fun getCustomTitle(): String?
    fun initComponents()
    fun initEvents()
    fun initLogic()
    fun getFragmentLayoutResId(): Int
    fun getMainActivityName(): String?
    fun initMenu(menu: Menu?)
    fun onGetNewArguments(bn: Bundle?)
    fun getFragmentState(): Bundle?
}