package com.hujiang.alg.sample

import android.app.ActionBar
import android.app.Activity
import android.os.Bundle
import android.view.MenuItem

/**
 * Created by rarnu on 7/26/16.
 */
abstract class BaseActivity: Activity() {

    abstract fun getLayoutId(): Int

    abstract fun init()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(getLayoutId())

        actionBar.setDisplayOptions(0, ActionBar.DISPLAY_HOME_AS_UP)
        actionBar.setDisplayHomeAsUpEnabled(true)

        init()
    }

    override fun onOptionsItemSelected(item: MenuItem?): Boolean {
        if (item!!.itemId == android.R.id.home) {
            finish()
            return true
        }
        return super.onOptionsItemSelected(item)
    }

}