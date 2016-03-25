package com.hujiang.devart.base

import android.content.res.Configuration
import com.hujiang.devart.R
import com.hujiang.devart.base.inner.InnerActivity

/**
 * Created by rarnu on 3/23/16.
 */
abstract class BaseActivity: InnerActivity() {

    override fun getCloseCondition(): Boolean = resources.configuration.orientation == Configuration.ORIENTATION_LANDSCAPE

    override fun getBaseLayout(): Int = R.layout.layout_replacement

    override fun getReplaceId(): Int = R.id.fReplacement

}