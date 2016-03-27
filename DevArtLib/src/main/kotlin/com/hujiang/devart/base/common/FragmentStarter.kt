package com.hujiang.devart.base.common

import android.app.Activity
import android.app.Fragment
import android.app.FragmentTransaction
import android.content.Intent
import com.hujiang.devart.R
import com.hujiang.devart.base.inner.UIInstance

/**
 * Created by rarnu on 3/25/16.
 */
object FragmentStarter {

    fun showContent(activity: Activity, clz: Class<*>, fContent: Fragment?) {
        if (UIInstance.dualPane) {
            activity.fragmentManager.beginTransaction().replace(R.id.fragmentDetail, fContent).setTransition(FragmentTransaction.TRANSIT_FRAGMENT_FADE).commit()
        } else {
            activity.startActivity(Intent(activity, clz))
        }
    }
}