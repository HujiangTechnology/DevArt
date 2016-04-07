package com.hujiang.devart.component.daemon

import android.content.ComponentName
import android.content.Context
import android.content.pm.PackageManager

/**
 * Created by rarnu on 4/7/16.
 */
object PackageUtils {

    fun setComponentDefault(context: Context?, componentClassName: String?){
        val pm = context?.packageManager
        val componentName = ComponentName(context?.packageName, componentClassName)
        pm?.setComponentEnabledSetting(componentName, PackageManager.COMPONENT_ENABLED_STATE_DEFAULT, PackageManager.DONT_KILL_APP)
    }

    fun isComponentDefault(context: Context?, componentClassName: String?): Boolean {
        val pm = context?.packageManager
        val componentName = ComponentName(context?.packageName, componentClassName)
        return pm?.getComponentEnabledSetting(componentName) == PackageManager.COMPONENT_ENABLED_STATE_DEFAULT
    }

}