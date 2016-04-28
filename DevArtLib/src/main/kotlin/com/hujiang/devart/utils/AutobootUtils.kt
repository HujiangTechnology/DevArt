package com.hujiang.devart.utils

import android.Manifest
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.content.pm.PackageInfo
import android.content.pm.PackageManager

/**
 * Created by rarnu on 4/8/16.
 */
object AutobootUtils {

    fun getAutobootApps(context: Context): MutableList<AutobootInfo?>? {
        var list = arrayListOf<AutobootInfo?>()

        val pm = context.packageManager
        var apps: MutableList<PackageInfo?>? = null
        try {
            apps = pm.getInstalledPackages(0)
        } catch (e: Exception) {
        }
        if (apps != null && apps.size != 0) {
            for (pi in apps) {
                if (pm.checkPermission(Manifest.permission.RECEIVE_BOOT_COMPLETED, pi?.packageName) == PackageManager.PERMISSION_GRANTED) {
                    val status = isEnabled(context, pi)
                    if (status != -1) {
                        val info = AutobootInfo()
                        info.info = pi
                        info.enabled = (status != 0)
                        list.add(info)
                    }
                }
            }
        }
        return list
    }

    fun switchAutoboot(context: Context, info: AutobootInfo?, enabled: Boolean): Boolean {
        var ret = true
        val /* PackageParser.Package */ pkg = ComponentUtils.parsePackageInfo(info?.info)
        val receivers = PackageParserUtils.packageReceivers(pkg)
        for (/* PackageParser.Activity */ riobj in receivers!!) {
            val ri = PackageParserUtils.Activity.fromComponent(riobj)
            if (ri!!.intents != null && ri.intents!!.size != 0) {
                for (/* PackageParser.ActivityIntentInfo */ aiiobj in ri.intents!!) {
                    val aii = aiiobj as IntentFilter
                    if (aii.countActions() > 0) {
                        for (i in 0..aii.countActions() - 1) {
                            if (aii.getAction(i) == Intent.ACTION_BOOT_COMPLETED) {
                                var operatingResult: Boolean
                                if (enabled) {
                                    operatingResult = ComponentUtils.enabledComponent(context, ri.getComponentName())
                                } else {
                                    operatingResult = ComponentUtils.disableComponent(context, ri.getComponentName())
                                }
                                if (!operatingResult) {
                                    ret = false
                                }
                                break
                            }
                        }
                    }
                }
            }
        }
        return ret
    }

    private fun isEnabled(context: Context, info: PackageInfo?): Int {
        var ret = -1
        try {
            val /* PackageParser.Package */ pkg = ComponentUtils.parsePackageInfo(info)
            val receivers = PackageParserUtils.packageReceivers(pkg)
            for (/* PackageParser.Activity */ riobj in receivers!!) {
                val ri = PackageParserUtils.Activity.fromComponent(riobj)
                if (ri!!.intents != null && ri.intents!!.size != 0) {
                    for (/* PackageParser.ActivityIntentInfo */ aiiobj in ri.intents!!) {
                        val aii = aiiobj as IntentFilter
                        if (aii.countActions() > 0) {
                            for (i in 0..aii.countActions() - 1) {
                                if (aii.getAction(i) == Intent.ACTION_BOOT_COMPLETED) {
                                    val cn = ri.getComponentName()
                                    ret = if (context.packageManager.getComponentEnabledSetting(cn) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED) 1 else 0
                                    break
                                }
                            }
                        }
                    }

                }
            }
        } catch (e: Exception) {

        }
        return ret
    }

    class AutobootInfo {
        var info: PackageInfo? = null
        var enabled = false
        var processing = false
    }

}