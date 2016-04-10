package com.hujiang.devart.utils

import android.app.ActivityManager
import android.content.ComponentName
import android.content.Context
import android.content.IntentFilter
import android.content.pm.ApplicationInfo
import android.content.pm.PackageInfo
import android.content.pm.PackageManager
import com.hujiang.devart.command.Command

/**
 * Created by rarnu on 4/8/16.
 */
object ComponentUtils {

    fun enabledComponent(context: Context, receiverName: ComponentName?): Boolean {
        try {
            Command.runCommand("pm enable '${receiverName?.packageName}/${receiverName?.className}'", true, null)
            return context.packageManager.getComponentEnabledSetting(receiverName) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
        } catch (t: Throwable) {
            return false
        }
    }

    fun disableComponent(context: Context, receiverName: ComponentName?): Boolean {
        try {
            Command.runCommand("pm disable '${receiverName?.packageName}/${receiverName?.className}'", true, null)
            return context.packageManager.getComponentEnabledSetting(receiverName) == PackageManager.COMPONENT_ENABLED_STATE_DISABLED
        } catch (t: Throwable) {
            return false
        }
    }

    fun parsePackageInfo(info: PackageInfo?): Any? /* PackageParser.Package */ {
        val fileAbsPath = info?.applicationInfo?.publicSourceDir
        val ppu = PackageParserUtils(fileAbsPath)
        val pkg = ppu.parsePackage(fileAbsPath, PackageParserUtils.PARSE_IS_SYSTEM)
        return pkg
    }

    fun getPackageRSList(context: Context, /* PackageParser.Package */ pkg: Any?): MutableList<CompInfo?>? {
        val lstComponentInfo = arrayListOf<CompInfo?>()
        val pm = context.packageManager
        val lstReceiver = PackageParserUtils.packageReceivers(pkg)
        for (/* PackageParser.Activity */ a in lstReceiver!!) {
            val aa = PackageParserUtils.Activity.fromComponent(a)
            val info = CompInfo()
            info.component = aa
            info.fullPackageName = aa?.getComponentName()?.className
            info.enabled = pm.getComponentEnabledSetting(aa?.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
            lstComponentInfo.add(info)
        }
        val lstService = PackageParserUtils.packageServices(pkg)
        for (/* PackageParser.Service */ s in lstService!!) {
            val ss = PackageParserUtils.Service.fromComponent(s)
            val info = CompInfo()
            info.component = ss
            info.fullPackageName = ss?.getComponentName()?.className
            info.enabled = pm.getComponentEnabledSetting(ss?.getComponentName()) != PackageManager.COMPONENT_ENABLED_STATE_DISABLED
            lstComponentInfo.add(info)
        }
        return lstComponentInfo
    }

    fun getPackageComponentName(/* PackageParser.Component<?> */ comp: Any?): ComponentName? {
        return (comp as PackageParserUtils.Component).getComponentName()
    }

    fun isServiceRunning(context: Context, className: String?): Boolean {
        var isRunning = false
        val activityManager = context.getSystemService(Context.ACTIVITY_SERVICE) as ActivityManager
        val serviceList = activityManager.getRunningServices(30)
        if (serviceList.size <= 0) {
            return false
        }
        for (si in serviceList) {
            if (si.service.className == className) {
                isRunning = true
                break
            }
        }
        return isRunning
    }


    class CompInfo {

        var component: PackageParserUtils.Component? = null /* PackageParser.Component<?> */
        var enabled = false
        var position = 0
        var fullPackageName: String? = null

        fun getCompName(): String? = component?.className?.substringAfterLast(".")

        fun isActivity(): Boolean = component is PackageParserUtils.Activity

        fun appendIntents(str: String?): String? {
            var nstr = str
            val pa = component as PackageParserUtils.Activity
            if (pa.intents != null) {
                if (pa.intents!!.size > 0) {
                    for (aobj in pa.intents!!) {
                        val aii = aobj as IntentFilter
                        if (aii.countActions() > 0) {
                            for (i in 0..aii.countActions() - 1) {
                                nstr += aii.getAction(i).substringAfterLast(".").replace("_", "").toLowerCase() + "<br />"
                            }
                        }
                    }
                }
            }
            return nstr
        }

        fun isServiceRunning(context: Context): Boolean {
            var ret = false
            if (!isActivity()) {
                ret = ComponentUtils.isServiceRunning(context, component?.className)
            }
            return ret
        }
    }

    class EnableappInfo {
        var info: ApplicationInfo? = null
        var log: String? = null

        // 0: succ | 1: exists | 2: fail
        var logId = -1
        var enabled = false

        // 0: system | 1:data | 2: internal | 3: private
        var type = -1
        var filePath: String? = null
    }


}