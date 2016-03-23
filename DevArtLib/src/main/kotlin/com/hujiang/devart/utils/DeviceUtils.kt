package com.hujiang.devart.utils

import android.content.Context
import android.content.pm.ApplicationInfo
import android.telephony.TelephonyManager

/**
 * Created by rarnu on 3/22/16.
 */
object DeviceUtils {

    /**
     * required android.permission.READ_PHONE_STATE
     */
    fun getDeviceUniqueId(context: Context): String {
        val tm  = context.getSystemService(Context.TELEPHONY_SERVICE) as TelephonyManager
        return "${tm.deviceId}-${tm.subscriberId}"
    }

    fun getAppVersionCode(context: Context, filePath: String): Int {
        var ret = 0
        try {
            val pi = context.packageManager.getPackageArchiveInfo(filePath, 0)
            ret = pi.versionCode
        } catch(e: Exception) {

        }
        return ret
    }

    fun getAppVersionCode(context: Context, info: ApplicationInfo?): Int {
        var ret = 0
        try {
            var pname = context.packageName
            if (info != null) {
                pname = info.packageName
            }
            val pi = context.packageManager.getPackageInfo(pname, 0)
            ret = pi.versionCode
        } catch(e: Exception) {

        }
        return ret
    }

    fun getAppVersionCode(context: Context): Int {
        return getAppVersionCode(context, null)
    }

    fun getAppVersionName(context: Context, filePath: String): String {
        var ret = ""
        try {
            val pi = context.packageManager.getPackageArchiveInfo(filePath, 0)
            ret = pi.versionName
        } catch(e: Exception) {

        }
        return ret
    }

    fun getAppVersionName(context: Context, info: ApplicationInfo?): String {
        var ret = ""
        try {
            var pname = context.packageName
            if (info != null) {
                pname = info.packageName
            }
            val pi = context.packageManager.getPackageInfo(pname, 0)
            ret = pi.versionName
        } catch(e: Exception) {

        }
        return ret
    }

    fun getAppVersionName(context: Context): String {
        return getAppVersionName(context, null)
    }

}