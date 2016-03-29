package com.hujiang.devart.utils

import android.content.Context
import android.content.pm.ApplicationInfo
import android.telephony.TelephonyManager

/**
 * Created by rarnu on 3/22/16.
 */
object DeviceUtils {

    val RO_BUILD_ID = "ro.build.id"
    val RO_BUILD_VERSION_SDK = "ro.build.version.sdk";
    val RO_BUILD_VERSION_RELEASE = "ro.build.version.release";
    val RO_PRODUCT_MODEL = "ro.product.model";
    val RO_PRODUCT_BRAND = "ro.product.brand";
    val RO_PRODUCT_NAME = "ro.product.name";
    val RO_PRODUCT_DEVICE = "ro.product.device";
    val RO_PRODUCT_BOARD = "ro.product.board";
    val RO_PRODUCT_CPU_ABI = "ro.product.cpu.abi";
    val RO_PRODUCT_CPU_ABI2 = "ro.product.cpu.abi2";
    val RO_PRODUCT_MANUFACTURER = "ro.product.manufacturer";
    val RO_BOARD_PLATFORM = "ro.board.platform";
    val RO_BUILD_DESCRIPTION = "ro.build.description";
    val RO_PRODUCT_VERSION = "ro.product.version";
    val RO_MIUI_UI_VERSION_CODE = "ro.miui.ui.version.code";
    val RO_MIUI_UI_VERSION_NAME = "ro.miui.ui.version.name";

    private val _buildPropFile = "/system/build.prop"
    private var _buildProp: MutableList<String>? = null

    fun getDeviceInfo(): DeviceInfo? {
        if (_buildProp == null) {
            _buildProp = FileUtils.readFile(_buildPropFile)
        }
        val info = DeviceInfo()
        info.roBuildId = findPropValue(RO_BUILD_ID)
        info.roBuildVersionSdk = findPropValue(RO_BUILD_VERSION_SDK)
        info.roBuildVersionRelease = findPropValue(RO_BUILD_VERSION_RELEASE)
        info.roProductModel = findPropValue(RO_PRODUCT_MODEL)
        info.roProductBrand = findPropValue(RO_PRODUCT_BRAND)
        info.roProductName = findPropValue(RO_PRODUCT_NAME)
        info.roProductDevice = findPropValue(RO_PRODUCT_DEVICE)
        info.roProductBoard = findPropValue(RO_PRODUCT_BOARD)
        info.roProductCpuAbi = findPropValue(RO_PRODUCT_CPU_ABI)
        info.roProductCpuAbi2 = findPropValue(RO_PRODUCT_CPU_ABI2)
        info.roProductManufacturer = findPropValue(RO_PRODUCT_MANUFACTURER)
        info.roBoardPlatform = findPropValue(RO_BOARD_PLATFORM)
        info.roBuildDescription = findPropValue(RO_BUILD_DESCRIPTION)
        info.roProductVersion = findPropValue(RO_PRODUCT_VERSION)
        return info
    }

    fun getBuildProp(key: String): String {
        if (_buildProp == null) {
            _buildProp = FileUtils.readFile(_buildPropFile)
        }
        return findPropValue(key)
    }

    private fun findPropValue(key: String): String {
        var tmp: String
        var value = ""
        var idx: Int
        for (s in _buildProp!!) {
            idx = s.indexOf("=")
            if (idx < 0) {
                continue;
            }
            tmp = s.substring(0, idx)
            if (tmp == key) {
                value = s.substring(idx + 1)
                break
            }
        }
        return value
    }

    /**
     * required android.permission.READ_PHONE_STATE
     */
    fun getDeviceUniqueId(context: Context): String {
        val tm = context.getSystemService(Context.TELEPHONY_SERVICE) as TelephonyManager
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

    fun getAppVersionCode(context: Context): Int = getAppVersionCode(context, null)

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

    fun getAppVersionName(context: Context): String = getAppVersionName(context, null)

    class DeviceInfo {
        var roBuildId = ""
        var roBuildVersionSdk = ""
        var roBuildVersionRelease = ""
        var roProductModel = ""
        var roProductBrand = ""
        var roProductName = ""
        var roProductDevice = ""
        var roProductBoard = ""
        var roProductCpuAbi = ""
        var roProductCpuAbi2 = ""
        var roProductManufacturer = ""
        var roBoardPlatform = ""
        var roBuildDescription = ""
        var roProductVersion = ""

        override fun toString(): String = JsonUtils.objectToJsonString(this)!!
    }
}