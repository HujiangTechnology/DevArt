package com.hujiang.devart.utils

import android.app.ActivityManager
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.content.pm.ApplicationInfo
import android.content.pm.PackageInfo
import android.content.pm.PackageManager
import android.content.res.Resources
import android.graphics.drawable.Drawable
import android.net.Uri
import com.hujiang.devart.R
import com.hujiang.devart.command.Command
import com.hujiang.devart.command.OnCommandExecutionListener
import com.hujiang.devart.security.SignatureUtils
import java.io.File
import kotlin.concurrent.thread

/**
 * Created by rarnu on 4/8/16.
 */
object ApkUtils {

    val INSTALL_AUTO = 0
    val INSTALL_INTERNAL = 1
    val INSTALL_SDCARD = 2

    fun getLauncherPackageName(context: Context): MutableList<String?>? {
        var ret: MutableList<String?>? = null
        val inLauncher = Intent(Intent.ACTION_MAIN)
        inLauncher.addCategory(Intent.CATEGORY_HOME)
        val list = context.packageManager.queryIntentActivities(inLauncher, 0)
        if (list != null && list.size != 0) {
            ret = arrayListOf<String?>()
            for (ri in list) {
                if (ri.activityInfo != null) {
                    ret.add(ri.activityInfo.packageName)
                }
            }
        }
        return ret
    }

    fun getTopPackage(context: Context): String? {
        val am = context.getSystemService(Context.ACTIVITY_SERVICE) as ActivityManager
        var ret: String? = null
        try {
            val cn = am.getRunningTasks(1)[0].topActivity
            ret = cn.packageName
        } catch (e: Exception) {

        }
        return ret
    }

    fun getSystemApps(context: Context): MutableList<SysappInfo?>? {
        val res = arrayListOf<SysappInfo?>()
        try {
            val packs = context.packageManager.getInstalledPackages(0)
            var position = 0
            if (packs != null && packs.size != 0) {
                for (p in packs) {
                    val newInfo = p.applicationInfo ?: continue
                    if (newInfo.sourceDir.contains("/system/app") || newInfo.sourceDir.contains("/system/priv-app")) {
                        val info = SysappInfo()
                        info.info = newInfo
                        info.position = position
                        res.add(info)
                        position++
                    }
                }
            }
        } catch (e: Exception) {
        }
        return res
    }

    fun getAppSize(path: String): String? {
        var fileLen = FileUtils.getFileSize(path)
        val odexPath = path.substring(0, path.length - 3) + "odex"
        val fOdex = File(odexPath)
        if (fOdex.exists()) {
            fileLen += FileUtils.getFileSize(odexPath)
        }
        return FileUtils.getReadableFileSize(fileLen)
    }

    fun getDataSize(path: String): String? {
        var ret = ""
        val result = Command.runCommand("busybox du -s $path", true, null)
        if (result.error == "") {
            ret = result.result
            try {
                ret = ret.substringBefore('\t')
            } catch (e: Exception) {
                ret = "unknown"
            }
        }
        return ret
    }

    fun installApp(filePath: String): Boolean {
        val result = Command.runCommand("pm install -r $filePath", true)
        return result.result.toLowerCase().contains("success")
    }

    fun installAppWithResult(filePath: String): String {
        val result = Command.runCommand("pm install -r $filePath", true)
        return result.error
    }

    fun getIconFromPackage(context: Context, info: ApplicationInfo): Drawable? = getIconFromPackageFile(context, info)

    fun getIconFromPackage(context: Context, archiveFilePath: String): Drawable? {
        val ppu = PackageParserUtils(archiveFilePath)
        val /* PackageParser.Package */ pkg = ppu.parsePackage(archiveFilePath, 0) ?: return null
        val info = PackageParserUtils.packageApplicationInfo(pkg)
        val pRes = context.resources
        val assmgr = context.assets
        val amu = AssetManagerUtils(assmgr)
        amu.addAssetPath(archiveFilePath)
        val res = Resources(assmgr, pRes.displayMetrics, pRes.configuration)
        if (info!!.icon != 0) {
            val icon = res.getDrawable(info.icon)
            return icon
        } else {
            return null
        }
    }

    fun getIconFromPackageFile(context: Context, info: ApplicationInfo): Drawable? {
        var res = context.resources
        val assmgr = context.assets
        val amu = AssetManagerUtils(assmgr)
        amu.addAssetPath(info.sourceDir)
        res = Resources(assmgr, res.displayMetrics, res.configuration)
        try {
            if (info.icon != 0) {
                return res.getDrawable(info.icon)
            } else {
                return null
            }
        } catch (e: Exception) {
            return null
        }
    }

    fun getLabelFromPackage(context: Context, info: ApplicationInfo): String? = getLabelFromPackageFile(context, info)

    fun getLabelFromPackageFile(context: Context, info: ApplicationInfo): String? {
        var res = context.resources
        val assetMag = context.assets
        val amu = AssetManagerUtils(assetMag)
        amu.addAssetPath(info.sourceDir)
        res = Resources(assetMag, res.displayMetrics, res.configuration)
        try {
            if (info.labelRes != 0) {
                return res.getText(info.labelRes).toString()
            } else {
                return null
            }
        } catch (e: Exception) {
            return null
        }
    }

    fun getLabelFromPackage2(context: Context, info: ApplicationInfo): String? {
        var res = context.resources
        val assetMag = context.assets
        val amu = AssetManagerUtils(assetMag)
        amu.addAssetPath(info.sourceDir)
        res = Resources(assetMag, res.displayMetrics, res.configuration)
        if (info.labelRes != 0) {
            return res.getText(info.labelRes).toString()
        } else {
            return info.packageName
        }
    }

    fun getAppInfoFromPackage(filePath: String): ApplicationInfo? {
        var info: ApplicationInfo? = null
        val /* PackageParser.Package */ pkg = getPackageInfoFromPackage(filePath, false)
        if (pkg != null) {
            info = PackageParserUtils.packageApplicationInfo(pkg)
        }
        return info
    }

    fun getPackageInfoFromPackage(filePath: String, collectSignature: Boolean): Any? /* PackageParser.Package */ {
        val ppu = PackageParserUtils(filePath)
        val /* PackageParser.Package */ pkg = ppu.parsePackage(filePath, 0)
        if (pkg != null && collectSignature) {
            ppu.packageCollectCertificates(pkg, 0)
        }
        return pkg
    }

    fun getInstalledApps(context: Context, includeSystem: Boolean): MutableList<DataappInfo?>? {
        val res = arrayListOf<DataappInfo?>()
        var packs: List<PackageInfo?>? = null
        try {
            packs = context.packageManager.getInstalledPackages(0)
        } catch (e: Exception) {

        }
        var position = 0
        if (packs != null && packs.size != 0) {
            for (p in packs) {
                val newInfo = p?.applicationInfo ?: continue
                if ((includeSystem && (newInfo.sourceDir.contains("/system/app/") || (newInfo.sourceDir.contains("/system/priv-app/")))) || newInfo.sourceDir.contains("/data/app/")) {
                    val info = DataappInfo()
                    info.info = newInfo
                    info.checked = false
                    info.position = position
                    info.installed = false
                    res.add(info)
                    position++
                }
            }
        }
        return res
    }

    fun uninstallApk(packageName: String): Boolean {
        try {
            val cmdRet = Command.runCommand("pm uninstall $packageName", true, null)
            return cmdRet.error == ""
        } catch (e: Exception) {
            return false
        }
    }

    fun applicationInstalled(context: Context, namespace: String): Boolean {
        try {
            val info = context.packageManager.getPackageInfo(namespace, 0)
            return info != null
        } catch (e: PackageManager.NameNotFoundException) {
            return false
        }
    }

    fun startApplication(namespace: String, activity: String): Boolean {
        try {
            val cmd = "am start -a android.intent.action.MAIN -c android.intent.category.LAUNCHER -n $namespace/$activity"
            Runtime.getRuntime().exec(cmd)
            return true
        } catch (e: Exception) {
            return false
        }
    }

    fun gotoApp(context: Context, namespace: String, url: String) {
        if (ApkUtils.applicationInstalled(context, namespace)) {
            openApp(context, namespace)
        } else {
            openDownloadApp(context, url)
        }
    }

    fun openDownloadApp(context: Context, url: String) {
        val inDownload = Intent(Intent.ACTION_VIEW)
        inDownload.data = Uri.parse(url)
        context.startActivity(inDownload)
    }

    fun openGooglePlayForApp(context: Context, namespace: String) {
        val inPlay = Intent(Intent.ACTION_VIEW)
        inPlay.data = Uri.parse("market://details?id=$namespace")
        context.startActivity(inPlay)
    }

    fun setInstallLocation(location: Int) {
        Command.runCommand("pm set-install-location $location", true, null)
    }

    fun openApp(context: Context, packageName: String): Boolean = openApp(context, packageName, false)

    fun openApp(context: Context, packageName: String, newTask: Boolean): Boolean {
        var pi: PackageInfo? = null
        try {
            pi = context.packageManager.getPackageInfo(packageName, 0)
        } catch (e: Exception) {
        }
        if (pi == null) {
            return false
        }
        val resolveIntent = Intent(Intent.ACTION_MAIN, null)
        resolveIntent.addCategory(Intent.CATEGORY_LAUNCHER)
        resolveIntent.`package` = pi.packageName
        var ret = false
        val apps = context.packageManager.queryIntentActivities(resolveIntent, 0)
        try {
            val ri = apps.iterator().next()
            if (ri != null) {
                val className = ri.activityInfo.name
                val intent = Intent(Intent.ACTION_MAIN)
                intent.addCategory(Intent.CATEGORY_LAUNCHER)
                val cn = ComponentName(packageName, className)
                intent.component = cn
                if (newTask) {
                    intent.flags = Intent.FLAG_ACTIVITY_NEW_TASK
                }
                context.startActivity(intent)
                ret = true
            }
        } catch (e: Exception) {

        }
        return ret
    }

    fun scanApksInSdcard(callback: OnCommandExecutionListener?) {
        thread {
            val cmd = "busybox find /sdcard/ -name \"*.apk\""
            Command.runCommand(cmd, true, callback)
        }
    }

    fun isAppInstalled(context: Context, packageName: String): Boolean {
        var info: ApplicationInfo? = null
        try {
            info = context.packageManager.getApplicationInfo(packageName, 0)
        } catch (e: PackageManager.NameNotFoundException) {

        }
        return info != null
    }

    /**
     * getApkFileStatus
     *
     * @param newinfo
     * @return status with the new application info<br>
     * return 0: installed with same signature<br>
     * return 1: installed with different signature<br>
     * return 2: no need update<br>
     * return 3: not installed<br>
     * return 4: error
     */
    fun getApkFileStatus(context: Context, newinfo: DataappInfo?): Int {
        try {
            val packageName = newinfo?.info?.packageName
            var installedInfo: ApplicationInfo? = null
            try {
                installedInfo = context.packageManager.getApplicationInfo(packageName, 0)
            } catch (e: PackageManager.NameNotFoundException) {

            }
            if (installedInfo == null) {
                return 3
            }
            val newVer = DeviceUtils.getAppVersionCode(context, newinfo!!.localPath!!)
            val oldVer = DeviceUtils.getAppVersionCode(context, installedInfo)
            if (newVer <= oldVer) {
                return 2
            }
            val compare = SignatureUtils.compareSignature(newinfo.localPath!!, installedInfo.sourceDir)
            return if (compare) 0 else 1
        } catch (e: Exception) {
            return 4
        }
    }

    fun findApplication(context: Context, regex: String, system: Boolean): ApplicationInfo? {
        var info: ApplicationInfo? = null
        val regs = regex.split("|") // 0:start, 1:end, 2:contain
        val pm = context.packageManager
        var list: MutableList<ApplicationInfo?>? = null
        try {
            list = pm.getInstalledApplications(0)
        } catch (e: Exception) {

        }
        var match = true
        if (list != null && list.size != 0) {
            var pkgName: String
            for (pn in list) {
                if (system && (!pn!!.sourceDir!!.contains("/system/app") && !pn.sourceDir.contains("/system/priv-app"))) {
                    continue
                }
                pkgName = pn!!.packageName
                if (regs[0].trim() != "") {
                    match = pkgName.startsWith(regs[0].trim())
                }
                if (match && regs[1].trim() != "") {
                    match = pkgName.endsWith(regs[1].trim())
                }
                if (match && regs[2].trim() != "") {
                    match = pkgName.contains(regs[2].trim())
                }
                if (match) {
                    try {
                        info = pm.getApplicationInfo(pkgName, 0)
                    } catch (e: Exception) {

                    }
                    if (info != null) {
                        break
                    }
                }
            }
        }
        return info
    }

    fun openInstallApk(context: Context, apkPath: String) {
        val intent = Intent(Intent.ACTION_VIEW)
        intent.setDataAndType(Uri.parse("file://$apkPath"), "application/vnd.android.package-archive")
        intent.flags = Intent.FLAG_ACTIVITY_NEW_TASK
        context.startActivity(intent)
    }

    class SysappInfo {
        var info: ApplicationInfo? = null
        var position = 0
    }

    class DataappInfo {
        var info: ApplicationInfo? = null
        var type = 0
        var checked = false
        var log: String? = null
        var localPath: String? = null
        var apkStatus = 0
        var appName: String? = null
        var installed = false
        // 0: succ | 1: exists | 2: fail
        var logId = 0
        var position = 0
        var installing = false
    }

}