package com.hujiang.devart.utils

import android.content.Context
import android.net.wifi.ScanResult
import android.net.wifi.WifiConfiguration
import android.net.wifi.WifiInfo
import android.net.wifi.WifiManager
import java.lang.reflect.Method

/**
 * Created by rarnu on 3/29/16.
 */
object WifiUtils {

    private var _wifiLock: WifiManager.WifiLock? = null
    private var _wifiManager: WifiManager? = null
    private var _wifiInfo: WifiInfo? = null
    private var _wifiList: MutableList<ScanResult>? = null
    private var _wifiConfigurations: MutableList<WifiConfiguration>? = null

    fun initWifiUtils(context: Context) {
        _wifiManager = context.getSystemService(Context.WIFI_SERVICE) as WifiManager
        _wifiInfo = _wifiManager?.connectionInfo
    }

    fun openWifi() {
        if (!_wifiManager!!.isWifiEnabled) {
            _wifiManager?.isWifiEnabled = true
        }
    }

    fun closeWifi() {
        if (!_wifiManager!!.isWifiEnabled) {
            _wifiManager?.isWifiEnabled = false
        }
    }

    fun checkState(): Int? = _wifiManager?.wifiState

    fun acquireWifiLock() = _wifiLock?.acquire()

    fun releaseWifiLock() = if (_wifiLock!!.isHeld) _wifiLock?.acquire() else { }

    fun createWifiLock(name: String) {
        _wifiLock = _wifiManager?.createWifiLock(name)
    }

    fun getConfiguration(): MutableList<WifiConfiguration>? = _wifiConfigurations

    fun connetionConfiguration(index: Int) {
        if (index > _wifiConfigurations!!.size) {
            return
        }
        _wifiManager?.enableNetwork(_wifiConfigurations!![index].networkId, true)
    }

    fun startScan() {
        _wifiManager?.startScan()
        _wifiList = _wifiManager?.scanResults
        _wifiConfigurations = _wifiManager?.configuredNetworks
    }

    fun getWifiList(): MutableList<ScanResult>?  {
        _wifiList = _wifiManager?.scanResults
        _wifiConfigurations = _wifiManager?.configuredNetworks
        return _wifiList
    }

    fun getMacAddress(): String? = _wifiInfo?.macAddress

    fun getBSSID(): String? = _wifiInfo?.bssid

    fun getSSID(): String? = _wifiInfo?.ssid

    fun getIpAddress(): Int? = _wifiInfo?.ipAddress

    fun getNetWordId(): Int? = _wifiInfo?.networkId

    fun getWifiInfo(): WifiInfo? = _wifiManager?.connectionInfo

    fun addNetWork(configuration: WifiConfiguration?): Int {
        val wcgId = _wifiManager?.addNetwork(configuration)
        val isSuccess = _wifiManager?.enableNetwork(wcgId!!, true)
        return if (isSuccess!!) wcgId!! else -1
    }

    fun connectWifi(netId: Int) = _wifiManager?.enableNetwork(netId, true)

    fun disConnectionWifi(netId: Int) {
        _wifiManager?.disableNetwork(netId)
        _wifiManager?.disconnect()
    }

    fun createWifiInfo(ssid: String, password: String, type: Int): WifiConfiguration? {
        val config = WifiConfiguration()
        config.allowedAuthAlgorithms.clear()
        config.allowedGroupCiphers.clear()
        config.allowedKeyManagement.clear()
        config.allowedPairwiseCiphers.clear()
        config.allowedProtocols.clear()
        config.SSID = "\"${ssid}\""
        val tempConfig = isExsits(ssid)
        if (tempConfig != null) {
            _wifiManager?.removeNetwork(tempConfig.networkId)
        }
        when(type) {
            1 -> {
                config.wepKeys[0] = ""
                config.allowedKeyManagement.set(WifiConfiguration.KeyMgmt.NONE)
                config.wepTxKeyIndex = 0
            }
            2 -> {
                config.hiddenSSID = true
                config.wepKeys[0] = "\"${password}\""
                config.allowedAuthAlgorithms.set(WifiConfiguration.AuthAlgorithm.SHARED)
                config.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.CCMP)
                config.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.TKIP)
                config.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.WEP40)
                config.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.WEP104)
                config.allowedKeyManagement.set(WifiConfiguration.KeyMgmt.NONE)
                config.wepTxKeyIndex = 0
            }
            3 -> {
                config.preSharedKey = "\"${password}\""
                config.hiddenSSID = true
                config.allowedAuthAlgorithms.set(WifiConfiguration.AuthAlgorithm.OPEN)
                config.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.TKIP)
                config.allowedKeyManagement.set(WifiConfiguration.KeyMgmt.WPA_PSK)
                config.allowedPairwiseCiphers.set(WifiConfiguration.PairwiseCipher.TKIP)
                config.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.CCMP)
                config.allowedPairwiseCiphers.set(WifiConfiguration.PairwiseCipher.CCMP)
                config.status = WifiConfiguration.Status.ENABLED
            }
        }
        return config
    }

    private fun isExsits(ssid: String): WifiConfiguration? {
        var config: WifiConfiguration? = null
        val existingConfigs = _wifiManager?.configuredNetworks
        for (existingConfig in existingConfigs!!) {
            if (existingConfig.SSID == "\"${ssid}\"") {
                config = existingConfig
                break
            }
        }
        return config
    }

    fun createWifiAp(ssid: String, password: String, enabled: Boolean): Boolean {
        if (enabled) {
            _wifiManager?.isWifiEnabled = false
        }
        try {
            val apConfig = WifiConfiguration()
            apConfig.SSID = ssid
            apConfig.preSharedKey = password
            apConfig.hiddenSSID = true
            apConfig.status = WifiConfiguration.Status.ENABLED
            apConfig.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.TKIP)
            apConfig.allowedGroupCiphers.set(WifiConfiguration.GroupCipher.CCMP)
            apConfig.allowedKeyManagement.set(WifiConfiguration.KeyMgmt.WPA_PSK)
            apConfig.allowedPairwiseCiphers.set(WifiConfiguration.PairwiseCipher.TKIP)
            apConfig.allowedPairwiseCiphers.set(WifiConfiguration.PairwiseCipher.CCMP)
            apConfig.allowedProtocols.set(WifiConfiguration.Protocol.RSN)
            val m = _wifiManager?.javaClass?.getDeclaredMethod("setWifiApEnabled", WifiConfiguration::class.java, Boolean::class.java)
            val ret = m?.invoke(_wifiManager, apConfig, enabled) as Boolean
            return ret
        } catch (e: Exception) {
            return false
        }
    }

    fun removeWifi(netId: Int) = _wifiManager?.removeNetwork(netId)

}