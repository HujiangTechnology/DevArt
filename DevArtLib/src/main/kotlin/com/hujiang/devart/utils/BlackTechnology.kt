package com.hujiang.devart.utils

import android.util.Log

/**
 * **BLACK TECHONOLEDGE CLASS**
 *
 * 黑科技类, 用时小心
 *
 * @Created by rarnu on 3/23/16.
 *
 */
object BlackTechnology {

    init {
        try {
            System.loadLibrary("blacktech")
        } catch(e: Exception) {
            Log.e("BlackTechnology", "System.loadLibrary(\"blacktech\") => ${e.message}")
        }
    }

    /**
     * ```
     * val str = blackGetMacAddress()
     * val macList = strToMacAddressList(str)
     * ```
     */
    external fun blackGetMacAddress(): String

    fun strToMacAddressList(str: String): MutableList<MacAddress>? {
        var result: MutableList<MacAddress>? = null
        try {
            val strMacs = str.split("^")
            result = arrayListOf<MacAddress>()
            for (s in strMacs) {
                val details = s.split("|")
                result.add(MacAddress(details[0], details[1], details[2]))
            }
        } catch(e: Exception) {
            Log.e("LOG", "strToMacAddressList => ${e.message}")
        }
        return result
    }

    data class MacAddress(var identifier: String = "", var attributes: String = "", var macAddress: String = "")

}