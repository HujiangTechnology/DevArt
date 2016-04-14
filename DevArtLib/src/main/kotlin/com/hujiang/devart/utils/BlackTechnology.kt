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

    external fun blackGetMacAddress(): MutableList<MacAddress?>?

    data class MacAddress(var identifier: String = "", var attributes: String = "", var macAddress: String = "")

}