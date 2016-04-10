package com.hujiang.devart.utils

import android.content.res.AssetManager

/**
 * Created by rarnu on 4/9/16.
 */
class AssetManagerUtils {

    private var _manager: AssetManager? = null

    constructor(am: AssetManager?) {
        _manager = am
    }

    fun addAssetPath(path: String): Int {
        var ret = 0
        try {
            val m = _manager!!.javaClass.getDeclaredMethod("addAssetPath", String::class.java)
            m.isAccessible = true
            ret = m.invoke(_manager, path) as Int
        } catch (e: Exception) {
        }
        return ret
    }

}