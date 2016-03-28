package com.hujiang.devart.component.cache

import android.graphics.Bitmap
import java.util.*

/**
 * Created by rarnu on 3/28/16.
 */
object MemoryCache {

    private val _cache = Collections.synchronizedMap(LinkedHashMap<String, Bitmap>(10, 1.5f, true))
    private var _size = 0L
    private var _limit = 1000000L

    fun initMemoryCache() = setLimit(Runtime.getRuntime().maxMemory() / 4)

    fun setLimit(newLimit: Long) {
        _limit = newLimit
    }

    fun get(id: String): Bitmap? = try {
        var bmp: Bitmap? = null
        if (_cache.containsKey(id)) {
            bmp = _cache[id]
        }
        bmp
    } catch (e: Exception) {
        null
    }


    fun put(id: String, bitmap: Bitmap?) =
            try {
                if (_cache.containsKey(id)) {
                    _size -= getSizeInBytes(_cache[id])
                }
                _cache.put(id, bitmap)
                _size += getSizeInBytes(bitmap)
                checkSize()
            } catch (t: Throwable) {
            }


    private fun checkSize() {
        if (_size > _limit) {
            val iter = _cache.entries.iterator()
            while (iter.hasNext()) {
                val entry = iter.next()
                _size -= getSizeInBytes(entry.value)
                iter.remove()
                if (_size <= _limit) {
                    break
                }
            }
        }
    }

    fun clearCache() = try {
        _cache.clear()
        _size = 0L
    } catch (ex: Exception) {
    }


    private fun getSizeInBytes(bitmap: Bitmap?): Long =
            if (bitmap == null) {
                0
            } else {
                (bitmap.rowBytes * bitmap.height).toLong()
            }


}