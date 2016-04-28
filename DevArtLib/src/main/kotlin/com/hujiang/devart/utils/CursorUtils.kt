package com.hujiang.devart.utils

import android.content.ContentValues
import android.database.Cursor

/**
 * Created by rarnu on 3/28/16.
 */
object CursorUtils {

    fun buildContentValues(obj: Any?): ContentValues? = try {
        val cv = ContentValues()
        val fs = ReflectionUtils.getClassFields(obj)
        var typeStr: String
        for (f in fs!!) {
            typeStr = f.type.simpleName
            when (typeStr) {
                "String" -> cv.put(f.name, f.get(obj) as String)
                "int" -> cv.put(f.name, f.getInt(obj))
                "double" -> cv.put(f.name, f.getDouble(obj))
                "boolean" -> cv.put(f.name, f.getBoolean(obj))
                "float" -> cv.put(f.name, f.getFloat(obj))
                "long" -> cv.put(f.name, f.getLong(obj))
                "byte" -> cv.put(f.name, f.getByte(obj))
                "short" -> cv.put(f.name, f.getShort(obj))
                "char" -> cv.put(f.name, f.getChar(obj).toString())
                else -> {
                }
            }
        }
        cv
    } catch (e: Exception) {
        null
    }


    fun fillCursorToObject(c: Cursor, obj: Any?) = try {
            val fs = ReflectionUtils.getClassFields(obj)
            var typeStr: String
            for (f in fs!!) {
                typeStr = f.type.simpleName
                when (typeStr) {
                    "String" -> f.set(obj, c.getString(c.getColumnIndex(f.name)))
                    "int" -> f.setInt(obj, c.getInt(c.getColumnIndex(f.name)))
                    "double" -> f.setDouble(obj, c.getDouble(c.getColumnIndex(f.name)))
                    "boolean" -> f.setBoolean(obj, c.getInt(c.getColumnIndex(f.name)) != 0)
                    "float" -> f.setFloat(obj, c.getFloat(c.getColumnIndex(f.name)))
                    "long" -> f.setLong(obj, c.getLong(c.getColumnIndex(f.name)))
                    "byte" -> f.setByte(obj, c.getString(c.getColumnIndex(f.name)).toByte())
                    "short" -> f.setShort(obj, c.getShort(c.getColumnIndex(f.name)))
                    "char" -> f.setChar(obj, c.getString(c.getColumnIndex(f.name))[0])
                    else -> { }
                }
            }
        } catch (e: Exception) {

        }

    fun buildSplittedString(list: MutableList<*>?, @Suppress("UNUSED_PARAMETER") splitter: String): String {
        var result = ""
        for (o in list!!) {
            result += "${o.toString()},"
        }
        if (result.length > 0) {
            result = result.substring(0, result.length - 1)
        }
        return result
    }

}