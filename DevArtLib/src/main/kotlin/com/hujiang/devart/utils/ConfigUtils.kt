package com.hujiang.devart.utils

import android.content.Context
import android.content.SharedPreferences
import android.content.pm.ApplicationInfo
import android.content.pm.PackageManager
import android.preference.PreferenceManager

/**
 * Created by rarnu on 3/28/16.
 */
object ConfigUtils {

    private var _sp: SharedPreferences? = null

    private fun initSharedPreference(context: Context) =
            if (_sp == null) { _sp = PreferenceManager.getDefaultSharedPreferences(context) } else { }


    fun getStringConfig(context: Context, key: String, def: String?): String? {
        initSharedPreference(context)
        return _sp?.getString(key, def)
    }

    fun getIntConfig(context: Context, key: String, def: Int): Int? {
        initSharedPreference(context)
        return _sp?.getInt(key, def)
    }

    fun getLongConfig(context: Context, key: String, def: Long): Long? {
        initSharedPreference(context)
        return _sp?.getLong(key, def)
    }

    fun getBooleanConfig(context: Context, key: String, def: Boolean): Boolean? {
        initSharedPreference(context)
        return _sp?.getBoolean(key, def)
    }

    fun getFloatConfig(context: Context, key: String, def: Float): Float? {
        initSharedPreference(context)
        return _sp?.getFloat(key, def)
    }

    fun setStringConfig(context: Context, key: String, value: String?) {
        initSharedPreference(context)
        _sp?.edit()?.putString(key, value)?.commit()
    }

    fun setIntConfig(context: Context, key: String, value: Int) {
        initSharedPreference(context)
        _sp?.edit()?.putInt(key, value)?.commit()
    }

    fun setLongConfig(context: Context, key: String, value: Long) {
        initSharedPreference(context)
        _sp?.edit()?.putLong(key, value)?.commit()
    }

    fun setBooleanConfig(context: Context, key: String, value: Boolean) {
        initSharedPreference(context)
        _sp?.edit()?.putBoolean(key, value)?.commit()
    }

    fun setFloatConfig(context: Context, key: String, value: Float) {
        initSharedPreference(context)
        _sp?.edit()?.putFloat(key, value)?.commit()
    }

    private fun getApplicationInfo(context: Context): ApplicationInfo? =
            context.packageManager.getApplicationInfo(context.packageName, PackageManager.GET_META_DATA)

    fun getManifestIntConfig(context: Context, key: String, def: Int): Int? =
            getApplicationInfo(context)?.metaData?.getInt(key, def)

    fun getManifestBooleanConfig(context: Context, key: String, def: Boolean): Boolean? =
            getApplicationInfo(context)?.metaData?.getBoolean(key, def)

    fun getManifestStringConfig(context: Context, key: String, def: String?): String? =
            getApplicationInfo(context)?.metaData?.getString(key, def)

    fun getManifestLongConfig(context: Context, key: String, def: Long): Long? =
            getApplicationInfo(context)?.metaData?.getLong(key, def)

    fun getManifestFloatConfig(context: Context, key: String, def: Float): Float? =
            getApplicationInfo(context)?.metaData?.getFloat(key, def)

    fun getObjectConfig(context: Context, key: String, obj: Any?) {
        initSharedPreference(context)
        val fs = ReflectionUtils.getClassFields(obj)
        var typeStr: String
        var keyStr: String
        for (f in fs!!) {
            typeStr = f.type.simpleName
            keyStr = "${key}_${f.name}"
            when (typeStr) {
                "String" -> try { f.set(obj, getStringConfig(context, keyStr, "")) } catch(e: Exception) { }
                "int" -> try { f.setInt(obj, getIntConfig(context, keyStr, 0)!!) } catch(e: Exception) { }
                "double" -> try { f.setDouble(obj, getFloatConfig(context, keyStr, 0.0f)!!.toDouble()) } catch(e: Exception) { }
                "boolean" -> try { f.setBoolean(obj, getBooleanConfig(context, keyStr, false)!!) } catch(e: Exception) {}
                "float" -> try { f.setFloat(obj, getFloatConfig(context, keyStr, 0.0f)!!) } catch(e: Exception) { }
                "long" -> try { f.setLong(obj, getLongConfig(context, keyStr, 0L)!!) } catch(e: Exception) { }
                "byte" -> try { f.setByte(obj, getIntConfig(context, keyStr, 0)!!.toByte()) } catch(e: Exception) { }
                "short" -> try { f.setShort(obj, getIntConfig(context, keyStr, 0)!!.toShort()) } catch(e: Exception) { }
                "char" -> try { f.setChar(obj, getStringConfig(context, keyStr, "")!![0]) } catch(e: Exception) { }
                else -> { }
            }
        }
    }

    fun setObjectConfig(context: Context, key: String, obj: Any?) {
        initSharedPreference(context)
        val fs = ReflectionUtils.getClassFields(obj)
        var typeStr: String
        var keyStr: String
        for (f in fs!!) {
            typeStr = f.type.simpleName
            keyStr = "${key}_${f.name}"
            when (typeStr) {
                "String" -> try { setStringConfig(context, keyStr, f.get(obj) as String) } catch (e: Exception) { }
                "int" -> try { setIntConfig(context, keyStr, f.getInt(obj)) } catch (e: Exception) { }
                "double" -> try { setFloatConfig(context, keyStr, f.getFloat(obj)) } catch (e: Exception) { }
                "boolean" -> try { setBooleanConfig(context, keyStr, f.getBoolean(obj)) } catch (e: Exception) { }
                "float" -> try { setFloatConfig(context, keyStr, f.getDouble(obj) as Float) } catch (e: Exception) { }
                "long" -> try { setLongConfig(context, keyStr, f.getLong(obj)) } catch (e: Exception) { }
                "byte" -> try { setIntConfig(context, keyStr, f.getByte(obj).toInt()) } catch (e: Exception) { }
                "short" -> try { setIntConfig(context, keyStr, f.getShort(obj).toInt()) } catch (e: Exception) { }
                "char" -> try { setStringConfig(context, keyStr, f.getChar(obj) as String) } catch (e: Exception) { }
                else -> { }
            }
        }
    }

}