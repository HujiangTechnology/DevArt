package com.hujiang.devart.utils

import java.lang.reflect.Field
import java.lang.reflect.Method

/**
 * Created by rarnu on 3/25/16.
 */
object ReflectionUtils {

    fun getClassFields(obj: Any?): Array<Field>? = obj?.javaClass?.declaredFields

    fun getClassMethods(obj: Any?): Array<Method>? = obj?.javaClass?.declaredMethods

    fun invokeClassMethod(obj: Any?, method: String, vararg params: Any?): Any? = obj?.javaClass?.getMethod(method)?.invoke(obj, params)

    fun getClassPrivateFieldValue(obj: Any?, field: String): Any? {
        var ret: Any? =  null
        try {
            val f = obj?.javaClass?.getDeclaredField(field)
            f?.isAccessible = true
            ret = f?.get(obj)
        } catch (e: Exception) {

        }
        return ret
    }

    fun setClassPrivateFieldValue(obj: Any?, field: String, value: Any?) {
        try {
            val f = obj?.javaClass?.getDeclaredField(field)
            f?.isAccessible = true
            f?.set(obj, value)
        } catch (e: Exception) {

        }
    }

    fun invokeClassPrivateMethod(obj: Any?, method: String, vararg params: Any?): Any? {
        var ret: Any? = null
        try {
            val m = obj?.javaClass?.getMethod(method)
            m?.isAccessible = true
            ret = m?.invoke(obj, params)
        } catch (e: Exception) {

        }
        return ret
    }

}