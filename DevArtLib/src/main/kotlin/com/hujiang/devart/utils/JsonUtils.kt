package com.hujiang.devart.utils

import org.json.JSONArray
import org.json.JSONObject
import java.lang.reflect.Field

/**
 * Created by rarnu on 3/28/16.
 */
class JsonUtils<T> {

    companion object {
        fun fillJsonToObject(jsonString: String, obj: Any?) = fillJsonToObject(JSONObject(jsonString), obj)

        fun fillJsonToObject(json: JSONObject?, obj: Any?) = try {
            val fs = ReflectionUtils.getClassFields(obj)
            var typeStr: String
            for (f in fs!!) {
                f.isAccessible = true
                typeStr = f.type.simpleName
                when (typeStr) {
                    "String" -> try {
                        f.set(obj, json!!.getString(f.name))
                    } catch(e: Exception) {
                    }
                    "int" -> try {
                        f.setInt(obj, json!!.getInt(f.name))
                    } catch(e: Exception) {
                    }
                    "double" -> try {
                        f.setDouble(obj, json!!.getDouble(f.name))
                    } catch(e: Exception) {
                    }
                    "boolean" -> try {
                        f.setBoolean(obj, json!!.getBoolean(f.name))
                    } catch(e: Exception) {
                    }
                    "float" -> try {
                        f.setFloat(obj, json!!.getDouble(f.name).toFloat())
                    } catch(e: Exception) {
                    }
                    "long" -> try {
                        f.setLong(obj, json!!.getLong(f.name))
                    } catch(e: Exception) {
                    }
                    "byte" -> try {
                        f.setByte(obj, json!!.getInt(f.name).toByte())
                    } catch(e: Exception) {
                    }
                    "short" -> try {
                        f.setShort(obj, json!!.getInt(f.name).toShort())
                    } catch(e: Exception) {
                    }
                    "char" -> try {
                        f.setChar(obj, json!!.getString(f.name)[0])
                    } catch(e: Exception) {
                    }
                    else -> {
                    }
                }
            }
        } catch(e: Exception) {
        }


        fun objectToJsonString(obj: Any?): String? {
            var str = "{"
            val fs = ReflectionUtils.getClassFields(obj)
            var typeStr: String
            for (f in fs!!) {
                f.isAccessible = true
                typeStr = f.type.simpleName
                when (typeStr) {
                    "String" -> try {
                        str += "\"${f.name}\":\"${f.get(obj) as String}\","
                    } catch (e: Exception) {
                    }
                    "int" -> try {
                        str += "\"${f.name}\":${f.getInt(obj)},"
                    } catch (e: Exception) {
                    }
                    "double" -> try {
                        str += "\"${f.name}\":${f.getDouble(obj)},"
                    } catch (e: Exception) {
                    }
                    "boolean" -> try {
                        str += "\"${f.name}\":${f.getBoolean(obj)},"
                    } catch (e: Exception) {
                    }
                    "float" -> try {
                        str += "\"${f.name}\":${f.getFloat(obj)},"
                    } catch (e: Exception) {
                    }
                    "long" -> try {
                        str += "\"${f.name}\":${f.getLong(obj)},"
                    } catch (e: Exception) {
                    }
                    "byte" -> try {
                        str += "\"${f.name}\":${f.getByte(obj)},"
                    } catch (e: Exception) {
                    }
                    "short" -> try {
                        str += "\"${f.name}\":${f.getShort(obj)},"
                    } catch (e: Exception) {
                    }
                    "char" -> try {
                        str += "\"${f.name}\":\"${f.getChar(obj)}\","
                    } catch (e: Exception) {
                    }
                    else -> try {
                        str += "\"${f.name}\":\"${f.get(obj).toString()}\","
                    } catch (e: Exception) {
                    }
                }
            }
            str = str.substring(0, str.length - 1)
            str += "}"
            return str
        }
    }

    private var _root: JsonNode? = null
    private var _classType: Class<T>? = null
    private var _innerClassType: Class<T>? = null

    constructor(ct: Class<T>, root: JsonNode?) {
        _classType = ct
        _root = root
    }

    constructor(ct: Class<T>, innerCt: Class<T>, root: JsonNode?) : this(ct, root) {
        _innerClassType = innerCt
    }

    fun toJson(obj: T): String? = when (_root!!.fieldType) {
        JsonNode.FieldType.ftObject -> objectToJson(obj!!, JSONObject(), _root!!).toString()
        JsonNode.FieldType.ftList -> listToJsonArray(obj!!, JSONArray(), _root!!).toString()
        JsonNode.FieldType.ftMap -> mapToJsonObject(obj!!, JSONObject(), _root!!).toString()
        JsonNode.FieldType.ftValue -> objectToJson(obj!!, JSONObject(), _root!!).toString()
    }

    fun toObject(jsonString: String): T {
        var obj = _classType?.newInstance()
        when (_root!!.fieldType) {
            JsonNode.FieldType.ftList -> obj = jsonToList(JSONArray(jsonString), _classType!!, _innerClassType!!, obj!!, _root!!) as T
            JsonNode.FieldType.ftMap -> obj = jsonToMap(JSONObject(jsonString), _classType!!, _innerClassType!!, obj!!, _root!!) as T
            JsonNode.FieldType.ftObject -> obj = jsonToObject(JSONObject(jsonString), _classType!!, obj!!, _root!!) as T
            JsonNode.FieldType.ftValue -> obj = jsonToObject(JSONObject(jsonString), _classType!!, obj!!, _root!!) as T
        }
        return obj!!
    }

    private fun objectToJson(obj: Any, jobj: JSONObject, node: JsonNode): JSONObject? {
        for (i in 0..node.childs!!.size - 1) {
            val f = getField(obj, node.childs!![i].fieldName)
            val type = node.childs!![i].fieldType
            switchTypeDoO2J(type, jobj, node.childs!![i].fieldName, f, obj, i, node.childs!![i])
        }
        return jobj
    }

    private fun jsonToList(jarr: JSONArray, cType: Class<*>, genericType: Class<*>, obj: Any, node: JsonNode): Any {
        for (i in 0..jarr.length() - 1) {
            if (node.subItemNode!!.fieldType == JsonNode.FieldType.ftValue) {
                (obj as MutableList<Any>).add(jarr.get(i))
            } else {
                val o = genericType.newInstance()
                for (j in 0..node.subItemNode!!.childs!!.size - 1) {
                    val f = getField(o, node.subItemNode!!.childs!![j].fieldName)
                    val type = node.subItemNode!!.childs!![j].fieldType
                    switchTypeDoJ2O(type, o, f, null, jarr, node.subItemNode!!.childs!![j].fieldName, null, i, j, node.subItemNode!!.childs!![j], true, false)
                }
                (obj as MutableList<Any>).add(o)
            }
        }
        return obj;
    }

    private fun jsonToMap(jobj: JSONObject, cType: Class<*>, genericType: Class<*>, obj: Any, node: JsonNode): Any {
        val iter = jobj.keys()
        var key: String
        while (iter.hasNext()) {
            key = iter.next() as String
            if (node.subItemNode!!.fieldType == JsonNode.FieldType.ftValue) {
                (obj as MutableMap<String, Any>).put(key, jobj.get(key))
            } else {
                val o = genericType.newInstance()
                for (i in 0..node.subItemNode!!.childs!!.size - 1) {
                    val f = getField(o, node.subItemNode!!.childs!![i].fieldName)
                    val type = node.subItemNode!!.childs!![i].fieldType
                    switchTypeDoJ2O(type, o, f, jobj, null, node.subItemNode!!.childs!![i].fieldName, key, i, -1, node.subItemNode!!.childs!![i], false, true)
                }
                (obj as MutableMap<String, Any>).put(key, o)
            }
        }
        return obj;
    }

    private fun getField(o: Any, name: String): Field {
        val f = o.javaClass.getDeclaredField(name)
        f.isAccessible = true
        return f
    }

    private fun switchTypeDoO2J(type: JsonNode.FieldType, jobj: JSONObject, name: String, f: Field, o: Any, index: Int, node: JsonNode) = when (type) {
        JsonNode.FieldType.ftValue -> jobj.put(name, f.get(o))
        JsonNode.FieldType.ftObject -> jobj.put(name, objectToJson(f.get(o), JSONObject(), node))
        JsonNode.FieldType.ftList -> jobj.put(name, listToJsonArray(f.get(o), JSONArray(), node))
        JsonNode.FieldType.ftMap -> jobj.put(name, mapToJsonObject(f.get(o), JSONObject(), node))
    }

    private fun switchTypeDoJ2O(type: JsonNode.FieldType, o: Any, f: Field, jobj: JSONObject?, jarr: JSONArray?, name: String, key: String?, index: Int, indexj: Int, node: JsonNode, isArray: Boolean, isMap: Boolean) = when (type) {
        JsonNode.FieldType.ftList -> {
            val fList = getField(o, name)
            val cList = fList.type
            val sType = fList.genericType.toString()
            val genericClassName = sType.substring(sType.indexOf("<") + 1, sType.indexOf(">"))
            val cListInner = Class.forName(genericClassName)
            val oList = cList.newInstance()
            if (isMap) {
                f.set(o, jsonToList(jobj!!.getJSONArray(key), cList, cListInner, oList, node.subItemNode!!.childs!![index]))
            } else {
                if (isArray) {
                    f.set(o, jsonToList(jarr!!.getJSONObject(index).getJSONArray(name), cList, cListInner, oList, node.subItemNode!!.childs!![indexj]))
                } else {
                    f.set(o, jsonToList(jobj!!.getJSONArray(name), cList, cListInner, oList, node.childs!![index]))
                }
            }
        }

        JsonNode.FieldType.ftMap -> {
            val fMap = getField(o, name)
            val cMap = fMap.type
            val sMapType = fMap.genericType.toString()
            val genericMapName = sMapType.substring(sMapType.indexOf(",") + 1, sMapType.indexOf(">")).trim()
            val cMapInner = Class.forName(genericMapName)
            val oMap = cMap.newInstance()
            if (isMap) {
                f.set(o, jsonToMap(jobj!!.getJSONObject(key), cMap, cMapInner, oMap, node.subItemNode!!.childs!![index]))
            } else {
                if (isArray) {
                    f.set(o, jsonToMap(jarr!!.getJSONObject(index).getJSONObject(name), cMap, cMapInner, oMap, node.subItemNode!!.childs!![indexj]))
                } else {
                    f.set(o, jsonToMap(jobj!!.getJSONObject(name), cMap, cMapInner, oMap, node))
                }
            }
        }

        JsonNode.FieldType.ftObject -> {
            val fSub = getField(o, name)
            val cSub = fSub.type
            val oSub = cSub.newInstance()
            if (isMap) {
                f.set(o, jsonToObject(jobj!!.getJSONObject(key), cSub, oSub, node.subItemNode!!.childs!![index]))
            } else {
                if (isArray) {
                    f.set(o, jsonToObject(jarr!!.getJSONObject(index).getJSONObject(name), cSub, oSub, node.subItemNode!!.childs!![indexj]))
                } else {
                    f.set(o, jsonToObject(jobj!!.getJSONObject(name), cSub, oSub, node.childs!![index]))
                }
            }
        }

        JsonNode.FieldType.ftValue -> {
            if (isMap) {
                f.set(o, jobj!!.getJSONObject(key).get(name))
            } else {
                if (isArray) {
                    f.set(o, jarr!!.getJSONObject(index).get(name))
                } else {
                    f.set(o, jobj!!.get(name))
                }
            }
        }
    }

    private fun jsonToObject(jobj: JSONObject, cType: Class<*>, obj: Any, node: JsonNode): Any {
        for (i in 0..node.childs!!.size - 1) {
            val f = getField(obj, node.childs!![i].fieldName)
            val type = node.childs!![i].fieldType
            switchTypeDoJ2O(type, obj, f, jobj, null, node.childs!![i].fieldName, null, i, -1, node.childs!![i], false, false)
        }
        return obj
    }

    private fun listToJsonArray(list: Any, jsonarray: JSONArray, node: JsonNode): JSONArray {
        val objList = list as MutableList<*>
        for (o in objList) {
            if (node.subItemNode!!.fieldType == JsonNode.FieldType.ftValue) {
                jsonarray.put(o)
            } else {
                val jo = JSONObject()
                for (j in 0..node.subItemNode!!.childs!!.size - 1) {
                    val f = getField(o!!, node.subItemNode!!.childs!![j].fieldName)
                    val type = node.subItemNode!!.childs!![j].fieldType
                    switchTypeDoO2J(type, jo, node.subItemNode!!.childs!![j].fieldName, f, o, j, node.subItemNode!!.childs!![j])
                }
                jsonarray.put(jo)
            }
        }
        return jsonarray
    }

    private fun mapToJsonObject(map: Any, jsonobject: JSONObject, node: JsonNode): JSONObject {
        val mapObj = map as MutableMap<*, *>
        val iter = mapObj.keys.iterator()
        var key: String
        var o: Any?
        while (iter.hasNext()) {
            key = iter.next() as String
            if (node.subItemNode!!.fieldType == JsonNode.FieldType.ftValue) {
                jsonobject.put(key, mapObj[key])
            } else {
                o = mapObj[key]
                val jo = JSONObject()
                for (i in 0..node.subItemNode!!.childs!!.size - 1) {
                    val f = getField(o!!, node.subItemNode!!.childs!![i].fieldName)
                    val type = node.subItemNode!!.childs!![i].fieldType
                    switchTypeDoO2J(type, jo, node.subItemNode!!.childs!![i].fieldName, f, o, i, node.subItemNode!!.childs!![i])
                }
                jsonobject.put(key, jo)
            }
        }
        return jsonobject
    }


    class JsonNode {
        var fieldName = ""
        var fieldType = FieldType.ftValue
        var childs: MutableList<JsonNode>? = null
        var subItemNode: JsonNode? = null

        constructor(name: String, type: FieldType) {
            fieldName = name
            fieldType = type
            childs = arrayListOf<JsonNode>()
        }

        constructor(name: String, type: FieldType, node: JsonNode?) : this(name, type) {
            subItemNode = node
        }

        enum class FieldType { ftValue, ftObject, ftList, ftMap }
    }

}