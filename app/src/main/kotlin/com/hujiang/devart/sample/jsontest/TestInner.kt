package com.hujiang.devart.sample.jsontest

/**
 * Created by rarnu on 3/29/16.
 */
class TestInner {

    var innerFA = "innerA"
    var innerFB = "innerB"
    var array = arrayListOf<TestArray>()
    var arrStr = arrayListOf<String>()
    val map = hashMapOf<String, String>()

    constructor() {
        for (i in 0..5 - 1) {
            array.add(TestArray(i))
            arrStr.add("item${i}")
            map.put("m${i}", (i * 2).toString())
        }
    }

    override fun toString(): String {
        var ret = "innerFA:${innerFA},innerFB:${innerFB},"
        ret += "arrStr:["
        for (i in 0..arrStr.size - 1) {
            ret += arrStr[i] + ","
        }
        ret += "],"
        ret += "array:["
        for (i in 0..array.size - 1) {
            ret += array[i].toString() + ","
        }
        ret += "],"
        ret += "map:["
        val iter = map.keys.iterator()
        var key: String
        while (iter.hasNext()) {
            key = iter.next()
            ret += "{${key}:${map[key].toString()}},"
        }
        ret += "]"
        return ret
    }

}