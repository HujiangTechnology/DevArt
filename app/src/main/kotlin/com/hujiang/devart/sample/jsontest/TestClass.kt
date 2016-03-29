package com.hujiang.devart.sample.jsontest

/**
 * Created by rarnu on 3/29/16.
 */
class TestClass {

    var fa = "a"
    var fb = "b"
    var testInner = TestInner()
    var test = hashMapOf<String, TestMap>()

    constructor() {
        for (i in 0..10 - 1) {
            test.put("test${i}", TestMap(i * 3))
        }
    }

    override fun toString(): String {
        var ret = "fa=${fa}, fb=${fb}, inner={${testInner.toString()}}, "
        ret += "test:["
        val iter = test.keys.iterator()
        var key: String
        while (iter.hasNext()) {
            key = iter.next()
            ret += "{${key}:${test[key].toString()}},"
        }
        ret += "]"
        return ret
    }


}
