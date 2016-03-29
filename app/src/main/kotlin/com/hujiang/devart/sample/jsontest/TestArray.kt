package com.hujiang.devart.sample.jsontest

/**
 * Created by rarnu on 3/29/16.
 */
class TestArray {

    var arr = 0

    constructor() { }

    constructor(arr: Int) { this.arr = arr }

    override fun toString(): String = "{arr:${arr}}"

}