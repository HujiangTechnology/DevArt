package com.hujiang.devart.sample.jsontest

/**
 * Created by rarnu on 3/29/16.
 */
class TestMap {

    var map = 0

    constructor() { }

    constructor(map: Int) { this.map = map }

    override fun toString(): String = "{map=${map}}"

}