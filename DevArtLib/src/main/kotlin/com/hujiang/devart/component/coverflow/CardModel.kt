package com.hujiang.devart.component.coverflow

import java.io.Serializable

/**
 * Created by rarnu on 4/19/16.
 */
class CardModel: Serializable {

    private var _title: String? = null
    private var _img: String? = "0"
    var isBorder = false

    constructor(title: String?, img: String?) {
        _title = title
        _img = img
    }

    constructor(isBorder: Boolean) {
        this.isBorder = isBorder
    }

    fun getTitle(): String? = _title

    fun setTitle(title: String?) {
        _title = title
    }

    fun getImg(): String? = _img

    fun setImg(img: String?) {
        _img = img
    }

}
