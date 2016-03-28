package com.hujiang.devart.component.mutax

/**
 * Created by rarnu on 3/27/16.
 */
interface OnMutaxMessage {

    fun onMutaxStateChange(operating: Boolean)

    fun onMutaxProgress(name: String, position: Int, total: Int)

    fun onMutaxMessage(operating: Boolean)
}