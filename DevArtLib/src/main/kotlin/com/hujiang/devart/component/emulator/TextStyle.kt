package com.hujiang.devart.component.emulator

/**
 * Created by rarnu on 4/11/16.
 */
object  TextStyle {

    val fxNormal = 0
    val fxBold = 1
    val fxItalic = 1 shl 1
    val fxUnderline = 1 shl 2
    val fxBlink = 1 shl 3
    val fxInverse = 1 shl 4
    val fxInvisible = 1 shl 5
    val ciForeground = 256
    val ciBackground = 257
    val ciCursor = 258
    val ciColorLength = ciCursor + 1
    val kNormalTextStyle = encode(ciForeground, ciBackground, fxNormal)

    fun encode(foreColor: Int, backColor: Int, effect: Int): Int = ((effect and 0x3f) shl 18) or ((foreColor and 0x1ff) shl 9) or (backColor and 0x1ff)

    fun decodeForeColor(encodedColor: Int): Int = (encodedColor shr 9) and 0x1ff

    fun decodeBackColor(encodedColor: Int): Int = encodedColor and 0x1ff

    fun decodeEffect(encodedColor: Int): Int = (encodedColor shr 18) and 0x3f

}