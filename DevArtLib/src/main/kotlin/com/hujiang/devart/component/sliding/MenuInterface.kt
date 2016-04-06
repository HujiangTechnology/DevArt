package com.hujiang.devart.component.sliding

import android.graphics.Canvas
import android.graphics.drawable.Drawable
import android.view.View

/**
 * Created by rarnu on 4/5/16.
 */
interface MenuInterface {

    fun scrollBehindTo(x: Int, y: Int, cvb: CustomViewBehind?, scrollScale: Float)
    fun getMenuLeft(cvb: CustomViewBehind?, content: View?): Int
    fun getAbsLeftBound(cvb: CustomViewBehind?, content: View?): Int
    fun getAbsRightBound(cvb: CustomViewBehind?, content: View?): Int
    fun marginTouchAllowed(content: View?, x: Int, threshold: Int): Boolean
    fun menuOpenTouchAllowed(content: View?, currPage: Int, x: Int): Boolean
    fun menuTouchInQuickReturn(content: View?, currPage: Int, x: Int): Boolean
    fun menuClosedSlideAllowed(x: Int): Boolean
    fun menuOpenSlideAllowed(x: Int): Boolean
    fun drawShadow(canvas: Canvas?, shadow: Drawable?, width: Int)
    fun drawFade(canvas: Canvas?, alpha: Int, cvb: CustomViewBehind?, content: View?)
    fun drawSelector(content: View?, canvas: Canvas?, percentOpen: Float)

}