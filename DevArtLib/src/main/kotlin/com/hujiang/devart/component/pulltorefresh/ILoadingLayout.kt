package com.hujiang.devart.component.pulltorefresh

import android.graphics.Typeface
import android.graphics.drawable.Drawable

/**
 * Created by rarnu on 3/30/16.
 */
interface ILoadingLayout {

    fun setLastUpdatedLabel(label: CharSequence?)

    fun setLoadingDrawable(drawable: Drawable?)

    fun setPullLabel(pullLabel: CharSequence?)

    fun setRefreshingLabel(refreshingLabel: CharSequence?)

    fun setReleaseLabel(releaseLabel: CharSequence?)

    fun setTextTypeface(tf: Typeface?)

}