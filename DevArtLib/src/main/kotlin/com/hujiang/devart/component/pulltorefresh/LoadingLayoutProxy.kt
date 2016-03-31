package com.hujiang.devart.component.pulltorefresh

import android.graphics.Typeface
import android.graphics.drawable.Drawable

/**
 * Created by rarnu on 3/30/16.
 */
class LoadingLayoutProxy: ILoadingLayout {

    private var _loadingLayouts: MutableSet<LoadingLayout>? = null

    constructor() {
        _loadingLayouts = hashSetOf<LoadingLayout>()
    }

    fun addLayout(layout: LoadingLayout?) {
        if (layout != null) {
            _loadingLayouts?.add(layout)
        }
    }

    override fun setLastUpdatedLabel(label: CharSequence?) {
        for (layout in _loadingLayouts!!) {
            layout.setLastUpdatedLabel(label)
        }
    }

    override fun setLoadingDrawable(drawable: Drawable?) {
        for (layout in _loadingLayouts!!) {
            layout.setLoadingDrawable(drawable)
        }
    }

    override fun setPullLabel(pullLabel: CharSequence?) {
        for (layout in _loadingLayouts!!) {
            layout.setPullLabel(pullLabel)
        }
    }

    override fun setRefreshingLabel(refreshingLabel: CharSequence?) {
        for (layout in _loadingLayouts!!) {
            layout.setRefreshingLabel(refreshingLabel)
        }
    }

    override fun setReleaseLabel(releaseLabel: CharSequence?) {
        for (layout in _loadingLayouts!!) {
            layout.setReleaseLabel(releaseLabel)
        }
    }

    override fun setTextTypeface(tf: Typeface?) {
        for (layout in _loadingLayouts!!) {
            layout.setTextTypeface(tf)
        }
    }
}