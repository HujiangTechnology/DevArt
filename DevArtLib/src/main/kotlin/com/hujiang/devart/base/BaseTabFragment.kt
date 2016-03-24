package com.hujiang.devart.base

import android.app.ActionBar
import android.app.Fragment
import android.support.v4.view.ViewPager
import com.hujiang.devart.base.inner.InnerFragment

/**
 * Created by rarnu on 3/24/16.
 */
class BaseTabFragment: InnerFragment, ActionBar.TabListener {

    protected var bar: ActionBar? = null
    private var pager: ViewPager? = null
    private var adapter: BaseFragmentStateAdapter? = null
    private var listFragment: List<Fragment>? = null
    private var currentPage = 0
    private var needRelease = true

    constructor(): super()

    constructor(tabTitle: String): super(tabTitle)

    constructor(needRelease: Boolean): super() {
        this.needRelease = needRelease
    }



}