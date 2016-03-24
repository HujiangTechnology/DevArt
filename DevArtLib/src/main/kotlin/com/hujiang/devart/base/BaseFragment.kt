package com.hujiang.devart.base

import com.hujiang.devart.base.inner.InnerFragment

/**
 * Created by rarnu on 3/23/16.
 */
abstract class BaseFragment: InnerFragment {

    constructor(): super()

    constructor(tabTitle: String): super(tabTitle)

    constructor(tagText: String, tabTitle: String): super(tabTitle)

}