package com.hujiang.devart.base

import com.hujiang.devart.base.inner.InnerFragment

/**
 * Created by rarnu on 3/25/16.
 */
abstract class BasePopupFragment: InnerFragment {

    constructor(): super()

    constructor(tabTitle: String): super(tabTitle)

}