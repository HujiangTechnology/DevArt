package com.hujiang.devart.component.pulltorefresh

import android.os.Bundle
import android.view.LayoutInflater

/**
 * Created by rarnu on 3/31/16.
 */
class PullToRefreshListFragment: PullToRefreshBaseListFragment<PullToRefreshListView>() {
    override fun onCreatePullToRefreshListView(inflater: LayoutInflater?, savedInstanceState: Bundle?): PullToRefreshListView =
            PullToRefreshListView(activity)
}