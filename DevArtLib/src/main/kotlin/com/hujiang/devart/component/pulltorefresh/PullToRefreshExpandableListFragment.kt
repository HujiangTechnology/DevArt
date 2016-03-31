package com.hujiang.devart.component.pulltorefresh

import android.os.Bundle
import android.view.LayoutInflater

/**
 * Created by rarnu on 3/31/16.
 */
class PullToRefreshExpandableListFragment: PullToRefreshBaseListFragment<PullToRefreshExpandableListView>() {

    override fun onCreatePullToRefreshListView(inflater: LayoutInflater?, savedInstanceState: Bundle?): PullToRefreshExpandableListView =
            PullToRefreshExpandableListView(activity)
}