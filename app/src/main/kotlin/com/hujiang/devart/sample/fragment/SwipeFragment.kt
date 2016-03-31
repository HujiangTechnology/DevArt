package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.widget.ListView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.sample.adapter.SwipeAdapter

/**
 * Created by rarnu on 3/31/16.
 */
class SwipeFragment: BaseFragment() {

    private var _lv: ListView? = null
    private var _adapter: SwipeAdapter? = null
    private var _list: MutableList<String>? = null

    override fun getBarTitle(): Int = R.string.swipe_name

    override fun getBarTitleWithPath(): Int = R.string.swipe_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _lv = innerView?.findViewById(R.id.lv) as ListView
        _list = arrayListOf("a","b","c","d", "e", "f", "g")
        _adapter = SwipeAdapter(activity, _list)
        _lv?.adapter = _adapter
    }

    override fun initEvents() {

    }

    override fun initLogic() {

    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_swipe

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

}