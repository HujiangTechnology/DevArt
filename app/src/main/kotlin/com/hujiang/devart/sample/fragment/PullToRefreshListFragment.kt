package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.Menu
import android.widget.ListView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.pulltorefresh.PullToRefreshBase
import com.hujiang.devart.component.pulltorefresh.PullToRefreshListView
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.sample.adapter.StringAdapter
import com.hujiang.devart.sample.loader.StringLoader
import java.util.*
import kotlin.concurrent.thread

/**
 * Created by rarnu on 3/31/16.
 */
class PullToRefreshListFragment: BaseFragment(), PullToRefreshBase.OnRefreshListener2<ListView> {


    private var _lv: PullToRefreshListView? = null
    private var _adapter: StringAdapter? = null
    private var _list: MutableList<String>? = null
    private var _loader: StringLoader? = null

    override fun getBarTitle(): Int = R.string.pulldown_listview_name

    override fun getBarTitleWithPath(): Int = R.string.pulldown_listview_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _lv = innerView?.findViewById(R.id.lv) as PullToRefreshListView
        _list = arrayListOf<String>()
        _adapter = StringAdapter(activity, _list)
        _lv?.setAdapter(_adapter)
        _loader = StringLoader(activity)
        _lv?.setMode(PullToRefreshBase.Mode.BOTH)
    }

    override fun initEvents() {
        _loader?.registerListener(0, { loader, data ->
            _list?.clear()
            if (data != null) {
                _list?.addAll(data)
            }
            _adapter?.setNewList(_list)
        })
        _lv?.setOnRefreshListener(this)
    }

    override fun initLogic() {
        _loader?.startLoading()
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_pulltorefreshlist

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    private val _hPull = object: Handler() {
        override fun handleMessage(msg: Message?) {
            val s = Random().nextInt().toString()
            when (msg!!.what) {
                0 -> _list?.add(0, s)
                1 -> _list?.add(s)
            }
            _adapter?.setNewList(_list)
            _lv?.onRefreshComplete()
            super.handleMessage(msg)
        }
    }

    override fun onPullDownToRefresh(refreshView: PullToRefreshBase<ListView>?) {
        thread {
            Thread.sleep(1000)
            _hPull.sendEmptyMessage(0)
        }
    }

    override fun onPullUpToRefresh(refreshView: PullToRefreshBase<ListView>?) {
        thread {
            Thread.sleep(1000)
            _hPull.sendEmptyMessage(1)
        }
    }
}