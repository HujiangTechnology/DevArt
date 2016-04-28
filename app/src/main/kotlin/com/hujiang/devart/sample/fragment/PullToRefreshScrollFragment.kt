package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.Menu
import android.widget.ScrollView
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.pulltorefresh.PullToRefreshBase
import com.hujiang.devart.component.pulltorefresh.PullToRefreshScrollView
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import kotlin.concurrent.thread

/**
 * Created by rarnu on 3/31/16.
 */
class PullToRefreshScrollFragment: BaseFragment(), PullToRefreshBase.OnRefreshListener2<ScrollView> {


    private var _sv: PullToRefreshScrollView? = null
    private var _tv: TextView? = null

    override fun getBarTitle(): Int = R.string.pulldown_layout_name

    override fun getBarTitleWithPath(): Int = R.string.pulldown_layout_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _sv = innerView?.findViewById(R.id.sv) as PullToRefreshScrollView
        _tv = innerView?.findViewById(R.id.tv) as TextView
    }

    override fun initEvents() {
        _sv?.setOnRefreshListener(this)
    }

    override fun initLogic() {

    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_pulltorefreshscroll

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    private val _hPull = object : Handler() {
        override fun handleMessage(msg: Message?) {
            var txt = _tv?.text.toString()
            txt += if (msg!!.what == 0) "pull down\n" else "pull up\n"
            _tv?.text = txt
            _sv?.onRefreshComplete()
            super.handleMessage(msg)
        }
    }

    override fun onPullDownToRefresh(refreshView: PullToRefreshBase<ScrollView>?) {
        thread {
            Thread.sleep(1000)
            _hPull.sendEmptyMessage(0)
        }
    }

    override fun onPullUpToRefresh(refreshView: PullToRefreshBase<ScrollView>?) {
        thread {
            Thread.sleep(1000)
            _hPull.sendEmptyMessage(1)
        }
    }
}