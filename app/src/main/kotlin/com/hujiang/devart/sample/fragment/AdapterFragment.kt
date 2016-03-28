package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import android.widget.ListView
import android.widget.SearchView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.sample.adapter.StringAdapter
import com.hujiang.devart.sample.loader.StringLoader

/**
 * Created by rarnu on 3/27/16.
 */
class AdapterFragment: BaseFragment(), SearchView.OnQueryTextListener {

    private var _lvAdapter: ListView? = null
    private var _adapter: StringAdapter? = null
    private var _list: MutableList<String>? = null
    private var _loader: StringLoader? = null
    private var _itemSearch: MenuItem? = null

    override fun getBarTitle(): Int = R.string.adapter_name

    override fun getBarTitleWithPath(): Int = R.string.adapter_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _lvAdapter = innerView?.findViewById(R.id.lvAdapter) as ListView
        _list = arrayListOf<String>()
        _adapter = StringAdapter(activity, _list)
        _lvAdapter?.adapter = _adapter
        _loader = StringLoader(activity)
    }

    override fun initEvents() {
        _loader?.registerListener(0, { loader, data ->
            _list?.clear()
            if (data != null) {
                _list?.addAll(data)
            }
            _adapter?.setNewList(_list)
        })
    }

    override fun initLogic() {
        _loader?.startLoading()
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_adapter

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) {
        _itemSearch = menu?.add(0, 3, 98, "Search")
        _itemSearch?.setIcon(android.R.drawable.ic_menu_search)
        _itemSearch?.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM)
        val sv = SearchView(activity)
        sv.setOnQueryTextListener(this)
        _itemSearch?.actionView = sv
    }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onQueryTextSubmit(query: String?): Boolean = false

    override fun onQueryTextChange(newText: String?): Boolean {
        _adapter?.filter(newText)
        return true
    }
}