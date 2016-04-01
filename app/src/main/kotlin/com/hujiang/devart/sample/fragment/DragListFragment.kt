package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.widget.ArrayAdapter
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.draglist.CombinedDragListener
import com.hujiang.devart.component.draglist.DragController
import com.hujiang.devart.component.draglist.DragListView
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 4/1/16.
 */
class DragListFragment: BaseFragment(), CombinedDragListener {

    private var _listview: DragListView? = null
    private var _controller: DragController? = null
    private var _adapter: ArrayAdapter<String>? = null
    private var _list: MutableList<String>? = null

    override fun getBarTitle(): Int = R.string.drag_listview_name

    override fun getBarTitleWithPath(): Int = R.string.drag_listview_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _listview = innerView?.findViewById(R.id.lvDrag) as DragListView
        _controller = DragController(_listview)
        _controller?.setDragHandleId(R.id.handlerDrag)
        _controller?.setRemoveEnabled(true)
        _controller?.setSortEnabled(true)
        _controller?.setDragInitMode(DragController.ON_DRAG)
        _controller?.setRemoveMode(DragController.FLING_REMOVE)
        _listview?.setFloatViewManager(_controller)
        _listview?.setDragEnabled(true)
        _listview?.setFloatAlpha(0.5F)

        _list = arrayListOf<String>()
        for (i in 0..50 - 1) {
            _list?.add("Item ${i + 1}")
        }
        _adapter = ArrayAdapter<String>(activity, R.layout.item_adapter_handle, R.id.tvDragText, _list)
        _listview?.adapter = _adapter
    }

    override fun initEvents() {
        _listview?.setOnTouchListener(_controller)
        _listview?.setDropListener(this)
        _listview?.setRemoveListener(this)
    }

    override fun initLogic() {

    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_drag_listview

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun drop(from: Int, to: Int) {
        if (from != to) {
            val item = _adapter?.getItem(from)
            _adapter?.remove(item)
            _adapter?.insert(item, to)
        }
    }

    override fun drag(from: Int, to: Int) { }

    override fun remove(which: Int) {
        _adapter?.remove(_adapter?.getItem(which))
    }
}