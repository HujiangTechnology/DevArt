package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.flip.FlipView
import com.hujiang.devart.component.flip.OverFlipMode
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.sample.adapter.FlipAdapter

/**
 * Created by rarnu on 4/6/16.
 */
class FlipViewFragment: BaseFragment(), FlipAdapter.Callback, FlipView.OnFlipListener, FlipView.OnOverFlipListener {

    private var _flipView: FlipView? = null
    private var _adapter: FlipAdapter? = null
    private var _list: MutableList<FlipAdapter.Item>? = null
    private var _miAdd: MenuItem? = null

    override fun getBarTitle(): Int = R.string.flipview_name

    override fun getBarTitleWithPath(): Int = R.string.flipview_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _flipView = innerView?.findViewById(R.id.fv) as FlipView
        _list = arrayListOf()
        for (i in 0..5) {
            _list?.add(FlipAdapter.Item())
        }
        _adapter = FlipAdapter(activity, _list)
        _adapter?.setCallback(this)
        _flipView?.setAdapter(_adapter)
        _flipView?.setOnFlipListener(this)
        _flipView?.peakNext(false)
        _flipView?.setOverFlipMode(OverFlipMode.RUBBER_BAND)
        _flipView?.setEmptyView(innerView?.findViewById(R.id.tvEmpty))
        _flipView?.setOnOverFlipListener(this)
    }

    override fun initEvents() {

    }

    override fun initLogic() {

    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_flipview

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) {
        _miAdd = menu?.add(0, 1, 99, "Add")
        _miAdd?.setIcon(R.drawable.ic_menu_add)
        _miAdd?.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS)
    }

    override fun onOptionsItemSelected(item: MenuItem?): Boolean {
        when (item!!.itemId) {
            1 -> {
                _adapter?.addItemsBefore(5)
                return true
            }
        }
        return super.onOptionsItemSelected(item)
    }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onPageRequested(page: Int) {
        _flipView?.smoothFlipTo(page)
    }

    override fun onFlippedToPage(v: FlipView?, position: Int, id: Long) {
        if(position > _flipView!!.getPageCount() - 3 && _flipView!!.getPageCount() < 30){
            _adapter?.addItems(5)
        }
    }

    override fun onOverFlip(v: FlipView?, mode: OverFlipMode, overFlippingPrevious: Boolean, overFlipDistance: Float, flipDistancePerPage: Float) {

    }
}