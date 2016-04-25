package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.coverflow.CardModel
import com.hujiang.devart.component.coverflow.CoverFlowAdapter
import com.hujiang.devart.component.coverflow.CoverFlowView
import com.hujiang.devart.component.coverflow.RecyclerItemClickListener
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import java.util.*

/**
 * Created by rarnu on 4/19/16.
 */
class CoverFlowFragment: BaseFragment(), CoverFlowView.CoverFlowItemListener {


    private var _coverFlowView: CoverFlowView? = null
    private var _coverFlowAdapter: CoverFlowAdapter? = null
    private var _text: TextView? = null
    private var _cardModels: LinkedList<CardModel?>? = null

    override fun getBarTitle(): Int = R.string.coverflow_name

    override fun getBarTitleWithPath(): Int = R.string.coverflow_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _coverFlowView = innerView?.findViewById(R.id.cf) as CoverFlowView
        _text = innerView?.findViewById(R.id.text) as TextView
        _cardModels = LinkedList()
        _cardModels?.add(CardModel("1.Alligator", R.drawable.alligator.toString()))
        _cardModels?.add(CardModel("2.Beaver", R.drawable.beaver.toString()))
        _cardModels?.add(CardModel("3.Frog", R.drawable.frog.toString()))
        _cardModels?.add(CardModel("4.Kangaroo", R.drawable.kangaroo.toString()))
        _cardModels?.add(CardModel("5.Leopard", R.drawable.leopard.toString()))
        _cardModels?.add(CardModel("6.Snail", R.drawable.snail.toString()))
        _cardModels?.add(CardModel("7.Wolf", R.drawable.wolf.toString()))
        _cardModels?.add(CardModel("8.Monkey", R.drawable.monkey.toString()))
        _cardModels?.add(CardModel("9.Tiger", R.drawable.tiger.toString()))
        _coverFlowView?.setOrientation(CoverFlowView.HORIZONTAL)
        _coverFlowAdapter = CoverFlowAdapter(activity, _cardModels)
        _coverFlowView?.setAdapter(_coverFlowAdapter)
        _coverFlowView?.getLayoutManager()?.scrollToPosition(_coverFlowAdapter!!.getItemCount() / 2)
        onItemSelected(_coverFlowAdapter!!.getItemCount() / 2)
        _coverFlowAdapter?.notifyDataSetChanged()
    }

    override fun initEvents() {
        _coverFlowView?.setCoverFlowListener(this)
        _coverFlowView?.addOnItemTouchListener(RecyclerItemClickListener(activity, object : RecyclerItemClickListener.OnItemClickListener {
            override fun onItemClick(view: View?, position: Int) {
                _coverFlowView?.scrollToCenter(position)
            }
        }))
    }

    override fun initLogic() {

    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_coverflow

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onItemChanged(position: Int) { }

    override fun onItemSelected(position: Int) {
        _text?.text = _cardModels!![position]!!.getTitle()
    }
}