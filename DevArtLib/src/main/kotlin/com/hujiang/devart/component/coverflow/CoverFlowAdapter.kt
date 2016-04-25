package com.hujiang.devart.component.coverflow

import android.content.Context
import android.view.View
import android.view.ViewGroup
import android.widget.ImageView
import android.widget.LinearLayout
import android.widget.TextView
import com.hujiang.devart.R
import com.hujiang.devart.component.recycler.RecyclerView
import com.nostra13.universalimageloader.core.DisplayImageOptions
import com.nostra13.universalimageloader.core.ImageLoader
import com.nostra13.universalimageloader.core.ImageLoaderConfiguration
import com.nostra13.universalimageloader.core.display.RoundedBitmapDisplayer
import java.util.*

/**
 * Created by rarnu on 4/19/16.
 */
class CoverFlowAdapter : RecyclerView.Adapter<CoverFlowAdapter.ViewHolder> {

    private var _cardModels: LinkedList<CardModel?>? = null
    private var _borderPosition = 0
    private var _loader: ImageLoader? = null
    private var _displayImageOptions: DisplayImageOptions? = null

    constructor(context: Context, cardModels: LinkedList<CardModel?>?) {
        _cardModels = cardModels
        _loader = ImageLoader.getInstance()
        _loader?.init(ImageLoaderConfiguration.createDefault(context))
        _displayImageOptions = DisplayImageOptions.Builder().displayer(RoundedBitmapDisplayer(1000)).build()
    }

    override fun onCreateViewHolder(parent: ViewGroup?, viewType: Int): ViewHolder? {
        val view = View.inflate(parent?.context, R.layout.card_item, null)
        return ViewHolder(view)
    }

    override fun onBindViewHolder(holder: ViewHolder?, position: Int) {
        holder?.cardLayout?.visibility = View.VISIBLE
        showPic(holder?.cardImage, "drawable://${_cardModels!![position]!!.getImg()}")
        if (!_cardModels!![position]!!.isBorder) {
            holder?.cardTitle?.text = _cardModels!![position]!!.getTitle()
        }
        if (position < _borderPosition || position > getItemCount() - _borderPosition - 1) {
            holder?.cardLayout?.visibility = View.GONE
        }
    }

    fun setBorderPosition(borderPosition: Int) {
        _borderPosition = borderPosition
        for (i in 0..borderPosition - 1) {
            _cardModels?.addFirst(CardModel(true))
            _cardModels?.addLast(CardModel(true))
        }
        notifyDataSetChanged()
    }

    private fun showPic(imgView: ImageView?, url: String?) {
        if (url == null) {
            imgView?.visibility = View.GONE
        } else {
            if (imgView != null && url != "drawable://0") {
                _loader?.displayImage(url, imgView, _displayImageOptions)
            }
        }
    }

    override fun getItemCount(): Int = _cardModels!!.size

    class ViewHolder : RecyclerView.ViewHolder {
        var cardLayout: LinearLayout? = null
        var cardImage: ImageView? = null
        var cardTitle: TextView? = null
        constructor(v: View?) : super(v) {
            cardLayout = v?.findViewById(R.id.cardLayout) as LinearLayout
            cardImage = v?.findViewById(R.id.cardImg) as ImageView
            cardTitle = v?.findViewById(R.id.cardTitle) as TextView
        }
    }

}