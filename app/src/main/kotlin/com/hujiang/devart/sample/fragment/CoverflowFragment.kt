package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.Button
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.coverflow.CoverFlowView
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.sample.adapter.ImageCoverFlowAdapter

/**
 * Created by rarnu on 4/26/16.
 */
class CoverflowFragment: BaseFragment(), View.OnClickListener, CoverFlowView.CoverFlowListener<ImageCoverFlowAdapter>, CoverFlowView.TopImageLongClickListener {

    private var _coverFlowView: CoverFlowView<ImageCoverFlowAdapter>? = null
    private var _adapter: ImageCoverFlowAdapter? = null
    private var _btnChange: Button? = null

    override fun getBarTitle(): Int = R.string.coverflow_name

    override fun getBarTitleWithPath(): Int = R.string.coverflow_name_with_path

    override fun getCustomTitle(): String? = null

    @Suppress("UNCHECKED_CAST")
    override fun initComponents() {
        _coverFlowView = innerView?.findViewById(R.id.coverflow) as CoverFlowView<ImageCoverFlowAdapter>
        _adapter = ImageCoverFlowAdapter(activity)
        _coverFlowView?.setAdapter(_adapter)

        _btnChange = innerView?.findViewById(R.id.btnChange) as Button


    }

    override fun initEvents() {
        _btnChange?.setOnClickListener(this)
        _coverFlowView?.setCoverFlowListener(this)
        _coverFlowView?.setTopImageLongClickListener(this)
    }

    override fun initLogic() {

    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_coverflow

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View?) {
        _adapter?.changeBitmap()
    }

    override fun imageOnTop(coverFlowView: CoverFlowView<ImageCoverFlowAdapter>?, position: Int, left: Float, top: Float, right: Float, bottom: Float) { }

    override fun topImageClicked(coverFlowView: CoverFlowView<ImageCoverFlowAdapter>?, position: Int) { }

    override fun invalidationCompleted() { }

    override fun onLongClick(position: Int) { }
}