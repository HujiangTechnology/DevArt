package com.hujiang.devart.sample.fragment

import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.text.Html
import android.view.Menu
import android.view.View
import android.widget.Button
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.flowtext.FlowTextView
import com.hujiang.devart.component.flowtext.OnLinkClickListener
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 4/15/16.
 */
class FlowTextFragment: BaseFragment(), View.OnClickListener, OnLinkClickListener {

    private var defaultFontSize = 0.0f
    private var _ftv: FlowTextView? = null
    private var _btnBigger: Button? = null
    private var _btnSmaller: Button? = null
    private var _btnReset: Button? = null

    override fun getBarTitle(): Int = R.string.flowtext_name

    override fun getBarTitleWithPath(): Int = R.string.flowtext_name_with_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _ftv = innerView?.findViewById(R.id.ftv) as FlowTextView
        defaultFontSize = _ftv!!.getTextSize()
        val content = getString(R.string.lorem)
        val html = Html.fromHtml(content)
        _ftv?.setText(html)
        _btnBigger = innerView?.findViewById(R.id.btnBigger) as Button
        _btnSmaller = innerView?.findViewById(R.id.btnSmaller) as Button
        _btnReset = innerView?.findViewById(R.id.btnReset) as Button
    }

    override fun initEvents() {
        _ftv?.setOnLinkClickListener(this)
        _btnBigger?.setOnClickListener(this)
        _btnSmaller?.setOnClickListener(this)
        _btnReset?.setOnClickListener(this)
    }

    override fun initLogic() {

    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_flowtext

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View?) {
        var currentFontSize = _ftv!!.getTextSize()
        when(v!!.id) {
            R.id.btnBigger -> currentFontSize++
            R.id.btnSmaller -> currentFontSize--
            R.id.btnReset -> currentFontSize = defaultFontSize
        }
        _ftv?.setTextSize(currentFontSize)
    }

    override fun onLinkClick(url: String?) {
        val inOpen = Intent(Intent.ACTION_VIEW, Uri.parse(url))
        startActivity(inOpen)
    }
}