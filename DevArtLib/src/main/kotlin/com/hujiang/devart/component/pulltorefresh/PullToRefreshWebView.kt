package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.os.Build
import android.os.Bundle
import android.util.AttributeSet
import android.util.FloatMath
import android.webkit.WebChromeClient
import android.webkit.WebView
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/31/16.
 */
open class PullToRefreshWebView: PullToRefreshBase<WebView> {

    companion object {
        val OVERSCROLL_FUZZY_THRESHOLD = 2
        val OVERSCROLL_SCALE_FACTOR = 1.5f

        private val defaultOnRefreshListener = object : OnRefreshListener<WebView> {
            override fun onRefresh(refreshView: PullToRefreshBase<WebView>?) {
                refreshView?.getRefreshableView()?.reload()
            }
        }
    }

    private val defaultWebChromeClient = object : WebChromeClient() {
        override fun onProgressChanged(view: WebView?, newProgress: Int) {
            if (newProgress == 100) {
                onRefreshComplete()
            }
        }
    }

    constructor(context: Context): super(context) {
        setOnRefreshListener(defaultOnRefreshListener)
        _refreshableView?.setWebChromeClient(defaultWebChromeClient)
    }

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        setOnRefreshListener(defaultOnRefreshListener)
        _refreshableView?.setWebChromeClient(defaultWebChromeClient)
    }

    constructor(context: Context, mode: Mode): super(context, mode) {
        setOnRefreshListener(defaultOnRefreshListener)
        _refreshableView?.setWebChromeClient(defaultWebChromeClient)
    }

    constructor(context: Context, mode: Mode, style: AnimationStyle): super(context, mode, style) {
        setOnRefreshListener(defaultOnRefreshListener)
        _refreshableView?.setWebChromeClient(defaultWebChromeClient)
    }

    override fun getPullToRefreshScrollDirection(): Orientation = Orientation.VERTICAL

    override fun createRefreshableView(context: Context, attrs: AttributeSet?): WebView {
        var webView: WebView
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.GINGERBREAD) {
            webView = InternalWebViewSDK9(context, attrs)
        } else {
            webView = WebView(context, attrs)
        }
        webView.id = R.id.webview
        return webView
    }

    @Suppress("DEPRECATION")
    override fun isReadyForPullEnd(): Boolean {
        val exactContentHeight = FloatMath.floor(_refreshableView!!.contentHeight * _refreshableView!!.scale)
        return _refreshableView!!.scrollY >= (exactContentHeight - _refreshableView!!.height)
    }

    override fun isReadyForPullStart(): Boolean = _refreshableView!!.scrollY == 0

    override fun onPtrRestoreInstanceState(savedInstanceState: Bundle?) {
        super.onPtrRestoreInstanceState(savedInstanceState)
        _refreshableView?.restoreState(savedInstanceState)
    }

    override fun onPtrSaveInstanceState(saveState: Bundle?) {
        super.onPtrSaveInstanceState(saveState)
        _refreshableView?.saveState(saveState)
    }

    inner class InternalWebViewSDK9: WebView {

        constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

        override fun overScrollBy(deltaX: Int, deltaY: Int, scrollX: Int, scrollY: Int, scrollRangeX: Int, scrollRangeY: Int, maxOverScrollX: Int, maxOverScrollY: Int, isTouchEvent: Boolean): Boolean {
            val returnValue = super.overScrollBy(deltaX, deltaY, scrollX, scrollY, scrollRangeX, scrollRangeY, maxOverScrollX, maxOverScrollY, isTouchEvent)
            OverscrollHelper.overScrollBy(this@PullToRefreshWebView, deltaX, scrollX, deltaY, scrollY, getScrollRange(), OVERSCROLL_FUZZY_THRESHOLD, OVERSCROLL_SCALE_FACTOR, isTouchEvent)
            return returnValue
        }

        @Suppress("DEPRECATION")
        private fun getScrollRange(): Int = Math.max(0.0f, FloatMath.floor(_refreshableView!!.contentHeight * _refreshableView!!.scale)- (height - paddingBottom - paddingTop)).toInt()
    }
}