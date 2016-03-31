package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.util.AttributeSet
import android.webkit.WebView
import java.util.concurrent.atomic.AtomicBoolean

/**
 * Created by rarnu on 3/31/16.
 */
class PullToRefreshWebView2: PullToRefreshWebView {

    companion object {
        val JS_INTERFACE_PKG = "ptr"
        val DEF_JS_READY_PULL_DOWN_CALL = "javascript:isReadyForPullDown();"
        val DEF_JS_READY_PULL_UP_CALL = "javascript:isReadyForPullUp();"
    }

    private var _jsCallback: JsValueCallback? = null
    private val _isReadyForPullDown = AtomicBoolean(false)
    private val _isReadyForPullUp = AtomicBoolean(false)

    constructor(context: Context): super(context)

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

    constructor(context: Context, mode: Mode): super(context, mode)

    override fun createRefreshableView(context: Context, attrs: AttributeSet?): WebView {
        val webView = super.createRefreshableView(context, attrs)
        _jsCallback = JsValueCallback()
        webView.addJavascriptInterface(_jsCallback, JS_INTERFACE_PKG)
        return webView
    }

    override fun isReadyForPullStart(): Boolean {
        getRefreshableView()?.loadUrl(DEF_JS_READY_PULL_DOWN_CALL)
        return _isReadyForPullDown.get()
    }

    override fun isReadyForPullEnd(): Boolean {
        getRefreshableView()?.loadUrl(DEF_JS_READY_PULL_UP_CALL)
        return _isReadyForPullUp.get()
    }

    inner class JsValueCallback {
        fun isReadyForPullUpResponse(response: Boolean) {
            _isReadyForPullUp.set(response)
        }
        fun isReadyForPullDownResponse(response: Boolean) {
            _isReadyForPullDown.set(response)
        }
    }
}