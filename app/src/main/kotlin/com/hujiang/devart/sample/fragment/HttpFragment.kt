package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.Menu
import android.view.View
import android.widget.Button
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.HttpUtils
import com.hujiang.devart.utils.MessageUtils
import kotlin.concurrent.thread

/**
 * Created by rarnu on 3/29/16.
 */
class HttpFragment : BaseFragment(), View.OnClickListener {


    private var _btnRequest: Button? = null
    private var _tvResult: TextView? = null

    override fun getBarTitle(): Int = R.string.http_request_name

    override fun getBarTitleWithPath(): Int = R.string.http_request_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _btnRequest = innerView?.findViewById(R.id.btnRequest) as Button
        _tvResult = innerView?.findViewById(R.id.tvResult) as TextView
    }

    override fun initEvents() {
        _btnRequest?.setOnClickListener(this)
    }

    override fun initLogic() {
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_http_request

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) {
    }

    override fun onGetNewArguments(bn: Bundle?) {
    }

    override fun getFragmentState(): Bundle? = null

    private val _hRequest = object : Handler() {
        override fun handleMessage(msg: Message?) {
            if (msg!!.what == 1) {
                _tvResult?.text = msg.obj as String?
            }
            super.handleMessage(msg)
        }
    }

    override fun onClick(v: View?) {
        thread {
            val ret = HttpUtils.get("http://www.hujiang.com", "")
            MessageUtils.sendMessage(_hRequest, 1, 0, 0, ret)
        }
    }
}