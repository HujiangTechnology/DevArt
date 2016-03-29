package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.Menu
import android.view.View
import android.widget.Button
import android.widget.ImageView
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.base.common.Actions
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.DownloadUtils
import java.io.File

/**
 * Created by rarnu on 3/29/16.
 */
class DownloadFragment: BaseFragment(), View.OnClickListener {

    private var _iv: ImageView? = null
    private var _tvProgress: TextView? = null
    private var _btnDownload: Button? = null

    override fun getBarTitle(): Int = R.string.download_name

    override fun getBarTitleWithPath(): Int = R.string.download_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _iv = innerView?.findViewById(R.id.iv) as ImageView
        _tvProgress = innerView?.findViewById(R.id.tvProgress) as TextView
        _btnDownload = innerView?.findViewById(R.id.btnDownload) as Button
    }

    override fun initEvents() {
        _btnDownload?.setOnClickListener(this)
    }

    override fun initLogic() { }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_download

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    private val _hProgress = object : Handler() {
        override fun handleMessage(msg: Message?) {
            when(msg!!.what) {
                Actions.WHAT_DOWNLOAD_START,
                Actions.WHAT_DOWNLOAD_PROGRESS -> _tvProgress?.text = "${msg.arg1}/${msg.arg2}"
                Actions.WHAT_DOWNLOAD_FINISH -> _tvProgress?.text = getString(R.string.ok)
            }
            super.handleMessage(msg)
        }
    }

    override fun onClick(v: View?) {
        val url = "http://res.a6.hjfile.cn/uploads/258afdb7-8bfd-4d99-9059-7c92bbf6dd1e.jpg"
        val localDir = "/sdcard/"
        val localFile = "test.jpg"
        val fLocal = File(localDir, localFile)
        if (fLocal.exists()) {
            fLocal.delete()
        }
        DownloadUtils.downloadFileT(activity, _iv, url, localDir, localFile, _hProgress)
    }
}