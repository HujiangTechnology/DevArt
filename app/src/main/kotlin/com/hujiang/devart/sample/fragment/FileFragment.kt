package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.Button
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.FileUtils

/**
 * Created by rarnu on 3/29/16.
 */
class FileFragment: BaseFragment(), View.OnClickListener {


    private var _btnWrite: Button? = null
    private var _btnRead: Button? = null
    private var _btnReadAssets: Button? = null
    private var _tvText: TextView? = null

    override fun getBarTitle(): Int = R.string.file_name

    override fun getBarTitleWithPath(): Int = R.string.file_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _btnWrite = innerView?.findViewById(R.id.btnWrite) as Button
        _btnRead = innerView?.findViewById(R.id.btnRead) as Button
        _btnReadAssets = innerView?.findViewById(R.id.btnReadAssets) as Button
        _tvText = innerView?.findViewById(R.id.tvText) as TextView
    }

    override fun initEvents() {
        _btnWrite?.setOnClickListener(this)
        _btnRead?.setOnClickListener(this)
        _btnReadAssets?.setOnClickListener(this)
    }

    override fun initLogic() { }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_file

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.btnWrite -> FileUtils.rewriteFile("/sdcard/test.txt", "test")
            R.id.btnRead -> {
                try {
                    val list = FileUtils.readFile("/sdcard/test.txt")
                    _tvText?.text = FileUtils.fileList2string(list)
                } catch(e: Exception) {

                }
            }
            R.id.btnReadAssets -> {
                val s = FileUtils.readAssetFile(activity, "test_file")
                _tvText?.text = s
            }
        }
    }
}