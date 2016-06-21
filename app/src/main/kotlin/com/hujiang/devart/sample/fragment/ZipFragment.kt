package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.util.Log
import android.view.Menu
import android.view.View
import android.widget.Button
import android.widget.Toast
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.ZipUtils
import java.io.File
import kotlin.concurrent.thread

/**
 * Created by rarnu on 4/2/16.
 */
class ZipFragment: BaseFragment(), View.OnClickListener {


    private var _btnZip: Button? = null
    private var _btnUnzip: Button? = null

    override fun getBarTitle(): Int = R.string.zip_name

    override fun getBarTitleWithPath(): Int = R.string.zip_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _btnZip = innerView?.findViewById(R.id.btnZip) as Button
        _btnUnzip = innerView?.findViewById(R.id.btnUnzip) as Button
    }

    override fun initEvents() {
        _btnZip?.setOnClickListener(this)
        _btnUnzip?.setOnClickListener(this)
    }

    override fun initLogic() {
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_zip

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View?) {
        when(v!!.id) {
            R.id.btnZip -> threadZip()
            R.id.btnUnzip -> threadUnzip()
        }
    }

    private val hZip = object : Handler() {
        override fun handleMessage(msg: Message?) {
            Log.e("handleMessage", "msg.what => ${msg!!.what}")
            Toast.makeText(activity, if (msg.what == 0) {
                val c = msg.obj as ZipUtils.CompressStatus
                "compress => msg: ${c.errMsg}, files: ${c.fileCount}, compress: ${c.compressCount}"
            } else {
                val uc = msg.obj as ZipUtils.UncompressStatus
                "uncompress => msg: ${uc.errMsg}, files: ${uc.fileCount}, uncompress: ${uc.uncompressCount}"
            }, Toast.LENGTH_SHORT).show()
            super.handleMessage(msg)
        }
    }

    private fun threadZip() {
        thread {
            val dest = "/sdcard/test.zip"
            val dir = File("/sdcard/test/")
            val errCode = ZipUtils.compress(dest, dir.absolutePath)
            Log.e("threadZip", "Error Code => $errCode")
            val size = ZipUtils.getFileSize(dest)
            Log.e("threadZip", "File Size => $size")
            val status = ZipUtils.getCompressStatus(dest)
            val msg = Message.obtain(hZip, 0, status)
            hZip.sendMessage(msg)
            msg.recycle()
        }
    }

    private fun threadUnzip() {
        thread {
            var src = File("/sdcard/test.zip")
            val dest = File("/sdcard/unzip/")
            if (!dest.exists()) {
                dest.mkdirs()
            }
            val size = ZipUtils.getFileSize(src.absolutePath)
            Log.e("threadUnzip", "File Size => $size")
            val errCode = ZipUtils.uncompress(src.absolutePath, dest.absolutePath)
            Log.e("threadUnzip", "Error Code => $errCode")
            val status = ZipUtils.getUncompressStatus(src.absolutePath)
            val msg = Message.obtain(hZip, 1, status)
            hZip.sendMessage(msg)
            msg.recycle()
        }
    }
}