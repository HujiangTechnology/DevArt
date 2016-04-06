package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.Menu
import android.widget.TextView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.security.AlgorithmUtils
import com.hujiang.devart.utils.MessageUtils
import kotlin.concurrent.thread

/**
 * Created by rarnu on 4/5/16.
 */
class AlgorithmFragment: BaseFragment() {

    private var _tvMd5: TextView? = null
    private var _tvSha1: TextView? = null
    private var _tvRSA: TextView? = null
    private var _tvDSA: TextView? = null

    override fun getBarTitle(): Int = R.string.algorithm_name

    override fun getBarTitleWithPath(): Int = R.string.algorithm_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _tvMd5 = innerView?.findViewById(R.id.tvMd5) as TextView
        _tvSha1 = innerView?.findViewById(R.id.tvSha1) as TextView
        _tvRSA = innerView?.findViewById(R.id.tvRSA) as TextView
        _tvDSA = innerView?.findViewById(R.id.tvDSA) as TextView
    }

    override fun initEvents() {

    }

    private val hMd5 = object : Handler() {
        override fun handleMessage(msg: Message?) {
            _tvMd5?.text = msg!!.obj as String
            super.handleMessage(msg)
        }
    }

    private val hSha1 = object : Handler() {
        override fun handleMessage(msg: Message?) {
            _tvSha1?.text = msg!!.obj as String
            super.handleMessage(msg)
        }
    }

    private val hRSA = object : Handler() {
        override fun handleMessage(msg: Message?) {
            _tvRSA?.text = msg!!.obj as String
            super.handleMessage(msg)
        }
    }

    private val hDSA = object : Handler() {
        override fun handleMessage(msg: Message?) {
            _tvDSA?.text = msg!!.obj as String
            super.handleMessage(msg)
        }
    }

    override fun initLogic() {
        val str = "abcdefg"
        thread {
            val retMd5 = AlgorithmUtils.md5EncryptString(str)
            val s = "MD5 Encrypt: ${retMd5}"
            MessageUtils.sendMessage(hMd5, 0, 0, 0, s)
        }
        thread {
            val retSha1 = AlgorithmUtils.sha1EncryptString(str)
            val s = "SHA1 Encrypt: ${retSha1}"
            MessageUtils.sendMessage(hSha1, 0, 0, 0, s)
        }
        thread {
            var s = ""
            val privPath = "/sdcard/rsa.priv"
            val pubPath = "/sdcard/rsa.pub"
            val retGK = AlgorithmUtils.rsaGenerateKeys(0, "", "", pubPath, privPath)
            if (retGK == 0) {
                s += "RSA Generate Key OK\n"
                val retEncrypt = AlgorithmUtils.rsaEncryptString(0, "", pubPath, str)
                s += "RSA Encrypted: ${retEncrypt}\n"
                val retDecrypt = AlgorithmUtils.rsaDecryptString(0, "", privPath, retEncrypt)
                s += "RSA Decrypted: ${retDecrypt}\n"
            } else {
                s += "RSA Generate Key Failed\n"
            }
            MessageUtils.sendMessage(hRSA, 0, 0, 0, s)
        }
        thread {
            var s = ""
            val privPath = "/sdcard/dsa.priv"
            val pubPath = "/sdcard/dsa.pub"
            val retGK = AlgorithmUtils.dsaGenerateKeys(0, "", "", pubPath, privPath)
            if (retGK == 0) {
                s += "DSA Generate Key OK\n"
                val retSign = AlgorithmUtils.dsaSignString(0, "", privPath, str)
                s += "DSA Sign: ${retSign}\n"
                val retVerify = AlgorithmUtils.dsaVerifyString(0, "", pubPath, retSign, str)
                s += "DSA Verify: ${if (retVerify == 0) "TRUE" else "FALSE"}"
            } else {
                s += "DSA Generate Key Failed\n"
            }
            MessageUtils.sendMessage(hDSA, 0, 0, 0, s)
        }
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_algorithm

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

}