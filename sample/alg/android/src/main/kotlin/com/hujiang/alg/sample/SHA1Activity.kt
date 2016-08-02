package com.hujiang.alg.sample

import android.os.Handler
import android.os.Message
import android.view.View
import android.widget.Button
import android.widget.EditText
import android.widget.TextView
import com.hujiang.devart.security.AlgorithmUtils
import kotlin.concurrent.thread

/**
 * Created by rarnu on 7/25/16.
 */
class SHA1Activity: BaseActivity(), View.OnClickListener {

    private var btnGo: Button? = null
    private var etSrc: EditText? = null
    private var tvDest: TextView? = null

    override fun getLayoutId(): Int = R.layout.activity_sha1

    override fun init() {
        btnGo = findViewById(R.id.btnGo) as Button?
        etSrc = findViewById(R.id.etSrc) as EditText?
        tvDest = findViewById(R.id.tvDest) as TextView?
        btnGo?.setOnClickListener(this)
    }

    private val h = object: Handler() {
        override fun handleMessage(msg: Message?) {
            tvDest?.text = msg!!.obj as String
            btnGo?.isEnabled = true
            super.handleMessage(msg)
        }
    }

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.btnGo -> {
                btnGo?.isEnabled = false
                val ori = etSrc?.text.toString()
                thread {
                    val enc = AlgorithmUtils.sha1EncryptString(ori)
                    val m =  Message()
                    m.obj = enc
                    h.sendMessage(m)
                }
            }
        }
    }
}