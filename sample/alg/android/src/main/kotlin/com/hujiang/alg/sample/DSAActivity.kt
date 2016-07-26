package com.hujiang.alg.sample

import android.app.Activity
import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.View
import android.widget.Button
import android.widget.EditText
import android.widget.TextView
import android.widget.Toast
import com.hujiang.devart.security.AlgorithmUtils
import kotlin.concurrent.thread

/**
 * Created by rarnu on 7/26/16.
 */
class DSAActivity: BaseActivity(), View.OnClickListener {

    private var btnKeyPair: Button? = null
    private var etEncSrc: EditText? = null
    private var tvEncDest: TextView? = null
    private var btnEncGo: Button? = null
    private var etVerifySrc: EditText? = null
    private var etVerifyOri: EditText? = null
    private var tvVerifyDest: TextView? = null
    private var btnVerifyGo: Button? = null
    private var tvStatus: TextView? = null

    private val PUBKEY_PATH = "/sdcard/dsa.pub"
    private val PUBKEY_PASS = "hujiang"
    private val PRIVKEY_PATH = "/sdcard/dsa.priv"
    private val PRIVKEY_PASS = "hujiang"

    override fun getLayoutId(): Int = R.layout.activity_dsa

    override fun init() {
        btnKeyPair = findViewById(R.id.btnKeyPair) as Button?
        etEncSrc = findViewById(R.id.etEncSrc) as EditText?
        tvEncDest = findViewById(R.id.tvEncDest) as TextView?
        btnEncGo = findViewById(R.id.btnEncGo) as Button?
        etVerifySrc = findViewById(R.id.etVerifySrc) as EditText?
        etVerifyOri = findViewById(R.id.etVerifyOri) as EditText?
        tvVerifyDest = findViewById(R.id.tvVerifyDest) as TextView?
        btnVerifyGo = findViewById(R.id.btnVerifyGo) as Button?
        tvStatus = findViewById(R.id.tvStatus) as TextView?

        btnKeyPair?.setOnClickListener(this)
        btnEncGo?.setOnClickListener(this)
        btnVerifyGo?.setOnClickListener(this)
    }

    private val h = object: Handler() {
        override fun handleMessage(msg: Message?) {
            tvStatus?.text = ""
            if (msg!!.what == 99) {
                Toast.makeText(this@DSAActivity, msg.obj as String, Toast.LENGTH_SHORT).show()
            } else if (msg.what == 100) {
                tvEncDest?.text = msg.obj as String
                etVerifySrc?.setText(msg.obj as String)
            } else if (msg.what == 101) {
                tvVerifyDest?.text = msg.obj as String
            } else {
                tvStatus?.text = msg.obj as String?
            }
            super.handleMessage(msg)
        }
    }

    private fun sendMessage(h: Handler, what: Int, msg: String) {
        val m = Message.obtain(h, what, msg)
        h.sendMessage(m)
        m.recycle()
    }

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.btnKeyPair -> {
                thread {
                    sendMessage(h, 0, "Generating Key Pair ...")
                    val ret = AlgorithmUtils.dsaGenerateKeys(0, PUBKEY_PASS, PRIVKEY_PASS, PUBKEY_PATH, PRIVKEY_PATH)
                    sendMessage(h, 99, if (ret == 0) "OK" else "FAIL")
                }
            }
            R.id.btnEncGo -> {
                val ori = etEncSrc?.text.toString()
                etVerifyOri?.setText(ori)
                thread {
                    sendMessage(h, 0, "Encrypting ...")
                    val enc = AlgorithmUtils.dsaSignString(0, PRIVKEY_PASS, PRIVKEY_PATH, ori)
                    sendMessage(h, 100, enc)
                }
            }
            R.id.btnVerifyGo -> {
                val e = etVerifySrc?.text.toString()
                val ori = etVerifyOri?.text.toString()
                thread {
                    sendMessage(h, 0, "Verifying ...")
                    val r = AlgorithmUtils.dsaVerifyString(0, PUBKEY_PASS, PUBKEY_PATH, e, ori)
                    sendMessage(h, 101, if (r == 0) "TRUE" else "FALSE")
                }
            }
        }
    }

}