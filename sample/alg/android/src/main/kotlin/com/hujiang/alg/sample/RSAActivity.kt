package com.hujiang.alg.sample

import android.os.Handler
import android.os.Message
import android.view.View
import android.widget.Button
import android.widget.EditText
import android.widget.TextView
import android.widget.Toast
import com.hujiang.devart.security.AlgorithmUtils
import java.io.File
import kotlin.concurrent.thread

/**
 * Created by rarnu on 7/26/16.
 */
class RSAActivity: BaseActivity(), View.OnClickListener {

    private var btnKeyPair: Button? = null
    private var etEncSrc: EditText? = null
    private var tvEncDest: TextView? = null
    private var btnEncGo: Button? = null
    private var etDecSrc: EditText? = null
    private var tvDecDest: TextView? = null
    private var btnDecGo: Button? = null
    private var tvStatus: TextView? = null

    private val PUBKEY_PATH = "/sdcard/rsa.pub"
    private val PUBKEY_PASS = "hujiang"
    private val PRIVKEY_PATH = "/sdcard/rsa.priv"
    private val PRIVKEY_PASS = "hujiang"

    override fun getLayoutId(): Int = R.layout.activity_rsa

    override fun init() {
        btnKeyPair = findViewById(R.id.btnKeyPair) as Button?
        etEncSrc = findViewById(R.id.etEncSrc) as EditText?
        tvEncDest = findViewById(R.id.tvEncDest) as TextView?
        btnEncGo = findViewById(R.id.btnEncGo) as Button?
        etDecSrc = findViewById(R.id.etDecSrc) as EditText?
        tvDecDest = findViewById(R.id.tvDecDest) as TextView?
        btnDecGo = findViewById(R.id.btnDecGo) as Button?
        tvStatus = findViewById(R.id.tvStatus) as TextView?

        btnKeyPair?.setOnClickListener(this)
        btnEncGo?.setOnClickListener(this)
        btnDecGo?.setOnClickListener(this)
    }

    private val h = object: Handler() {
        override fun handleMessage(msg: Message?) {
            tvStatus?.text = ""
            if (msg!!.what == 99) {
                btnKeyPair?.isEnabled = true
                Toast.makeText(this@RSAActivity, msg.obj as String, Toast.LENGTH_SHORT).show()
            } else if (msg.what == 100) {
                btnEncGo?.isEnabled = true
                tvEncDest?.text = msg.obj as String
                etDecSrc?.setText(msg.obj as String)
            } else if (msg.what == 101) {
                btnDecGo?.isEnabled = true
                tvDecDest?.text = msg.obj as String
            } else {
                tvStatus?.text = msg.obj as String?
            }
            super.handleMessage(msg)
        }
    }

    private fun sendMessage(h: Handler, what: Int, msg: String) {
        val m = Message()
        m.what = what
        m.obj = msg
        h.sendMessage(m)
    }

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.btnKeyPair -> {
                btnKeyPair?.isEnabled = false
                thread {
                    sendMessage(h, 0, "Generating Key Pair ...")
                    val ret = AlgorithmUtils.rsaGenerateKeys(0, PUBKEY_PASS, PRIVKEY_PASS, PUBKEY_PATH, PRIVKEY_PATH)
                    sendMessage(h, 99, if (ret == 0) "OK" else "FAIL")
                }
            }
            R.id.btnEncGo -> {
                if (!File(PUBKEY_PATH).exists() || !File(PRIVKEY_PATH).exists()) {
                    Toast.makeText(this, "please generate key pair first.", Toast.LENGTH_SHORT).show()
                    return
                }
                btnEncGo?.isEnabled = false
                val ori = etEncSrc?.text.toString()
                thread {
                    sendMessage(h, 0, "Encrypting ...")
                    val enc = AlgorithmUtils.rsaEncryptString(0, PUBKEY_PASS, PUBKEY_PATH, ori)
                    sendMessage(h, 100, enc)
                }
            }
            R.id.btnDecGo -> {
                if (!File(PUBKEY_PATH).exists() || !File(PRIVKEY_PATH).exists()) {
                    Toast.makeText(this, "please generate key pair first.", Toast.LENGTH_SHORT).show()
                    return
                }
                btnDecGo?.isEnabled = false
                val ori = etDecSrc?.text.toString()
                thread {
                    sendMessage(h, 0, "Decrypting ...")
                    val dec = AlgorithmUtils.rsaDecryptString(0, PRIVKEY_PASS, PRIVKEY_PATH, ori)
                    sendMessage(h, 101, dec)
                }
            }
        }
    }

}