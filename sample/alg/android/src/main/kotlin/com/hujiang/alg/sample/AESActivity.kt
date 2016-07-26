package com.hujiang.alg.sample

import android.view.View
import android.widget.Button
import android.widget.EditText
import android.widget.TextView
import com.hujiang.devart.security.AlgorithmUtils

/**
 * Created by rarnu on 7/26/16.
 */
class AESActivity: BaseActivity(), View.OnClickListener {

    private var etEncSrc: EditText? = null
    private var etEncKey: EditText? = null
    private var tvEncDest: TextView? = null
    private var btnEncGo: Button? = null
    private var etDecSrc: EditText? = null
    private var etDecKey: EditText? = null
    private var tvDecDest: TextView? = null
    private var btnDecGo: Button? = null

    override fun getLayoutId(): Int = R.layout.activity_aes

    override fun init() {
        etEncSrc = findViewById(R.id.etEncSrc) as EditText?
        etEncKey = findViewById(R.id.etEncKey) as EditText?
        tvEncDest = findViewById(R.id.tvEncDest) as TextView?
        btnEncGo = findViewById(R.id.btnEncGo) as Button?
        etDecSrc = findViewById(R.id.etDecSrc) as EditText?
        etDecKey = findViewById(R.id.etDecKey) as EditText?
        tvDecDest = findViewById(R.id.tvDecDest) as TextView?
        btnDecGo = findViewById(R.id.btnDecGo) as Button?

        btnEncGo?.setOnClickListener(this)
        btnDecGo?.setOnClickListener(this)
    }

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.btnEncGo -> {
                val ori = etEncSrc?.text.toString()
                val key = etEncKey?.text.toString()
                val enc = AlgorithmUtils.aesEncryptECB128(key, ori)
                tvEncDest?.text = enc
                etDecSrc?.setText(enc)
                etDecKey?.setText(key)
            }
            R.id.btnDecGo -> {
                val ori = etDecSrc?.text.toString()
                val key = etDecKey?.text.toString()
                val dec = AlgorithmUtils.aesDecryptECB128(key, ori)
                tvDecDest?.text = dec
            }
        }
    }
}