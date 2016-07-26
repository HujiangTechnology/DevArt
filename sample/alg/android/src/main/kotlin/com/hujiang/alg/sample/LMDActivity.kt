package com.hujiang.alg.sample

import android.app.ActionBar
import android.app.Activity
import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.EditText
import android.widget.TextView
import com.hujiang.devart.security.AlgorithmUtils

/**
 * Created by rarnu on 7/25/16.
 */
class LMDActivity: BaseActivity(), View.OnClickListener {

    private var btnGo: Button? = null
    private var etSrc: EditText? = null
    private var tvDest: TextView? = null

    override fun getLayoutId(): Int = R.layout.activity_lmd

    override fun init() {
        btnGo = findViewById(R.id.btnGo) as Button?
        etSrc = findViewById(R.id.etSrc) as EditText?
        tvDest = findViewById(R.id.tvDest) as TextView?
        btnGo?.setOnClickListener(this)
    }

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.btnGo -> {
                val ori = etSrc?.text.toString()
                val enc = AlgorithmUtils.lmdEncryptString(ori)
                tvDest?.text = enc

            }
        }
    }

}