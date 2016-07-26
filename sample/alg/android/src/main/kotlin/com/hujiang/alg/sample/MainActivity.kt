package com.hujiang.alg.sample

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.View
import android.widget.AdapterView
import android.widget.ArrayAdapter
import android.widget.ListView

class MainActivity : Activity(), AdapterView.OnItemClickListener {


    private var lvAlg: ListView? = null
    private var adapter: ArrayAdapter<String>? = null
    private var list = arrayListOf("MD5","SHA1","LMD","ELF", "BASE64", "RSA", "DSA", "RDL", "RSASSA", "AES")


    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        lvAlg = findViewById(R.id.lvAlg) as ListView?
        adapter = ArrayAdapter(this, R.layout.item_main, R.id.tvAlgName, list)
        lvAlg?.adapter = adapter

        lvAlg?.onItemClickListener = this
    }

    override fun onItemClick(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        val name = "${list[position]}Activity"
        val cls = Class.forName("com.hujiang.alg.sample.$name")
        val inActivity = Intent(this, cls)
        startActivity(inActivity)
    }

}
