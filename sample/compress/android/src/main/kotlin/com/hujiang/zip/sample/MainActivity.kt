package com.hujiang.zip.sample

import android.app.Activity
import android.os.Bundle
import android.os.Handler
import android.os.Message
import android.view.View
import android.widget.*
import com.hujiang.devart.utils.ZipUtils
import kotlin.concurrent.thread

class MainActivity : Activity(), AdapterView.OnItemSelectedListener, View.OnClickListener {

    val SDCARD = "/sdcard"
    val SDZIPSRC = "$SDCARD/test"
    val SDZIPSRCSINGLE = "$SDCARD/test.txt"
    val SDZIPDEST = "$SDCARD"
    val SDUNZIPDEST = "$SDCARD/unzip/"
    val SDUNZIPDESTSINGLE = "$SDCARD/test.file"

    val ZIP_TYPE = listOf(
            FormatInfo(".hjz", 0, SDZIPSRC, "$SDZIPDEST/test.hjz", "$SDZIPDEST/test.hjz", SDUNZIPDEST),
            FormatInfo(".hjp", 0, SDZIPSRC, "$SDZIPDEST/test.hjp", "$SDZIPDEST/test.hjp", SDUNZIPDEST),
            FormatInfo(".zip", 0, SDZIPSRC, "$SDZIPDEST/test.zip", "$SDZIPDEST/test.zip", SDUNZIPDEST),
            FormatInfo(".jar", 0, SDZIPSRC, "$SDZIPDEST/test.jar", "$SDZIPDEST/test.jar", SDUNZIPDEST),
            FormatInfo(".tar", 0, SDZIPSRC, "$SDZIPDEST/test.tar", "$SDZIPDEST/test.tar", SDUNZIPDEST),
            FormatInfo(".gz", 1, SDZIPSRCSINGLE, "$SDZIPDEST/test.gz", "$SDZIPDEST/test.gz", SDUNZIPDESTSINGLE),
            FormatInfo(".gzip", 1, SDZIPSRCSINGLE, "$SDZIPDEST/test.gzip", "$SDZIPDEST/test.gzip", SDUNZIPDESTSINGLE),
            FormatInfo(".tgz", 0, SDZIPSRC, "$SDZIPDEST/test.tgz", "$SDZIPDEST/test.tgz", SDUNZIPDEST),
            FormatInfo(".tar.gz", 0, SDZIPSRC, "$SDZIPDEST/test.tar.gz", "$SDZIPDEST/test.tar.gz", SDUNZIPDEST)
    )

    private var etSrc: EditText? = null
    private var etDest: EditText? = null
    private var spFormat: Spinner? = null
    private var tvFormatHint: TextView? = null
    private var btnZip: Button? = null
    private var btnUnzip: Button? = null
    private var btnGo: Button? = null
    private var tvLog: TextView? = null
    private var adapter: FormatAdapter? = null

    private var isUnzip = false

    public override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        etSrc = findViewById(R.id.etSrc) as EditText?
        etDest = findViewById(R.id.etDest) as EditText?
        spFormat = findViewById(R.id.spFormat) as Spinner?
        tvFormatHint = findViewById(R.id.tvFormatHint) as TextView?
        btnZip = findViewById(R.id.btnZip) as Button?
        btnUnzip = findViewById(R.id.btnUnzip) as Button?
        tvLog = findViewById(R.id.tvLog) as TextView?
        adapter = FormatAdapter(this, ZIP_TYPE)
        spFormat?.adapter = adapter
        spFormat?.onItemSelectedListener = this
        spFormat?.setSelection(0)

        btnZip = findViewById(R.id.btnZip) as Button?
        btnUnzip = findViewById(R.id.btnUnzip) as Button?
        btnGo = findViewById(R.id.btnGo) as Button?

        btnZip?.setOnClickListener(this)
        btnUnzip?.setOnClickListener(this)
        btnGo?.setOnClickListener(this)

        refreshTitle()
    }

    private fun refreshTitle() {
        val mode = getString(if (isUnzip) R.string.main_unzip else R.string.main_zip)
        actionBar.title = "ZipSample ($mode Mode)"

        val item = ZIP_TYPE[spFormat!!.selectedItemPosition]
        etSrc?.setText(if (isUnzip) item.unzipSrc else item.zipSrc)
        etDest?.setText(if (isUnzip) item.unzipDest else item.zipDest)

    }

    override fun onNothingSelected(parent: AdapterView<*>?) {

    }

    override fun onItemSelected(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        val item = ZIP_TYPE[position]
        val type = item.fileType
        tvFormatHint?.text = getString(if (type == 0) R.string.main_multi else R.string.main_single)
        etSrc?.setText(if (isUnzip) item.unzipSrc else item.zipSrc)
        etDest?.setText(if (isUnzip) item.unzipDest else item.zipDest)

    }

    override fun onClick(v: View?) {
        when(v!!.id) {
            R.id.btnZip -> {
                isUnzip = false
                refreshTitle()
            }
            R.id.btnUnzip -> {
                isUnzip = true
                refreshTitle()
            }
            R.id.btnGo -> {
                btnGo?.isEnabled = false
                // gogogo
                if (isUnzip) {
                    threadUnzip()
                } else {
                    threadZip()
                }
            }
        }
    }

    private val hZip = object: Handler() {
        override fun handleMessage(msg: Message?) {
            btnGo?.isEnabled = true
            super.handleMessage(msg)
        }
    }

    private val hMsg = object: Handler() {
        override fun handleMessage(msg: Message?) {
            tvLog?.text = tvLog?.text.toString() + "\n${msg?.obj as String}"
            super.handleMessage(msg)
        }
    }

    private fun sendMessage(h: Handler, msg: String) {
        val m = Message.obtain(h, 0, msg)
        h.sendMessage(m)
        m.recycle()
    }

    private fun threadZip() {
        thread {
            val src = etSrc?.text.toString()
            val dest = etDest?.text.toString()
            val start = System.currentTimeMillis()
            sendMessage(hMsg, "[zip-start] $start")
            val errorCode = ZipUtils.compress(dest, src)
            val end = System.currentTimeMillis()
            sendMessage(hMsg, "[zip-end] $end")
            val status = ZipUtils.getCompressStatus(dest)
            sendMessage(hMsg, "[zip-status] err: $errorCode")
            sendMessage(hMsg, "[zip-status] msg: ${status.errMsg}")
            sendMessage(hMsg, "[zip-status] filePath: ${status.filePath}")
            sendMessage(hMsg, "[zip-status] fileCount: ${status.fileCount}")
            sendMessage(hMsg, "[zip-status] compressedCount: ${status.compressCount}")
            sendMessage(hZip, "")
        }

    }

    private fun threadUnzip() {

        thread {
            val src = etSrc?.text.toString()
            val dest = etDest?.text.toString()
            val start = System.currentTimeMillis()
            sendMessage(hMsg, "[unzip-start] $start")
            val errorCode = ZipUtils.uncompress(src, dest)
            val end = System.currentTimeMillis()
            sendMessage(hMsg, "[unzip-end] $end")
            val status = ZipUtils.getUncompressStatus(src)
            sendMessage(hMsg, "[unzip-status] err: $errorCode")
            sendMessage(hMsg, "[unzip-status] msg: ${status.errMsg}")
            sendMessage(hMsg, "[unzip-status] filePath: ${status.filePath}")
            sendMessage(hMsg, "[unzip-status] fileCount: ${status.fileCount}")
            sendMessage(hMsg, "[unzip-status] uncompressedCount: ${status.uncompressCount}")
            sendMessage(hZip, "")
        }
    }

}
