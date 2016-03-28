package com.hujiang.devart.utils

import android.content.Context
import android.graphics.BitmapFactory
import android.os.Handler
import android.os.Message
import android.view.View
import android.widget.ImageView
import android.widget.TextView
import com.hujiang.devart.base.common.Actions
import java.io.File
import java.io.FileOutputStream
import java.net.HttpURLConnection
import java.net.URL

/**
 * Created by rarnu on 3/27/16.
 */
object  DownloadUtils {

    private val listDownloading = arrayListOf<DownloadInfo>()

    fun stopDownloadTask(localDir: String, localFile: String) {
        val filePath = localDir + localFile
        for (di in listDownloading) {
            if (di.fileName == filePath) {
                di.thread?.interrupt()
                listDownloading.remove(di)
                break
            }
        }
    }

    fun downloadFileT(context: Context, iv: ImageView?, url: String, localDir: String, localFile: String, hProgress: Handler?, bop: BitmapFactory.Options?, isRound: Boolean, radis: Int) {
        var nlocalDir = localDir
        if (!nlocalDir.endsWith("/")) {
            nlocalDir += "/"
        }
        val fDir = File(nlocalDir)
        if (!fDir.exists()) {
            fDir.mkdirs()
        }
        val filePath = nlocalDir + localFile

        val fImg = File(filePath)
        if (fImg.exists()) {
            if (iv != null) {
                var bmp = if (bop != null) { BitmapFactory.decodeFile(filePath, bop) } else { BitmapFactory.decodeFile(filePath) }
                if (isRound) {
                    bmp = ImageUtils.roundedCornerBitmap(bmp, radis.toFloat())
                }
                iv.setImageBitmap(bmp)
            } else if (hProgress != null) {
                MessageUtils.sendMessage(hProgress, Actions.WHAT_DOWNLOAD_FINISH)
            }
            return
        }

        val hImage = object : Handler() {
            override fun handleMessage(msg: Message?) {
                if (msg!!.what == 1) {
                    val file = File(filePath)
                    if (file.exists()) {
                        if (iv != null) {
                            var bmp = if (bop != null) { BitmapFactory.decodeFile(filePath, bop) } else { BitmapFactory.decodeFile(filePath) }
                            if (isRound) {
                                bmp = ImageUtils.roundedCornerBitmap(bmp, radis.toFloat())
                            }
                            iv.setImageBitmap(bmp)
                        }
                    }
                    for (di in listDownloading) {
                        if (di.fileName == filePath) {
                            listDownloading.remove(di)
                            break
                        }
                    }
                }
                super.handleMessage(msg)
            }
        }

        val tDownload = Thread {
            downloadFile(url, filePath, hProgress, null)
            hImage.sendEmptyMessage(1)
        }

        val info = DownloadInfo()
        info.fileName = filePath
        info.thread = tDownload

        var hasTask = false
        for (di in listDownloading) {
            if (di.fileName == info.fileName) {
                hasTask = true
                break
            }
        }
        if (!hasTask) {
            listDownloading.add(info)
            tDownload.start()
        }
    }

    fun downloadFileT(context: Context, iv: ImageView?, tv: TextView?, url: String, localDir: String, localFile: String) {
        val hDownload = object : Handler() {
            override fun handleMessage(msg: Message?) {
                when (msg!!.what) {
                    Actions.WHAT_DOWNLOAD_START -> {
                        tv?.visibility = View.VISIBLE
                        tv?.text = "${msg.arg1} / ${msg.arg2}"
                    }
                    Actions.WHAT_DOWNLOAD_PROGRESS -> {
                        tv?.text = "${msg.arg1} / ${msg.arg2}"
                    }
                    Actions.WHAT_DOWNLOAD_FINISH -> {
                        tv?.visibility = View.GONE
                    }
                }
                super.handleMessage(msg)
            }
        }
        downloadFileT(context, iv, url, localDir, localFile, hDownload)
    }

    fun downloadFileT(context: Context, iv: ImageView?, url: String, localDir: String, localFile: String, hProgress: Handler?) =
            downloadFileT(context, iv, url, localDir, localFile, hProgress, null, false, 0)

    fun downloadFileT(context: Context, iv: ImageView?, url: String, localDir: String, localFile: String, hProgress: Handler?, isRound: Boolean, radis: Int) =
            downloadFileT(context, iv, url, localDir, localFile, hProgress, null, isRound, radis)

    fun downloadFile(address: String, localFile: String, h: Handler?, callback: BreakableThread.RunningCallback?) {
        val fTmp = File(localFile)
        if (fTmp.exists()) {
            fTmp.delete()
        }
        var isDownloadNormal = true
        var url: URL?
        var filesize = 0
        var position = 0
        try {
            url = URL(address)
            val con = url.openConnection() as HttpURLConnection
            var ins = con.inputStream
            filesize = con.contentLength
            MessageUtils.sendMessage(h, Actions.WHAT_DOWNLOAD_START, position, filesize)
            val fileOut = File(localFile + ".tmp")
            val out = FileOutputStream(fileOut)
            val buffer = ByteArray(1024)
            var count: Int
            while (true) {
                count = ins.read(buffer)
                if (count != -1) {
                    out.write(buffer, 0, count)
                    position += count
                    MessageUtils.sendMessage(h, Actions.WHAT_DOWNLOAD_PROGRESS, position, filesize)
                    if (callback != null) {
                        if (!callback!!.getRunningState()) {
                            isDownloadNormal = false
                            break
                        }
                    }
                } else {
                    break
                }
            }
            ins.close()
            out.close()
            fileOut.renameTo(fTmp)
            if (!isDownloadNormal) {
                fileOut.delete()
                fTmp.delete()
            }
            MessageUtils.sendMessage(h, Actions.WHAT_DOWNLOAD_FINISH, 0, filesize)
        } catch (e: Exception) {
            MessageUtils.sendMessage(h, Actions.WHAT_DOWNLOAD_FINISH, 0, filesize, e.message)
        }

    }

    class DownloadInfo {
        var fileName: String? = null
        var thread: Thread? = null
    }

    class BreakableThread : Thread {

        interface RunningCallback {
            fun getRunningState(): Boolean
        }

        private var _runningCallback: RunningCallback? = null
        var runningCallback: RunningCallback?
            get() = _runningCallback
            set(value) {
                _runningCallback = value
            }

        constructor(callback: RunningCallback?) : super() {
            _runningCallback = callback
        }
    }

}