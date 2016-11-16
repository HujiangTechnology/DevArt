package com.hujiang.devart.sample.server

import android.util.Log
import com.hujiang.devart.sample.DevArtApplication
import com.hujiang.devart.server.HTTPServer
import com.hujiang.devart.server.HTTPSession
import com.hujiang.devart.server.IHTTPSession
import com.hujiang.devart.server.Response
import com.hujiang.devart.utils.FileUtils
import java.io.ByteArrayInputStream

/**
 * Created by rarnu on 4/27/16.
 */
class DevServer(port: Int) : HTTPServer(port) {

    override fun serve(session: IHTTPSession?): Response? {
        session as HTTPSession
        val parms = session._parms
        val uri = session._uri

        Log.e("LOG", "serve => ${uri}, ${parms}")

        if (uri == "" || uri == "/") {
            return loadFile("index.html")
        } else if (uri!!.startsWith("/calc")) {
            val n1 = parms!!["n1"]!!.toInt()
            val n2 = parms["n2"]!!.toInt()
            val typ = parms["typ"]!!.toInt()
            Log.e("LOG", "CALC => ${n1}, ${n2}, ${typ}")
            var ret = ""
            when (typ) {
                1 -> ret = "${n1 + n2}"
                2 -> ret = "${n1 - n2}"
                3 -> ret = "${n1 * n2}"
                4 -> ret = if (n2 == 0) "error" else "${n1 * 1.0f / n2}"
            }
            return loadData(ret)
        } else if (uri == "/intro") {
            return loadFile("devart.html")
        } else if (uri.startsWith("/json")) {
            val retStr = "{\"result\": \"ok\"}"
            return loadData(retStr)
        } else {
            var localFile = uri.substring(1)
            return loadFile(localFile)
        }
    }

    fun loadFile(fileName: String?): Response? {
        val indexText = FileUtils.readAssetFile(DevArtApplication.instance()!!.baseContext, fileName!!)
        val data = ByteArrayInputStream(indexText?.toByteArray())
        return Response(Response.Status.OK, MIME_HTML, data, indexText!!.length.toLong())
    }

    fun loadData(data: String?): Response? {
        val dataStream = ByteArrayInputStream(data?.toByteArray())
        return Response(Response.Status.OK, MIME_HTML, dataStream, data!!.length.toLong())
    }

}