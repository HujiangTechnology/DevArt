package com.hujiang.devart.utils

import com.hujiang.devart.utils.http.*
import com.hujiang.devart.utils.socket.SSLSocket
import org.apache.http.HttpVersion
import org.apache.http.client.CookieStore
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpDelete
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.methods.HttpPost
import org.apache.http.client.methods.HttpRequestBase
import org.apache.http.conn.ClientConnectionManager
import org.apache.http.conn.params.ConnManagerParams
import org.apache.http.conn.scheme.PlainSocketFactory
import org.apache.http.conn.scheme.Scheme
import org.apache.http.conn.scheme.SchemeRegistry
import org.apache.http.conn.ssl.SSLSocketFactory
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager
import org.apache.http.message.BasicNameValuePair
import org.apache.http.params.BasicHttpParams
import org.apache.http.params.HttpConnectionParams
import org.apache.http.params.HttpParams
import org.apache.http.params.HttpProtocolParams
import org.apache.http.util.EntityUtils
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.io.OutputStreamWriter
import java.net.URL
import java.security.KeyStore

/**
 * Created by rarnu on 3/25/16.
 */
object HttpUtils {

    fun simplePostWithHeader(host: String, param: String, encoding: String, property: MutableMap<String, String>?): String {
        val url = URL(host)
        val conn = url.openConnection()
        if (property != null) {
            val iter = property.entries.iterator()
            while (iter.hasNext()) {
                val entry = iter.next()
                conn.addRequestProperty(entry.key, entry.value)
            }
        }
        conn.doOutput = true
        val osw = OutputStreamWriter(conn.outputStream)
        osw.write(param)
        osw.flush()
        osw.close()
        val isr = InputStreamReader(conn.inputStream, encoding)
        val br = BufferedReader(isr)
        var content: String?
        var result = ""
        while(true) {
            content = br.readLine()
            if (content != null) {
                result += "${content}\n"
            } else {
                break
            }
        }
        br.close()
        isr.close()
        return result
    }

    fun simplePost(host: String, param: String, encoding: String): String = simplePostWithHeader(host, param, encoding, null)

    fun post(host: String, getParams: String, params: MutableList<BasicNameValuePair>?, encoding: String): String? = post("${host}?${getParams}", params, encoding)

    fun post(host: String, params: MutableList<BasicNameValuePair>?, encoding: String): String? {
        val httpPost = HttpPost(host)
        try {
            val pEntity = UrlEncodedFormEntity(params, encoding)
            httpPost.entity = pEntity
        } catch (e: Exception) {

        }
        return executeForResult(httpPost, encoding)
    }

    fun postFile(host: String, params: MutableList<BasicNameValuePair>?, files: MutableList<BasicNameValuePair>?, encoding: String): String? {
        val httpPost = buildPostFileParts(host, params, files, encoding)
        return executeForResult(httpPost, encoding)
    }

    fun postFileWithProgress(host: String, params: MutableList<BasicNameValuePair>?, files: MutableList<BasicNameValuePair>?, encoding: String, callback: ProgressedMultipartEntity.ProgressListener?): String? {
        val httpPost = buildPostFilePartsWithCallback(host, params, files, encoding, callback)
        return executeForResult(httpPost, encoding)
    }

    fun postWithCookie(host: String, params: MutableList<BasicNameValuePair>?, cookie: CookieStore?, encoding: String): HttpRequestResponseData? {
        val httpPost = HttpPost(host)
        try {
            val pEntity = UrlEncodedFormEntity(params, encoding)
            httpPost.entity = pEntity
        } catch (e: Exception) {

        }
        return executeForData(httpPost, cookie, encoding)
    }

    fun postFileWithCookie(host: String, params: MutableList<BasicNameValuePair>?, files: MutableList<BasicNameValuePair>?, cookie: CookieStore?, encoding: String): HttpRequestResponseData? {
        val httpPost = buildPostFileParts(host, params, files, encoding)
        return executeForData(httpPost, cookie, encoding)
    }

    fun postFileWithCookieAndProgress(host: String, params: MutableList<BasicNameValuePair>?, files: MutableList<BasicNameValuePair>?, cookie: CookieStore?, encoding: String, callback: ProgressedMultipartEntity.ProgressListener?): HttpRequestResponseData? {
        val httpPost = buildPostFilePartsWithCallback(host, params, files, encoding, callback)
        return executeForData(httpPost, cookie, encoding)
    }

    private fun buildPostFileParts(host: String, params: MutableList<BasicNameValuePair>?, files: MutableList<BasicNameValuePair>?, encoding: String): HttpPost? =
            buildPostFilePartsWithCallback(host, params, files, encoding, null)

    private fun buildPostFilePartsWithCallback(host: String, params: MutableList<BasicNameValuePair>?, files: MutableList<BasicNameValuePair>?, encoding: String, callback: ProgressedMultipartEntity.ProgressListener?): HttpPost? {
        val httpPost = HttpPost(host)
        try {
            val p = arrayOfNulls<Part>(params!!.size + files!!.size)
            for (i in 0..params.size - 1) {
                p[i] = StringPart(params[i].name, params[i].value, encoding)
            }
            val idx = params.size
            for (i in idx..p.size - 1) {
                p[i] = FilePart(files[i - idx].name, File(files[i - idx].value), "*/*", encoding)
            }
            var multipart: MultipartEntity?
            if (callback == null) {
                multipart = MultipartEntity(p)
            } else {
                multipart = ProgressedMultipartEntity(p, callback)
            }
            httpPost.entity = multipart
        } catch (e: Exception) {

        }
        return httpPost
    }


    fun get(host: String, params: String, encoding: String): String? {
        val request = HttpGet("${host}?${params}")
        return executeForResult(request, encoding)
    }

    fun delete(host: String, params: String, encoding: String): String? {
        val request = HttpDelete("${host}?${params}")
        return executeForResult(request, encoding)
    }

    fun getWithCookie(host: String, params: String, cookie: CookieStore?, encoding: String): HttpRequestResponseData? {
        val request = HttpGet("${host}?${params}")
        return executeForData(request, cookie, encoding)
    }

    private fun buildHttpParams(encoding: String): BasicHttpParams? {
        val httpParams = BasicHttpParams()
        HttpProtocolParams.setVersion(httpParams, HttpVersion.HTTP_1_1)
        HttpProtocolParams.setContentCharset(httpParams, encoding)
        HttpProtocolParams.setUseExpectContinue(httpParams, true)
        ConnManagerParams.setTimeout(httpParams, 10000)
        HttpConnectionParams.setConnectionTimeout(httpParams, 10000)
        HttpConnectionParams.setSoTimeout(httpParams, 10000)
        return httpParams
    }

    private fun buildConnectionManager(params: HttpParams?): ClientConnectionManager? {
        var ret: ClientConnectionManager? = null
        try {
            val store = KeyStore.getInstance(KeyStore.getDefaultType())
            store.load(null, null);
            val sf = SSLSocket(store)
            sf.hostnameVerifier = SSLSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER
            val reg = SchemeRegistry()
            reg.register(Scheme("http", PlainSocketFactory.getSocketFactory(), 80))
            reg.register(Scheme("https", sf, 443))
            ret = ThreadSafeClientConnManager(params, reg)
        } catch (e: Exception) {

        }
        return ret
    }

    private fun executeForResult(request: HttpRequestBase?, encoding: String): String? {
        try {
            val params = buildHttpParams(encoding)
            val connMgr = buildConnectionManager(params)
            val client = DefaultHttpClient(connMgr, params)
            val response = client.execute(request)
            var result = ""
            val statusCode = response.statusLine.statusCode
            if (statusCode == 200) {
                result = EntityUtils.toString(response.entity, encoding)
            }
            return result
        } catch (e: Exception) {
            return null
        }
    }

    private fun executeForData(request: HttpRequestBase?, cookie: CookieStore?, encoding: String): HttpRequestResponseData? {
        var `data`: HttpRequestResponseData? = null
        try {
            var params = buildHttpParams(encoding)
            val connMgr = buildConnectionManager(params)
            val client = DefaultHttpClient(connMgr, params)
            if (cookie != null) {
                client.cookieStore = cookie
            }
            val response = client.execute(request)
            val statusCode = response.statusLine.statusCode
            if (statusCode == 200) {
                `data` = HttpRequestResponseData()
                `data`.`data` = EntityUtils.toString(response.entity, encoding)
                `data`.cookie = client.cookieStore
            }
        } catch (e: Exception) {

        }
        return `data`
    }

}