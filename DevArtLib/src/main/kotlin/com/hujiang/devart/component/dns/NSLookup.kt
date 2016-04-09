package com.hujiang.devart.component.dns

import com.hujiang.devart.component.dns.record.Address
import java.io.*
import java.net.Socket

/**
 * Created by rarnu on 4/8/16.
 */
object NSLookup {

    fun nslookup(domain: String?, nameserver: String?): MutableList<Address?>?  {
        var result: MutableList<Address?>? = null
        val query = DNSQuery(domain, DNS.TYPE_ANY, DNS.CLASS_IN)
        try {
            val socket = Socket(nameserver, DNS.DEFAULT_PORT)
            socket.soTimeout = 10000
            sendQuery(query, socket)
            getResponse(query, socket)
            socket.close()
            val answers = query.getAnswers()
            result = arrayListOf<Address?>()
            while (answers!!.hasMoreElements()) {
                val dr = answers.nextElement()
                if (dr != null) {
                    if (dr.getRRType() == DNS.CLASS_IN) {
                        result.add(dr as Address)
                    }
                }
            }
        } catch (e: IOException) {
        }
        return result
    }

    fun sendQuery(query: DNSQuery?, socket: Socket?) {
        val bufferedOut = BufferedOutputStream(socket?.outputStream)
        val dataOut = DataOutputStream(bufferedOut)
        val data = query?.extractQuery()
        dataOut.writeShort(data!!.size)
        dataOut.write(data)
        dataOut.flush()
    }

    fun getResponse(query: DNSQuery?, socket: Socket?) {
        val bufferedIn = BufferedInputStream(socket?.inputStream)
        val dataIn = DataInputStream(bufferedIn)
        val responseLength = dataIn.readUnsignedShort()
        var data = ByteArray(responseLength)
        dataIn.readFully(data)
        query?.receiveResponse(data, responseLength)
    }
}