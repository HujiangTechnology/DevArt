package com.hujiang.devart.component.dns

import java.io.ByteArrayInputStream
import java.io.DataInputStream
import java.io.EOFException
import java.io.IOException
import java.nio.charset.Charset

/**
 * Created by rarnu on 4/8/16.
 */
class DNSInputStream: ByteArrayInputStream {

    protected var dataIn: DataInputStream? = null
    private var _dnsIn: DNSInputStream? = null

    constructor(data: ByteArray?, offset: Int, length: Int): super(data, offset, length) {
        dataIn = DataInputStream(this)
    }

    fun readByte(): Int = dataIn!!.readUnsignedByte()

    fun readShort(): Int = dataIn!!.readUnsignedShort()

    fun readInt(): Long = dataIn!!.readInt().toLong() and 0xffffffffL

    fun readString(): String {
        val len = readByte()
        if (len == 0) {
            return ""
        } else {
            val buffer = ByteArray(len)
            dataIn?.readFully(buffer)
            return String(buffer, Charset.forName("latin1"))
        }
    }

    fun readDomainName(): String {
        if (pos >= count) {
            throw EOFException("EOF reading domain name")
        }
        if ((buf[pos].toInt() and 0xc0) == 0) {
            var label = readString()
            if (label.length > 0) {
                val tail = readDomainName()
                if (tail.length > 0) {
                    label = label + '.' + tail
                }
            }
            return label
        } else {
            if ((buf[pos].toInt() and 0xc0) != 0xc0) {
                throw IOException("Invalid domain name compression offset")
            }
            val offset = readShort() and 0x3fff
            _dnsIn = DNSInputStream(buf, offset, buf.size - offset)
            return _dnsIn!!.readDomainName()
        }
    }

    fun readRR(): DNSRR? {
        val rrName = readDomainName()
        val rrType = readShort()
        val rrClass = readShort()
        val rrTTL = readInt()
        val rrDataLen = readShort()
        val rrDNSIn = DNSInputStream(buf, pos, rrDataLen)
        pos += rrDataLen
        try {
            val myName = javaClass.name
            val periodIndex = myName.lastIndexOf('.')
            val myPackage = myName.substring(0, 1 + periodIndex)
            val theClass = Class.forName(myPackage + "record." + DNS.typeName(rrType))
            val rr = theClass.newInstance() as DNSRR
            rr.init(rrName, rrType, rrClass, rrTTL, rrDNSIn)
            return rr
        } catch (e: Exception) {
            return null
        }
    }

}