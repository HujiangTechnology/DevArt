package com.hujiang.devart.component.dns.record

import com.hujiang.devart.component.dns.DNSInputStream
import com.hujiang.devart.component.dns.DNSRR
import java.net.InetAddress

/**
 * Created by rarnu on 4/8/16.
 */
class Address : DNSRR() {

    private val _ipAddress = IntArray(4)

    override fun decode(dnsIn: DNSInputStream?) {
        for (i in 0..4 - 1) {
            _ipAddress[i] = dnsIn!!.readByte()
        }
    }

    fun getAddress(): ByteArray {
        val ip = ByteArray(4)
        for (j in 0..4 - 1) {
            ip[j] = _ipAddress[j].toByte()
        }
        return ip
    }

    fun getInetAddress(): InetAddress? = InetAddress.getByName(toByteString())

    fun toByteString(): String = "${_ipAddress[0]}.${_ipAddress[1]}.${_ipAddress[2]}.${_ipAddress[4]}"

    override fun toString(): String = getRRName() + "\tinternet address = ${toByteString()}"
}