package com.hujiang.devart.component.dns.record

import com.hujiang.devart.component.dns.DNSInputStream
import com.hujiang.devart.component.dns.DNSRR
import java.net.InetAddress

/**
 * Created by rarnu on 4/8/16.
 */
class WellKnownServices : DNSRR() {

    private var _ipAddress = IntArray(4)
    private var _protocol = 0
    private var _data: ByteArray? = null

    override fun decode(dnsIn: DNSInputStream?) {
        for (j in 0..4 - 1) {
            _ipAddress[j] = dnsIn!!.readByte()
        }
        _protocol = dnsIn!!.readByte()
        _data = ByteArray(dnsIn.available())
        dnsIn.read(_data)
    }

    fun getAddress(): ByteArray? {
        val ip = ByteArray(4)
        for (j in 0..4 - 1) {
            ip[j] = _ipAddress[j].toByte()
        }
        return ip
    }

    fun getInetAddress(): InetAddress? = InetAddress.getByName(toByteString())

    fun getProtocol(): Int = _protocol

    fun getData(): ByteArray? {
        val copy = ByteArray(_data!!.size)
        System.arraycopy(_data, 0, copy, 0, _data!!.size)
        return copy
    }

    private fun toByteString(): String = "${_ipAddress[0]}.${_ipAddress[1]}.${_ipAddress[2]}.${_ipAddress[3]}"

    override fun toString(): String {
        val services = StringBuffer()
        for (i in _data!!.size - 1 downTo 0) {
            for (j in 7 downTo 0) {
                services.append((_data!![i].toInt() ushr j) and 1)
            }
        }
        return getRRName() + "\twell-known services" + "\n\taddress = ${toByteString()}\n\tprotocol = ${_protocol}\n\tservices = ${services}"
    }
}