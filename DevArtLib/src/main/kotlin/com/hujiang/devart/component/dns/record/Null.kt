package com.hujiang.devart.component.dns.record

import com.hujiang.devart.component.dns.DNSInputStream
import com.hujiang.devart.component.dns.DNSRR
import java.nio.charset.Charset

/**
 * Created by rarnu on 4/8/16.
 */
class Null : DNSRR() {

    private var _data: ByteArray? = null
    private var _text: String? = null


    override fun decode(dnsIn: DNSInputStream?) {
        _data = ByteArray(dnsIn!!.available())
        dnsIn.read(_data)
        _text = String(_data!!, Charset.forName("latin1"))
    }

    fun getNullData(): ByteArray? {
        val copy = ByteArray(_data!!.size)
        System.arraycopy(_data, 0, copy, 0, _data!!.size)
        return copy
    }

    override fun toString(): String = getRRName() + "\tnull data = [${_text}]"
}