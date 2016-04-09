package com.hujiang.devart.component.dns.record

import com.hujiang.devart.component.dns.DNSInputStream
import com.hujiang.devart.component.dns.DNSRR

/**
 * Created by rarnu on 4/8/16.
 */
class Pointer : DNSRR() {

    private var _pointer: String? = null

    override fun decode(dnsIn: DNSInputStream?) {
        _pointer = dnsIn?.readDomainName()
    }

    fun getPointer(): String? = _pointer

    override fun toString(): String = getRRName() + "\tpointer = ${_pointer}"
}