package com.hujiang.devart.component.dns.record

import com.hujiang.devart.component.dns.DNSInputStream
import com.hujiang.devart.component.dns.DNSRR

/**
 * Created by rarnu on 4/8/16.
 */
class NameServer : DNSRR() {

    private var _nameServer: String? = null

    override fun decode(dnsIn: DNSInputStream?) {
        _nameServer = dnsIn?.readDomainName()
    }

    fun getNameServer(): String? = _nameServer

    override fun toString(): String = getRRName() + "\tnameserver = ${_nameServer}"
}