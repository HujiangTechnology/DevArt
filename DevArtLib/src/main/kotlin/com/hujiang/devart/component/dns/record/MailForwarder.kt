package com.hujiang.devart.component.dns.record

import com.hujiang.devart.component.dns.DNSInputStream
import com.hujiang.devart.component.dns.DNSRR

/**
 * Created by rarnu on 4/8/16.
 */
class MailForwarder : DNSRR() {

    private var _mailForwarder: String? = null

    override fun decode(dnsIn: DNSInputStream?) {
        _mailForwarder = dnsIn?.readDomainName()
    }

    fun getMailForwarder(): String? = _mailForwarder

    override fun toString(): String = getRRName() + "\tmail forwarder = $_mailForwarder"
}