package com.hujiang.devart.component.dns.record

import com.hujiang.devart.component.dns.DNSInputStream
import com.hujiang.devart.component.dns.DNSRR

/**
 * Created by rarnu on 4/8/16.
 */
class MailInfo : DNSRR() {

    private var _rBox: String? = null
    private var _eBox: String? = null

    override fun decode(dnsIn: DNSInputStream?) {
        _rBox = dnsIn?.readDomainName()
        _eBox = dnsIn?.readDomainName()
    }

    fun getResponsibleMailbox(): String? = _rBox

    fun getErrorMailbox(): String? = _eBox

    override fun toString(): String = getRRName() + "\tresponsible mailbox = ${_rBox}, error mailbox = ${_eBox}"
}