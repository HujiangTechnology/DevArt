package com.hujiang.devart.component.dns.record

import com.hujiang.devart.component.dns.DNSInputStream
import com.hujiang.devart.component.dns.DNSRR

/**
 * Created by rarnu on 4/8/16.
 */
class MailDomain : DNSRR() {

    private var _mailDestination: String? = null

    override fun decode(dnsIn: DNSInputStream?) {
        _mailDestination = dnsIn?.readDomainName()
    }

    fun getMailDestination(): String? = _mailDestination

    override fun toString(): String = getRRName() + "\tmail destination = ${_mailDestination}"
}