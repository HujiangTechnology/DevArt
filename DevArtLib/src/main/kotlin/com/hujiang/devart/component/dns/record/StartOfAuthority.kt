package com.hujiang.devart.component.dns.record

import com.hujiang.devart.component.dns.DNSInputStream
import com.hujiang.devart.component.dns.DNSRR

/**
 * Created by rarnu on 4/8/16.
 */
class StartOfAuthority : DNSRR() {

    private var _origin: String? = null
    private var _mailAddress: String? = null
    private var _serial = 0L
    private var _refresh = 0L
    private var _retry = 0L
    private var _expire = 0L
    private var _ttl = 0L

    override fun decode(dnsIn: DNSInputStream?) {
        _origin = dnsIn!!.readDomainName()
        _mailAddress = dnsIn.readDomainName()
        _serial = dnsIn.readInt()
        _refresh = dnsIn.readInt()
        _retry = dnsIn.readInt()
        _expire = dnsIn.readInt()
        _ttl = dnsIn.readInt()
    }

    fun getOrigin(): String? = _origin

    fun getMailAddress(): String? = _mailAddress

    fun getSerial(): Long = _serial

    fun getRefresh(): Long = _refresh

    fun getRetry(): Long = _retry

    fun getExpire(): Long = _expire

    fun getTTL(): Long = _ttl

    override fun toString(): String = getRRName() + "\tstart of authority\n\torigin = $_origin\n\tmail address = $_mailAddress\n\tserial = $_serial\n\trefresh = $_refresh\n\tretry = $_retry\n\texpire = $_expire\n\tminimum TTL = $_ttl"
}