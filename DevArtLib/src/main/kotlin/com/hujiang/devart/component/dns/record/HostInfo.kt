package com.hujiang.devart.component.dns.record

import com.hujiang.devart.component.dns.DNSInputStream
import com.hujiang.devart.component.dns.DNSRR

/**
 * Created by rarnu on 4/8/16.
 */
class HostInfo : DNSRR() {

    private var _cpu: String? = null
    private var _os: String? = null

    override fun decode(dnsIn: DNSInputStream?) {
        _cpu = dnsIn?.readString()
        _os = dnsIn?.readString()
    }

    fun getCPUInfo(): String? = _cpu

    fun getOSInfo(): String? = _os

    override fun toString(): String = getRRName() + "\tOS = ${_os}, CPU = ${_cpu}"
}