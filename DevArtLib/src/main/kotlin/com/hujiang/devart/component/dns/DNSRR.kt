package com.hujiang.devart.component.dns

/**
 * Created by rarnu on 4/8/16.
 */
abstract class DNSRR {

    private var _rrName: String? = null
    private var _rrType = 0
    private var _rrClass = 0
    private var _rrTTL = 0L
    private var _rrCreated = 0L


    fun init(name: String?, type: Int, clas: Int, ttl: Long, dnsIn: DNSInputStream?) {
        _rrName = name
        _rrType = type
        _rrClass = clas
        _rrTTL = ttl
        _rrCreated = System.currentTimeMillis()
        decode(dnsIn)
    }

    protected abstract fun decode(dnsIn: DNSInputStream?)

    fun getRRName(): String? = _rrName

    fun getRRType(): Int = _rrType

    fun getRRClass(): Int = _rrClass

    fun getRRTTL(): Long = _rrTTL

    fun isValid(): Boolean = _rrTTL * 1000 > System.currentTimeMillis() - _rrCreated

}