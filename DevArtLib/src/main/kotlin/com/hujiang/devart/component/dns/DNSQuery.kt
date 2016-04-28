package com.hujiang.devart.component.dns

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import java.io.EOFException
import java.io.IOException
import java.util.*

/**
 * Created by rarnu on 4/8/16.
 */
class DNSQuery {

    companion object {
        private var _globalID = 0
    }

    private var _queryHost: String? = null
    private var _queryType = 0
    private var _queryClass = 0
    private var _queryID = 0
    private var _answers = Vector<DNSRR?>()
    private var _authorities = Vector<DNSRR?>()
    private var _additional = Vector<DNSRR?>()
    private var _authoritative = false
    private var _truncated = false
    private var _recursive = false

    constructor(host: String?, type: Int, clas: Int) {
        val labels = StringTokenizer(host, ".")
        while (labels.hasMoreTokens()) {
            if (labels.nextToken().length > 63) {
                throw IllegalArgumentException("Invalid hostname: " + host)
            }
        }
        _queryHost = host
        _queryType = type
        _queryClass = clas
        synchronized (javaClass) {
            _queryID = (++_globalID) % 65536
        }
    }

    fun getQueryHost(): String? = _queryHost

    fun getQueryType(): Int = _queryType

    fun getQueryClass(): Int = _queryClass

    fun getQueryID(): Int = _queryID

    fun extractQuery(): ByteArray? {
        val byteArrayOut = ByteArrayOutputStream()
        val dataOut = DataOutputStream(byteArrayOut)
        try {
            dataOut.writeShort(_queryID)
            dataOut.writeShort((0 shl DNS.SHIFT_QUERY) or (DNS.OPCODE_QUERY shl DNS.SHIFT_OPCODE) or (1 shl DNS.SHIFT_RECURSE_PLEASE))
            dataOut.writeShort(1) // queries
            dataOut.writeShort(0) // answers
            dataOut.writeShort(0) // authorities
            dataOut.writeShort(0) // additional
            val labels = StringTokenizer(_queryHost, ".")
            while (labels.hasMoreTokens()) {
                val label = labels.nextToken()
                dataOut.writeByte(label.length)
                dataOut.writeBytes(label)
            }
            dataOut.writeByte(0)
            dataOut.writeShort(_queryType)
            dataOut.writeShort(_queryClass)
        } catch (e: IOException) {
        }
        return byteArrayOut.toByteArray()
    }

    fun receiveResponse(data: ByteArray?, length: Int) {
        val dnsIn = DNSInputStream(data, 0, length)
        val id = dnsIn.readShort()
        if (id != _queryID) {
            throw IOException("ID does not match request")
        }
        val flags = dnsIn.readShort()
        decodeFlags(flags)
        var numQueries = dnsIn.readShort()
        var numAnswers = dnsIn.readShort()
        var numAuthorities = dnsIn.readShort()
        var numAdditional = dnsIn.readShort()
        while (numQueries-- > 0) {
            dnsIn.readDomainName()
            dnsIn.readShort()
            dnsIn.readShort()
        }
        try {
            while (numAnswers-- > 0) {
                _answers.addElement(dnsIn.readRR())
            }
            while (numAuthorities-- > 0) {
                _authorities.addElement(dnsIn.readRR())
            }
            while (numAdditional-- > 0) {
                _additional.addElement(dnsIn.readRR())
            }
        } catch (e: EOFException) {
            if (!_truncated) {
                throw e
            }
        }
    }

    @Suppress("UNUSED_VARIABLE")
    protected fun decodeFlags(flags: Int) {
        val isResponse = ((flags shr DNS.SHIFT_QUERY) and 1) != 0
        if (!isResponse) {
            throw IOException("Response flag not set")
        }
        val opcode = (flags shr DNS.SHIFT_OPCODE) and 15
        _authoritative = ((flags shr DNS.SHIFT_AUTHORITATIVE) and 1) != 0
        _truncated = ((flags shr DNS.SHIFT_TRUNCATED) and 1) != 0
        val recurseRequest = ((flags shr DNS.SHIFT_RECURSE_PLEASE) and 1) != 0
        _recursive = ((flags shr DNS.SHIFT_RECURSE_AVAILABLE) and 1) != 0
        val code = (flags shr DNS.SHIFT_RESPONSE_CODE) and 15
        if (code != 0) {
            throw IOException(DNS.codeName(code) + " ($code)")
        }
    }

    fun isAuthoritative(): Boolean = _authoritative

    fun isTruncated(): Boolean = _truncated

    fun isRecursive(): Boolean = _recursive

    fun getAnswers(): Enumeration<DNSRR?>? = _answers.elements()

    fun getAuthorities(): Enumeration<DNSRR?>? = _authorities.elements()

    fun getAdditional(): Enumeration<DNSRR?>? = _additional.elements()

}