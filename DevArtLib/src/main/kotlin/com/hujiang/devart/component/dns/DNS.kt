package com.hujiang.devart.component.dns

/**
 * Created by rarnu on 4/8/16.
 */
object DNS {

    val DEFAULT_PORT = 53
    val TYPE_A = 1 // address
    val TYPE_NS = 2 // nameserver
    val TYPE_MD = 3 // mail domain
    val TYPE_MF = 4 // mail forwarder
    val TYPE_CNAME = 5 // canonical name
    val TYPE_SOA = 6 // start of authority
    val TYPE_MB = 7 // mail box
    val TYPE_MG = 8 // mail group
    val TYPE_MR = 9 // mail rename
    val TYPE_NULL = 10 // null
    val TYPE_WKS = 11 // well-known services
    val TYPE_PTR = 12 // pointer
    val TYPE_HINFO = 13 // host info
    val TYPE_MINFO = 14 // mail info
    val TYPE_MX = 15 // mail exchanger
    val TYPE_TXT = 16 // text
    val TYPE_AXFR = 252 // zone transfer request
    val TYPE_MAILB = 253 // mailbox request
    val TYPE_MAILA = 254 // mail agent request
    val TYPE_ANY = 255 // request any

    val CLASS_IN = 1 // internet
    val CLASS_CS = 2 // csnet
    val CLASS_CH = 3 // chaos
    val CLASS_HS = 4 // hesiod
    val CLASS_ANY = 255 // request any

    val SHIFT_QUERY = 15
    val SHIFT_OPCODE = 11
    val SHIFT_AUTHORITATIVE = 10
    val SHIFT_TRUNCATED = 9
    val SHIFT_RECURSE_PLEASE = 8
    val SHIFT_RECURSE_AVAILABLE = 7
    val SHIFT_RESERVED = 4
    val SHIFT_RESPONSE_CODE = 0

    val OPCODE_QUERY = 0
    val OPCODE_IQUERY = 1
    val OPCODE_STATUS = 2

    private val _typeNames = arrayOf(
            "Address", "NameServer", "MailDomain", "MailForwarder", "CanonicalName",
            "StartOfAuthority", "MailBox", "MailGroup", "MailRename", "Null",
            "WellKnownServices", "Pointer", "HostInfo", "MailInfo", "MailExchanger", "Text")
    private val _codeNames = arrayOf(
            "Format error", "Server failure", "Name not known", "Not implemented", "Refused")

    fun typeName(type: Int): String {
        return if (type >= 1 && type <= 16) _typeNames[type - 1] else "Unknown"
    }

    fun codeName(code: Int): String {
        return if (code >= 1 && code <= 5) _codeNames[code - 1] else "Unknown error"
    }

}