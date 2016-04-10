package com.hujiang.devart.component.dns.record

import com.hujiang.devart.component.dns.DNSInputStream
import com.hujiang.devart.component.dns.DNSRR
import java.util.*

/**
 * Created by rarnu on 4/8/16.
 */
class Text : DNSRR() {

    private var _texts = Vector<String>()

    override fun decode(dnsIn: DNSInputStream?) {
        var s: String?
        while (true) {
            s = dnsIn?.readString()
            if (s != null) {
                _texts.add(s)
            } else {
                break
            }
        }
    }

    fun getTexts(): Enumeration<String> = _texts.elements()

    override fun toString(): String {
        val result = StringBuffer()
        for (i in 0.._texts.size - 1) {
            if (i > 0) {
                result.append("\n\t\t")
            }
            result.append(_texts.elementAt(i))
        }
        return getRRName() + "\ttext = ${result}"
    }
}