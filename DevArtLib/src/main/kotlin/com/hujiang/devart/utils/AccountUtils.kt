package com.hujiang.devart.utils

import android.accounts.Account
import android.accounts.AccountManager
import android.content.Context
import java.util.regex.Pattern

/**
 * Created by rarnu on 3/29/16.
 */
object AccountUtils {

    fun getAllAccounts(context: Context): List<Account?>? {
        val am = AccountManager.get(context)
        return am.accounts.asList()
    }

    fun getBindedEmailAddress(context: Context): String = try {
        var ret = ""
        val am = AccountManager.get(context)
        val accs = am.accounts
        var google = ""
        var email = ""
        var other = ""
        for (a in accs) {
            if (a.type == "com.google") {
                google = a.name
            } else if (a.type == "com.android.email") {
                email = a.name
            } else {
                if (isEmail(a.name)) {
                    other = a.name
                }
            }
        }
        if (google != "") {
            ret = google
        }
        if (ret == "" && email != "") {
            ret = email
        }
        if (ret == "") {
            ret = other
        }
        ret
    } catch (e: Exception) {
        ""
    }

    private fun isEmail(text: String): Boolean =
            Pattern.compile("^([a-z0-9A-Z]+[-|\\.]?)+[a-z0-9A-Z]@([a-z0-9A-Z]+(-[a-z0-9A-Z]+)?\\.)+[a-zA-Z]{2,}$").matcher(text).matches()

}