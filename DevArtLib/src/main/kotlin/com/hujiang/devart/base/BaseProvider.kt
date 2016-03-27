package com.hujiang.devart.base

import android.content.ContentProvider
import android.content.ContentUris
import android.content.ContentValues
import android.content.Context
import android.database.Cursor
import android.net.Uri
import android.os.CancellationSignal
import com.hujiang.devart.base.BaseDatabase

/**
 * Created by rarnu on 3/25/16.
 */
abstract class BaseProvider: ContentProvider() {

    companion object {
        var CONTENT_URI: Uri? = null
    }

    abstract fun getUriName(): String?

    abstract fun createDatabase(context: Context): BaseDatabase?

    abstract fun doQuery(database: BaseDatabase?, actionId: Int, uri: Uri?, projection: Array<String>?, selection: String?, selectionArgs: Array<String>?, sortOrder: String?): Cursor?

    abstract fun doUpdate(database: BaseDatabase?, actionId: Int, uri: Uri?, values: ContentValues?, selection: String?, selectionArgs: Array<String>?): Int

    abstract fun doInsert(database: BaseDatabase?, actionId: Int, uri: Uri?, values: ContentValues?): Uri?

    abstract fun doDelete(database: BaseDatabase?, actionId: Int, uri: Uri?, selection: String?, selectionArgs: Array<String>?): Int

    abstract override fun getType(uri: Uri?): String?

    var database: BaseDatabase? = null


    override fun delete(uri: Uri?, selection: String?, selectionArgs: Array<String>?): Int {
        val actionId = extractActionId(uri)
        var ret = -1
        if (database != null) {
            ret = doDelete(database, actionId, uri, selection, selectionArgs)
        }
        return ret
    }

    override fun insert(uri: Uri?, values: ContentValues?): Uri? {
        val actionId = extractActionId(uri)
        var u: Uri? = null
        if (database != null) {
            u = doInsert(database, actionId, uri, values)
        }
        return u
    }

    override fun onCreate(): Boolean {
        CONTENT_URI = Uri.parse("content://" + getUriName())
        if (database == null) {
            try {
                database = createDatabase(context)
            } catch (e: Exception) {
            }
        }
        return database != null
    }

    override fun query(uri: Uri?, projection: Array<String>?, selection: String?, selectionArgs: Array<String>?, sortOrder: String?): Cursor? {
        val actionId = extractActionId(uri)
        var c: Cursor? = null
        if (database != null) {
            c = doQuery(database, actionId, uri, projection, selection, selectionArgs, sortOrder)
        }
        return c
    }

    override fun update(uri: Uri?, values: ContentValues?, selection: String?, selectionArgs: Array<String>?): Int {
        val actionId = extractActionId(uri)
        var ret = -1
        if (database != null) {
            ret = doUpdate(database, actionId, uri, values, selection, selectionArgs)
        }
        return ret
    }

    private fun extractActionId(uri: Uri?): Int = try {
        ContentUris.parseId(uri).toInt()
    } catch(e: Exception) {
        -1
    }
}