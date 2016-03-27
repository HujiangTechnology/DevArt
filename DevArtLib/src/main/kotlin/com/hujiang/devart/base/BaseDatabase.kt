package com.hujiang.devart.base

import android.content.ContentValues
import android.content.Context
import android.database.Cursor
import android.database.sqlite.SQLiteDatabase
import java.io.File

/**
 * Created by rarnu on 3/25/16.
 */
abstract class BaseDatabase {

    private var _database: SQLiteDatabase? = null

    constructor(context: Context) {
        var path = getDatabasePath(context)
        if (!path.endsWith("/")) {
            path += "/"
        }
        val name = getDatabaseFileName(context)
        val dbName = path + name
        val fDb = File(dbName)
        if (!fDb.exists()) {
            createOrCopyDatabaseFile(context, path, name, isCopyDatabase())
        }
        _database = SQLiteDatabase.openDatabase(dbName, null, SQLiteDatabase.OPEN_READWRITE)
    }


    private fun createOrCopyDatabaseFile(context: Context, path: String, name: String, copy: Boolean) {
        if (copy) {
            copyDatabaseFile(context, path, name)
        } else {
            try {
                val dbName = path + name
                if (File(dbName).createNewFile()) {
                    _database = SQLiteDatabase.openOrCreateDatabase(dbName, null)
                    val listSql = getListSqlCreateTables()
                    if (listSql != null && listSql.size != 0) {
                        for (sql in listSql) {
                            try {
                                _database?.execSQL(sql)
                            } catch (e: Exception) {
                            }
                        }
                    }
                    _database?.close()
                }
            } catch (e: Exception) {

            }
        }
    }

    abstract fun getDatabasePath(context: Context): String

    abstract fun getDatabaseFileName(context: Context): String

    abstract fun getListSqlCreateTables(): MutableList<String>?

    abstract fun isCopyDatabase(): Boolean

    abstract fun copyDatabaseFile(context: Context, path: String, name: String)

    fun query(table: String?, columns: Array<String>?, selection: String?, selectionArgs: Array<String>?, groupBy: String?, having: String?, orderBy: String?): Cursor? =
            _database?.query(table, columns, selection, selectionArgs, groupBy, having, orderBy)


    fun query(table: String?, columns: Array<String>?, selection: String?, selectionArgs: Array<String>?, groupBy: String?, having: String?, orderBy: String?, limit: String): Cursor? =
            _database?.query(false, table, columns, selection, selectionArgs, groupBy, having, orderBy, limit)


    fun query(distinct: Boolean, table: String?, columns: Array<String>, selection: String?, selectionArgs: Array<String>?, groupBy: String?, having: String?, orderBy: String?, limit: String?): Cursor? =
            _database?.query(distinct, table, columns, selection, selectionArgs, groupBy, having, orderBy, limit)


    fun rawQuery(sql: String): Cursor? =
            _database?.rawQuery(sql, null)

    fun rawQuery(sql: String, args: Array<String>?): Cursor? =
            _database?.rawQuery(sql, args)


    fun insert(table: String, values: ContentValues?): Long? = try {
        _database?.insert(table, null, values)
    } catch(e: Exception) {
        -1
    }

    fun update(table: String, values: ContentValues?, whereClause: String, whereArgs: Array<String>?): Int? = try {
        _database?.update(table, values, whereClause, whereArgs)
    } catch(e: Exception) {
        -1
    }

    fun delete(table: String, whereClause: String?, whereArgs: Array<String>?): Int? = try {
        _database?.delete(table, whereClause, whereArgs)
    } catch(e: Exception) {
        -1
    }

    fun execSQL(sql: String) = try {
        _database?.execSQL(sql)
    } catch(e: Exception) {
    }

    fun close() = _database?.close()

}