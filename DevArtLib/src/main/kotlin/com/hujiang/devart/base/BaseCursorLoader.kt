package com.hujiang.devart.base

import android.content.AsyncTaskLoader
import android.content.Context
import android.database.Cursor

/**
 * Created by rarnu on 3/23/16.
 */
abstract class BaseCursorLoader(context: Context?) : AsyncTaskLoader<Cursor>(context) {

    abstract override fun loadInBackground(): Cursor?

    override fun onStartLoading() {
        forceLoad()
    }

    override fun onCanceled(data: Cursor?) {
        super.onCanceled(data)
    }

    override fun onStopLoading() {
        cancelLoad()
    }

    override fun onReset() {
        stopLoading()
    }

}