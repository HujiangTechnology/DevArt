package com.hujiang.devart.base

import android.content.AsyncTaskLoader
import android.content.Context

/**
 * Created by rarnu on 3/23/16.
 */
abstract class BaseClassLoader<T>(context: Context?) : AsyncTaskLoader<T>(context) {

    abstract override fun loadInBackground(): T?

    override fun onStartLoading() = forceLoad()

    override fun onCanceled(data: T?) = super.onCanceled(data)

    override fun onStopLoading() {
        cancelLoad()
    }

    override fun onReset() = stopLoading()
}