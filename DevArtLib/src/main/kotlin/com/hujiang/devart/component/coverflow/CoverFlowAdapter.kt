package com.hujiang.devart.component.coverflow

import android.database.DataSetObservable
import android.database.DataSetObserver
import android.graphics.Bitmap

/**
 * Created by rarnu on 4/26/16.
 */
abstract class CoverFlowAdapter {

    private val _dataSetObservable = DataSetObservable()

    open fun registerDataSetObserver(observer: DataSetObserver?) = _dataSetObservable.registerObserver(observer)

    open fun unregisterDataSetObserver(observer: DataSetObserver?) = _dataSetObservable.unregisterObserver(observer)

    open fun notifyDataSetChanged() = _dataSetObservable.notifyChanged()

    open fun notifyDataSetInvalidated() = _dataSetObservable.notifyInvalidated()

    open fun getItemViewType(position: Int): Int = 0

    open fun getViewTypeCount(): Int = 1

    abstract fun getCount(): Int

    abstract fun getImage(position: Int): Bitmap?

}