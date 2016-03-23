package com.hujiang.devart.base

import android.content.Context
import com.hujiang.devart.base.inner.InnerAdapter

/**
 * Created by rarnu on 3/23/16.
 */
abstract class BaseAdapter<T>(context: Context, list: MutableList<T>?) : InnerAdapter<T>(context, list)