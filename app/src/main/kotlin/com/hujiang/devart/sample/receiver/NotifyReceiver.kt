package com.hujiang.devart.sample.receiver

import android.content.Context
import android.widget.Toast
import com.hujiang.devart.base.BaseNotifyReceiver

/**
 * Created by rarnu on 4/7/16.
 */
class NotifyReceiver: BaseNotifyReceiver() {
    override fun onReceiveNotify(context: Context?, id: Int) {
        Toast.makeText(context, id.toString(), Toast.LENGTH_SHORT).show()
    }
}