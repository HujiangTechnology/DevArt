package com.hujiang.devart.sample.service

import android.app.Notification
import android.content.Intent
import com.hujiang.devart.base.BaseService
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 3/28/16.
 */
class DemoService: BaseService() {

    companion object {
        val DEMO_SERVICE_ACTION = "com.hujiang.devart.service"
    }
    private val _inService = Intent(DEMO_SERVICE_ACTION)

    override fun initIntent() {
        _inService.putExtra("operating", true)
    }

    override fun fiIntent() {
        _inService.putExtra("operating", false)
    }

    override fun showNotification(): Boolean = true

    override fun getSendIntent(): Intent? = _inService

    override fun doOperation(command: String, n: Notification?) = Thread.sleep(5000)

    override fun getCommandCondition(command: String): Boolean = true

    override fun getIcon24(): Int = R.mipmap.ic_launcher
}