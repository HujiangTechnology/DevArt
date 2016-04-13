package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.MenuItem
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.emulator.EmulatorView
import com.hujiang.devart.component.emulator.ShellTermSession
import com.hujiang.devart.component.emulator.TermSession
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.InputMethodUtils
import com.hujiang.devart.utils.UIUtils

/**
 * Created by rarnu on 4/12/16.
 */
class TerminalFragment : BaseFragment() {

    private var _emu: EmulatorView? = null
    private var _session: TermSession? = null
    private var _itemSendCtrl: MenuItem? = null
    private var _itemSendFn: MenuItem? = null
    private var _itemToggleInputMethod: MenuItem? = null

    override fun getBarTitle(): Int = R.string.emuview_name

    override fun getBarTitleWithPath(): Int = R.string.emuview_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _emu = innerView?.findViewById(R.id.ev) as EmulatorView
        _session = ShellTermSession("")
        _session?.setDefaultUTF8Mode(true)
        _emu?.attachSession(_session)
        _emu?.setDensity(UIUtils.dm)
        _emu?.setTextSize(10)
    }

    override fun initEvents() {

    }

    override fun initLogic() {

    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_terminal

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) {
        _itemToggleInputMethod = menu?.add(0, 10, 98, "")
        _itemToggleInputMethod?.setIcon(android.R.drawable.ic_menu_sort_alphabetically)
        _itemToggleInputMethod?.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM)
        _itemSendCtrl = menu?.add(0, 11, 99, "")
        _itemSendCtrl?.title = "CTRL"
        _itemSendCtrl?.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM)
        _itemSendFn = menu?.add(0, 12, 100, "")
        _itemSendFn?.title = "FN"
        _itemSendFn?.setShowAsAction(MenuItem.SHOW_AS_ACTION_IF_ROOM)
    }

    override fun onOptionsItemSelected(item: MenuItem?): Boolean {
        when (item!!.itemId) {
            10 -> InputMethodUtils.toggleSoftKeyboard(activity)
            11 -> _emu?.sendControlKey()
            12 -> _emu?.sendFnKey()
        }
        return true
    }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onDestroy() {
        _session?.finish()
        super.onDestroy()
    }
}