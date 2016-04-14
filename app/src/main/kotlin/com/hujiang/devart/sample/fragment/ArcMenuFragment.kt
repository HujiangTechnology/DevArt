package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.ImageView
import android.widget.Toast
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.arcmenu.ArcMenu
import com.hujiang.devart.component.arcmenu.ArcRayMenuIntf
import com.hujiang.devart.component.arcmenu.RayMenu
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 4/14/16.
 */
class ArcMenuFragment: BaseFragment() {

    companion object {
        private val ITEM_DRAWABLES = intArrayOf(R.drawable.composer_camera, R.drawable.composer_music,
            R.drawable.composer_place, R.drawable.composer_sleep, R.drawable.composer_thought, R.drawable.composer_with)
    }

    private var _arcMenu: ArcMenu? = null
    private var _arcMenu2: ArcMenu? = null
    private var _rayMenu: RayMenu? = null

    override fun getBarTitle(): Int = R.string.arcmenu_name

    override fun getBarTitleWithPath(): Int = R.string.arcmenu_name_with_name

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _arcMenu = innerView?.findViewById(R.id.arc_menu) as ArcMenu
        _arcMenu2 = innerView?.findViewById(R.id.arc_menu_2) as ArcMenu
        _rayMenu = innerView?.findViewById(R.id.ray_menu) as RayMenu

        initArcMenu(_arcMenu)
        initArcMenu(_arcMenu2)
        initArcMenu(_rayMenu)
    }

    override fun initEvents() { }

    override fun initLogic() { }

    private fun initArcMenu(v: View?) {
        val itemCount = ITEM_DRAWABLES.size
        for (i in 0..itemCount - 1) {
            val item = ImageView(activity)
            item.setImageResource(ITEM_DRAWABLES[i])
            val position = i
            (v as ArcRayMenuIntf).addItem(item, View.OnClickListener { Toast.makeText(activity, "position: ${position}", Toast.LENGTH_SHORT).show() })
        }
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_arcmenu

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null
}