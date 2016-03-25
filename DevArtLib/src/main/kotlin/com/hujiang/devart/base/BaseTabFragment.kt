package com.hujiang.devart.base

import android.app.ActionBar
import android.app.Fragment
import android.app.FragmentManager
import android.app.FragmentTransaction
import android.os.Build
import android.support.v4.view.ViewPager
import com.hujiang.devart.R
import com.hujiang.devart.base.inner.InnerFragment
import java.lang.reflect.Field

/**
 * Created by rarnu on 3/24/16.
 */
abstract class BaseTabFragment: InnerFragment, ActionBar.TabListener, ViewPager.OnPageChangeListener {

    protected var bar: ActionBar? = null
    private var pager: ViewPager? = null
    private var adapter: BaseFragmentStateAdapter? = null
    private var listFragment: MutableList<Fragment?>? = null
    var currentPage = 0
    private var needRelease = true

    constructor(): super()

    constructor(tabTitle: String): super(tabTitle)

    constructor(needRelease: Boolean): super() {
        this.needRelease = needRelease
    }

    override fun onDestroyView() {
        if (needRelease) {
            bar?.removeAllTabs()
            bar?.navigationMode = ActionBar.NAVIGATION_MODE_STANDARD
            adapter = null
            listFragment = null
            pager?.post { pager?.adapter = null }
        }
        super.onDestroyView()
    }

    override fun initComponents() {
        bar = activity.actionBar
        bar?.navigationMode = ActionBar.NAVIGATION_MODE_TABS
        pager = innerView?.findViewById(R.id.pager) as ViewPager
        pager?.offscreenPageLimit = 3
        listFragment = arrayListOf<Fragment?>()
        initFragmentList(listFragment)

        var fm: FragmentManager?
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1) {
            fm = childFragmentManager
        } else {
            fm = fragmentManager
        }
        if (fm != null) {
            adapter = BaseFragmentStateAdapter(fm, listFragment)
            pager?.post { pager?.adapter = adapter }
        }
        initTab()
    }

    override fun getFragmentLayoutResId(): Int = R.layout.layout_tab

    fun addTab(position: Int, fragment: BaseFragment) {
        if (listFragment!!.indexOf(fragment) == -1) {
            val t = bar?.newTab()?.setText(fragment.tabTitle)?.setTabListener(this)
            if (fragment.tabIcon != -1) {
                t?.setIcon(fragment.tabIcon)
            }
            if (position == -1) {
                listFragment?.add(fragment)
                bar?.addTab(t)
            } else {
                listFragment?.add(position, fragment)
                bar?.addTab(t, position)
            }

            var fm: FragmentManager?
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1) {
                fm = childFragmentManager
            } else {
                fm = fragmentManager
            }
            if (fm != null) {
                adapter = BaseFragmentStateAdapter(fm, listFragment)
                pager?.post {
                    pager?.adapter = adapter
                    val newPosition = if (position == -1) { listFragment!!.size - 1 } else { position }
                    pager?.currentItem = newPosition
                }
            }

        }
    }

    fun removeTab(position: Int) {
        var newPosition = position
        if (listFragment!!.size <= position) {
            return
        }
        listFragment?.removeAt(position)
        bar?.removeTabAt(position)
        newPosition--
        if (newPosition < 0) {
            newPosition = 0
        }
        val nPos = newPosition
        var fm: FragmentManager?
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1) {
            fm = childFragmentManager
        } else {
            fm = fragmentManager
        }
        if (fm != null) {
            adapter = BaseFragmentStateAdapter(fm, listFragment)
            pager?.post {
                pager?.adapter = adapter
                pager?.currentItem = nPos
            }
        }
    }

    abstract fun initFragmentList(listFragment: MutableList<Fragment?>?)

    private fun initTab() {
        bar?.removeAllTabs()
        for (bf in listFragment!!) {
            if (bf != null) {
                val t = bar?.newTab()?.setText((bf as BaseFragment).tabTitle)?.setTabListener(this)
                if ((bf as BaseFragment).tabIcon != -1) {
                    t?.setIcon(bf.tabIcon)
                }
                bar?.addTab(t)
            }
        }
    }

    override fun initEvents() {
        pager?.addOnPageChangeListener(this)
    }

    override fun initLogic() {
        pager?.post { pager?.currentItem = 0 }
    }

    override fun onTabReselected(tab: ActionBar.Tab?, ft: FragmentTransaction?) { }

    override fun onTabSelected(tab: ActionBar.Tab?, ft: FragmentTransaction?) {
        if (pager?.currentItem != tab!!.position) {
            currentPage = tab.position
            pager?.post {
                pager?.adapter?.notifyDataSetChanged()
                pager?.currentItem = tab.position
            }
        }
    }

    override fun onTabUnselected(tab: ActionBar.Tab?, ft: FragmentTransaction?) { }

    override fun onPageScrollStateChanged(state: Int) { }

    override fun onPageScrolled(position: Int, positionOffset: Float, positionOffsetPixel: Int) { }

    override fun onPageSelected(position: Int) {
        currentPage = position
        bar?.setSelectedNavigationItem(position)
    }

    fun setTabPosition(position: Int) {
        bar?.setSelectedNavigationItem(position)
        pager?.post { pager?.currentItem = position }
    }

    override fun onDetach() {
        super.onDetach()
        try {
            var fChildFm: Field?
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR1) {
                fChildFm = Fragment::class.java.getDeclaredField("mChildFragmentManager")
            } else {
                fChildFm = Fragment::class.java.getDeclaredField("mFragmentManager")
            }
            if (fChildFm != null) {
                fChildFm.isAccessible = true
                fChildFm.set(this, null)
            }
        } catch (e: Exception) {

        }
    }

}