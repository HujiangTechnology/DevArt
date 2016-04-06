package com.hujiang.devart.component.flip

import android.util.SparseArray
import android.view.View

/**
 * Created by rarnu on 4/6/16.
 */
class Recycler {

    private var _scraps: Array<SparseArray<Scrap?>?>? = null
    private var _currentScraps: SparseArray<Scrap?>? = null
    private var _viewTypeCount = 0

    fun setViewTypeCount(viewTypeCount: Int) {
        if (viewTypeCount < 1) {
            throw IllegalArgumentException("Can't have a viewTypeCount < 1")
        }
        if (_currentScraps != null && viewTypeCount == _scraps!!.size) {
            return
        }
        val scrapViews = arrayOfNulls<SparseArray<Scrap?>>(viewTypeCount)
        for (i in 0..viewTypeCount - 1) {
            scrapViews[i] = SparseArray<Scrap?>()
        }
        _viewTypeCount = viewTypeCount
        _currentScraps = scrapViews[0]
        _scraps = scrapViews
    }

    fun getScrapView(position: Int, viewType: Int): Scrap? {
        if (_viewTypeCount == 1) {
            return retrieveFromScrap(_currentScraps, position)
        } else if (viewType >= 0 && viewType < _scraps!!.size) {
            return retrieveFromScrap(_scraps!![viewType], position)
        }
        return null
    }

    companion object {
        fun retrieveFromScrap(scrapViews: SparseArray<Scrap?>?, position: Int): Scrap? {
            val size = scrapViews!!.size()
            if (size > 0) {
                var result = scrapViews.get(position, null)
                if (result != null) {
                    scrapViews.remove(position)
                    return result
                }
                val index = size - 1
                result = scrapViews.valueAt(index)
                scrapViews.removeAt(index)
                result?.valid = false
                return result
            }
            return null
        }
    }

    fun addScrapView(scrap: View?, position: Int, viewType: Int) {
        val item = Scrap(scrap, true)
        if (_viewTypeCount == 1) {
            _currentScraps?.put(position, item)
        } else {
            _scraps!![viewType]?.put(position, item)
        }
        scrap?.setAccessibilityDelegate(null)
    }

    fun invalidateScraps() {
        for (array in _scraps!!) {
            for (i in 0..array!!.size() - 1) {
                array.valueAt(i)?.valid = false
            }
        }
    }

    data class Scrap(var scrap: View?, var valid: Boolean)

}