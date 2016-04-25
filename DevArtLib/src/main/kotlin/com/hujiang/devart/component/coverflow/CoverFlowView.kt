package com.hujiang.devart.component.coverflow

import android.content.Context
import android.graphics.*
import android.os.Handler
import android.util.AttributeSet
import android.view.View
import com.hujiang.devart.component.recycler.LinearLayoutManager
import com.hujiang.devart.component.recycler.RecyclerView

/**
 * Created by rarnu on 4/19/16.
 */
class CoverFlowView: RecyclerView {

    companion object {
        val VERTICAL = 1
        val HORIZONTAL = 2
    }

    private var _lastPosition = 0
    private var _currentPosition = 0
    private var _leftBorderPosition = 0
    private var _rightBorderPosition = 0
    private var _orientation = 0
    private var _flag = false
    private var _coverFlowListener: CoverFlowItemListener? = null
    private var _layoutManager: LinearLayoutManager? = null
    private val _camera = Camera()
    private val _matrix = Matrix()
    private val _paint = Paint(Paint.FILTER_BITMAP_FLAG)

    constructor(context: Context): super(context) {
        init()
    }

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        init()
    }

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        init()
    }

    private fun init() {
        _paint.isAntiAlias = true
        this.isChildrenDrawingOrderEnabled = true
        this.addOnScrollListener(CoverFlowScrollListener())
    }

    fun scrollToCenter(position: Int) {
        if (position <= _rightBorderPosition && position >= _leftBorderPosition) {
            val firstPosition = _layoutManager!!.findFirstVisibleItemPosition()
            val currentPosition = position - firstPosition
            val targetChild = getChildAt(currentPosition)
            val location = IntArray(2)
            targetChild.getLocationInWindow(location)
            val targetItemX = location[0] + targetChild.width / 2
            val size = Point()
            display.getSize(size)
            val width = size.x
            val centerX = width / 2
            Handler().post { smoothScrollBy(targetItemX - centerX, 0) }
        }
    }

    override fun drawChild(canvas: Canvas?, child: View?, drawingTime: Long): Boolean {
        val bitmap = getChildDrawingCache(child)
        val top = child!!.top
        val left = child.left
        val childCenterY = child.height / 2
        val childCenterX = child.width / 2
        val parentCenterY = height / 2
        val parentCenterX = width / 2
        val absChildCenterY = child.top + childCenterY
        val absChildCenterX = child.left + childCenterX
        val distanceY = parentCenterY - absChildCenterY
        val distanceX = parentCenterX - absChildCenterX
        if (_orientation == HORIZONTAL) {
            prepareMatrix(_matrix, distanceX, width / 2)
        } else {
            prepareMatrix(_matrix, distanceY, height / 2)
        }
        _matrix.preTranslate(-childCenterX.toFloat(), -childCenterY.toFloat())
        _matrix.postTranslate(childCenterX.toFloat(), childCenterY.toFloat())
        _matrix.postTranslate(left.toFloat(), top.toFloat())
        canvas?.drawBitmap(bitmap, _matrix, _paint)
        return false
    }

    private fun prepareMatrix(outMatrix: Matrix?, distanceY: Int, r: Int) {
        val d = Math.min(r, Math.abs(distanceY))
        val translateZ = Math.sqrt(((r * r) - (d * d)).toDouble()).toFloat()
        _camera.save()
        _camera.translate(0.0f, 0.0f, r - translateZ)
        _camera.getMatrix(outMatrix)
        _camera.restore()
    }

    override fun getChildDrawingOrder(childCount: Int, i: Int): Int {
        val centerChild = childCount / 2
        if (!_flag) {
            (_adapter as CoverFlowAdapter).setBorderPosition(centerChild)
            _leftBorderPosition = centerChild
            _rightBorderPosition = (_adapter as CoverFlowAdapter).getItemCount() - centerChild - 1
            _flag = true
        }
        _currentPosition = _layoutManager!!.findFirstVisibleItemPosition() + centerChild
        if (_lastPosition != _currentPosition) {
            _lastPosition = _currentPosition
            _coverFlowListener?.onItemChanged(_currentPosition)
        }
        var rez = i
        if (i > centerChild) {
            rez = (childCount - 1) - i + centerChild
        } else if (i == centerChild) {
            rez = childCount - 1
        } else {
            rez = i
        }
        return rez
    }

    private fun getChildDrawingCache(child: View?): Bitmap? {
        var bitmap = child?.drawingCache
        if (bitmap == null) {
            child?.isDrawingCacheEnabled = true
            child?.buildDrawingCache()
            bitmap = child?.drawingCache
        }
        return bitmap
    }

    override fun getLayoutManager(): LinearLayoutManager? = _layoutManager

    fun setCoverFlowListener(coverFlowListener: CoverFlowItemListener?) {
        _coverFlowListener = coverFlowListener
    }

    fun setOrientation(orientation: Int) {
        _orientation = orientation
        var itemDecoration: DividerItemDecoration
        if (orientation == VERTICAL) {
            _layoutManager = LinearLayoutManager(context, LinearLayoutManager.VERTICAL, false)
            itemDecoration = DividerItemDecoration(0, -50)
        } else {
            _layoutManager = LinearLayoutManager(context, LinearLayoutManager.HORIZONTAL, false)
            itemDecoration = DividerItemDecoration(-50, 0)
        }
        _layoutManager = _layoutManager
        addItemDecoration(itemDecoration)
    }

    inner class CoverFlowScrollListener: RecyclerView.OnScrollListener() {

        override fun onScrollStateChanged(recyclerView: RecyclerView?, newState: Int) {
            super.onScrollStateChanged(recyclerView, newState)
            if (newState == RecyclerView.SCROLL_STATE_IDLE) {
                _coverFlowListener?.onItemSelected(_currentPosition)
                if (_currentPosition > _rightBorderPosition) {
                    scrollToCenter(_rightBorderPosition)
                    return
                }
                if (_currentPosition < _leftBorderPosition) {
                    scrollToCenter(_leftBorderPosition)
                    return
                }
                val firstPosition = _layoutManager!!.findFirstVisibleItemPosition()
                val centerChild = this@CoverFlowView.getChildAt(_currentPosition - firstPosition)
                val location = IntArray(2)
                centerChild.getLocationInWindow(location)
                val centerItemX = location[0] + centerChild.width / 2
                val size = Point()
                display.getSize(size)
                val width = size.x
                val centerX = width / 2
                this@CoverFlowView.smoothScrollBy(centerItemX - centerX, 0)
            }
        }
    }

    class DividerItemDecoration: RecyclerView.ItemDecoration {

        private var _leftPadding = 0
        private var _topPadding = 0

        constructor(leftPadding: Int, topPadding: Int) {
            _leftPadding = leftPadding
            _topPadding = topPadding
        }

        override fun getItemOffsets(outRect: Rect?, view: View?, parent: RecyclerView?, state: State?) {
            if (view!!.id == 0) {
                return
            }
            outRect?.left = _leftPadding
            outRect?.top = _topPadding
        }
    }

    interface CoverFlowItemListener {
        fun onItemChanged(position: Int)
        fun onItemSelected(position: Int)
    }

}