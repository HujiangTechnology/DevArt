package com.hujiang.devart.component.coverflow

import android.app.ActivityManager
import android.content.Context
import android.database.DataSetObserver
import android.graphics.*
import android.os.Build
import android.util.AttributeSet
import android.util.LruCache
import android.util.SparseArray
import android.view.MotionEvent
import android.view.VelocityTracker
import android.view.View
import android.view.ViewConfiguration
import android.view.animation.AccelerateDecelerateInterpolator
import android.view.animation.AnimationUtils
import android.widget.Scroller
import com.hujiang.devart.R
import com.hujiang.devart.utils.ImageUtils

/**
 * Created by rarnu on 4/26/16.
 */
open class CoverFlowView<T: CoverFlowAdapter>: View {

    companion object {
        val DURATION = 200L
        val INVALID_POSITION = -1
        val DEFAULT_VISIBLE_IMAGES = 3
        val NO_POSITION = -1
        val CHILD_SPACING = -200
        val ALPHA_DATUM = 76
        val CARD_SCALE = 0.15f
        val MOVE_POS_MULTIPLE = 3.0f
        val TOUCH_MINIMUM_MOVE = 5
        val MOVE_SPEED_MULTIPLE = 1.0f
        val MAX_SPEED = 6.0f
        val FRICTION = 10.0f
        val LONG_CLICK_DELAY = ViewConfiguration.getLongPressTimeout()
    }

    enum class CoverFlowGravity { TOP, BOTTOM, CENTER_VERTICAL }
    enum class CoverFlowLayoutMode { MATCH_PARENT, WRAP_CONTENT }

    var _visibleImages = DEFAULT_VISIBLE_IMAGES

    var _recycler: RecycleBin? = null
    var _coverFlowCenter = 0
    var _adapter: T? = null
    var _visibleChildCount = 0
    var _itemCount = 0
    var _dataSetChanged = false
    var _gravity: CoverFlowGravity? = null
    var _layoutMode: CoverFlowLayoutMode? = null
    var _coverFlowPadding: Rect? = null
    var _drawFilter: PaintFlagsDrawFilter? = null
    var _childTransformer: Matrix? = null
    var _reflectionTransformer: Matrix? = null
    var _drawChildPaint: Paint? = null
    var _touchRect: RectF? = null
    var _width = 0
    var _touchMoved = false
    var _touchStartPos = 0.0f
    var _touchStartX = 0.0f
    var _touchStartY = 0.0f
    var _offset = 0.0f
    var _startOffset = 0.0f
    var _startTime = 0L
    var _startSpeed = 0.0f
    var _duration = 0.0f
    var _animationRunnable: Runnable? = null
    var _velocity: VelocityTracker? = null
    var _childHeight = 0
    var _childTranslateY = 0
    var _reflectionTranslateY = 0
    var _reflectHeightFraction = 0.0f
    var _reflectGap = 0
    var _topImageClickEnable = true
    var _coverFlowListener: CoverFlowListener<T>? = null
    var _longClickListener: TopImageLongClickListener? = null
    var _longClickRunnable: LongClickRunnable? = null
    var _longClickPosted = false
    var _longClickTriggled = false
    var _topImageIndex = 0
    var _scroller: Scroller? = null
    var _imageRecorder: SparseArray<IntArray?>? = null
    var _standardAlpha = 0
    var _dataSetObserver = object: DataSetObserver() {

        override fun onChanged() {
            val nextItemCount = _adapter!!.getCount()
            if (_topImageIndex % _itemCount > nextItemCount - 1) {
                _offset = (nextItemCount - _visibleImages - 1).toFloat()
            } else {
                _offset += _visibleImages
                while (_offset < 0 || _offset >= _itemCount) {
                    if (_offset < 0) {
                        _offset += _itemCount
                    } else if (_offset >= _itemCount) {
                        _offset -= _itemCount
                    }
                }
                _offset -= _visibleImages
            }
            _itemCount = nextItemCount
            resetCoverFlow()
            requestLayout()
            invalidate()
            super.onChanged()
        }

        override fun onInvalidated() {
            super.onInvalidated()
        }
    }

    private fun resetCoverFlow() {
        if (_itemCount < 3) {
            throw  IllegalArgumentException("total count in adapter must larger than 3!")
        }
        val totalVisible = _visibleImages * 2 + 1
        if (_itemCount < totalVisible) {
            _visibleImages = (_itemCount - 1) / 2
        }
        _childHeight = 0
        _standardAlpha = (255 - ALPHA_DATUM) / _visibleImages
        _gravity = CoverFlowGravity.CENTER_VERTICAL
        _layoutMode = CoverFlowLayoutMode.WRAP_CONTENT
        _imageRecorder?.clear()
        _topImageIndex = INVALID_POSITION
        _dataSetChanged = true
    }

    constructor(context: Context): super(context) {
        init()
    }

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        initAttributes(context, attrs)
        init()
    }

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        initAttributes(context, attrs)
        init()
    }

    private fun initAttributes(context: Context, attrs: AttributeSet?) {
        val a = context.obtainStyledAttributes(attrs, R.styleable.ImageCoverFlowView)
        val totalVisibleChildren = a.getInt(R.styleable.ImageCoverFlowView_visibleImage, 3)
        setVisibleImage(totalVisibleChildren)
        _reflectHeightFraction = a.getFraction(R.styleable.ImageCoverFlowView_reflectionHeight, 100, 0, 0.0f)
        if (_reflectHeightFraction > 100) {
            _reflectHeightFraction = 100.0f
        }
        _reflectHeightFraction /= 100
        _reflectGap = a.getDimensionPixelSize(R.styleable.ImageCoverFlowView_reflectionGap, 0)
        _gravity = CoverFlowGravity.values()[a.getInt(R.styleable.ImageCoverFlowView_coverflowGravity, CoverFlowGravity.CENTER_VERTICAL.ordinal)]
        _layoutMode = CoverFlowLayoutMode.values()[a.getInt(R.styleable.ImageCoverFlowView_coverflowLayoutMode, CoverFlowLayoutMode.WRAP_CONTENT.ordinal)]
        a.recycle()
    }

    fun setVisibleImage(count: Int) {
        if (count % 2 == 0) {
            throw IllegalArgumentException("visible image must be an odd number")
        }
        if (count < 3) {
            throw IllegalArgumentException("visible image must larger than 3")
        }
        _visibleImages = count / 2
        _standardAlpha = (255 - ALPHA_DATUM) / _visibleImages
    }

    private fun init() {
        setWillNotDraw(false)
        isClickable = true
        _childTransformer = Matrix()
        _reflectionTransformer = Matrix()
        _touchRect = RectF()
        _imageRecorder = SparseArray()
        _drawChildPaint = Paint()
        _drawChildPaint?.isAntiAlias = true
        _drawChildPaint?.flags = Paint.ANTI_ALIAS_FLAG
        _coverFlowPadding = Rect()
        _drawFilter = PaintFlagsDrawFilter(0, Paint.ANTI_ALIAS_FLAG or Paint.FILTER_BITMAP_FLAG)
        _scroller = Scroller(context, AccelerateDecelerateInterpolator())
    }

    override fun onSizeChanged(w: Int, h: Int, oldw: Int, oldh: Int) {
        super.onSizeChanged(w, h, oldw, oldh)
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec)
        if (_adapter == null) {
            return
        }
        if (!_dataSetChanged) {
            return
        }
        _coverFlowPadding?.left = paddingLeft
        _coverFlowPadding?.right = paddingRight
        _coverFlowPadding?.top = paddingTop
        _coverFlowPadding?.bottom = paddingBottom
        val heightMode = MeasureSpec.getMode(heightMeasureSpec)
        val widthSize = MeasureSpec.getSize(widthMeasureSpec)
        var heightSize = MeasureSpec.getSize(heightMeasureSpec)
        val availableHeight = heightSize - _coverFlowPadding!!.top - _coverFlowPadding!!.bottom
        var maxChildTotalHeight = 0
        val visibleCount = (_visibleImages shl 1) + 1
        val mid = Math.floor(_offset + 0.5).toInt()
        val leftChild = visibleCount shr 1
        val startPos = getActuallyPosition(mid - leftChild)
        for (i in startPos..visibleCount + startPos - 1) {
            val child = _adapter?.getImage(i)
            val childHeight = child!!.height
            val childTotalHeight = (childHeight + childHeight * _reflectHeightFraction + _reflectGap).toInt()
            maxChildTotalHeight = if (maxChildTotalHeight < childTotalHeight) childTotalHeight else maxChildTotalHeight
        }

        if (heightMode == MeasureSpec.EXACTLY || heightMode == MeasureSpec.AT_MOST) {
            if (availableHeight < maxChildTotalHeight) {
                _childHeight = availableHeight
            } else {
                if (_layoutMode == CoverFlowLayoutMode.MATCH_PARENT) {
                    _childHeight = availableHeight
                } else if (_layoutMode == CoverFlowLayoutMode.WRAP_CONTENT) {
                    _childHeight = maxChildTotalHeight
                    if (heightMode == MeasureSpec.AT_MOST) {
                        heightSize = _childHeight + _coverFlowPadding!!.top+ _coverFlowPadding!!.bottom
                    }
                }
            }
        } else {
            if (_layoutMode == CoverFlowLayoutMode.MATCH_PARENT) {
                _childHeight = availableHeight
            } else if (_layoutMode == CoverFlowLayoutMode.WRAP_CONTENT) {
                _childHeight = maxChildTotalHeight
                heightSize = _childHeight + _coverFlowPadding!!.top + _coverFlowPadding!!.bottom
            }
        }

        if (_gravity == CoverFlowGravity.CENTER_VERTICAL) {
            _childTranslateY = (heightSize shr 1) - (_childHeight shr 1)
        } else if (_gravity == CoverFlowGravity.TOP) {
            _childTranslateY = _coverFlowPadding!!.top
        } else if (_gravity == CoverFlowGravity.BOTTOM) {
            _childTranslateY = heightSize - _coverFlowPadding!!.bottom - _childHeight
        }
        _reflectionTranslateY = (_childTranslateY + _childHeight - _childHeight * _reflectHeightFraction).toInt()
        setMeasuredDimension(widthSize, heightSize)
        _visibleChildCount = visibleCount
        _width = widthSize
    }

    private fun getActuallyPosition(position: Int): Int {
        if (_adapter == null) {
            return INVALID_POSITION
        }
        val max = _adapter!!.getCount()
        var nposition = position
        nposition += _visibleImages
        while (nposition < 0 || nposition >= max) {
            if (nposition < 0) {
                nposition += max
            } else if (nposition >= max) {
                nposition -= max
            }
        }
        return nposition
    }

    override fun onLayout(changed: Boolean, left: Int, top: Int, right: Int, bottom: Int) { }

    override fun onDraw(canvas: Canvas?) {
        if (_adapter == null) {
            super.onDraw(canvas)
            return
        }
        canvas?.drawFilter = _drawFilter
        val offset = _offset
        val mid = Math.floor(offset + 0.5).toInt()
        val rightChild = if (_visibleChildCount % 2 == 0) (_visibleChildCount shr 1) - 1 else _visibleChildCount shr 1
        val leftChild = _visibleChildCount shr 1
        val startPos = mid - leftChild
        for (i in startPos..mid - 1) {
            drawChild(canvas, mid, i, i - offset)
        }
        val endPos = mid + rightChild
        for (i in endPos downTo mid) {
            drawChild(canvas, mid, i, i - offset)
        }
        if ((offset - offset.toInt()) == 0.0f) {
            imageOnTop(getActuallyPosition(offset.toInt()))
        }
        super.onDraw(canvas)
        _coverFlowListener?.invalidationCompleted()
    }

    private fun imageOnTop(position: Int) {
        _topImageIndex = position
        val wAndh = _imageRecorder?.get(position)
        val heightInView = (_childHeight - _childHeight * _reflectHeightFraction - _reflectGap).toInt()
        val scale = heightInView * 1.0f / wAndh!![1]
        val widthInView = (wAndh[0] * scale).toInt()
        _touchRect?.left = ((_width shr 1) - (widthInView shr 1)).toFloat()
        _touchRect?.top = _childTranslateY.toFloat()
        _touchRect?.right = _touchRect!!.left + widthInView
        _touchRect?.bottom = _touchRect!!.top + heightInView
        _coverFlowListener?.imageOnTop(this, position, _touchRect!!.left, _touchRect!!.top, _touchRect!!.right, _touchRect!!.bottom)
    }

    protected fun drawChild(canvas: Canvas?, mid: Int, position: Int, offset: Float) {
        val actuallyPosition = getActuallyPosition(position)
        val child = _adapter?.getImage(actuallyPosition)
        val reflection = obtainReflection(child)
        var wAndh = _imageRecorder?.get(actuallyPosition)
        if (wAndh == null) {
            wAndh = intArrayOf(child!!.width, child.height)
            _imageRecorder?.put(actuallyPosition, wAndh)
        } else {
            wAndh[0] = child!!.width
            wAndh[1] = child.height
        }
        if (!child.isRecycled && canvas != null) {
            makeChildTransformer(child, mid, position, offset)
            canvas.drawBitmap(child, _childTransformer, _drawChildPaint)
            if (reflection != null) {
                canvas.drawBitmap(reflection, _reflectionTransformer, _drawChildPaint)
            }
        }
    }

    private fun obtainReflection(src: Bitmap?): Bitmap? {
        if (_reflectHeightFraction <= 0) {
            return null
        }
        var reflection = _recycler!!.getCachedReflectiuon(src)
        if (reflection == null || reflection.isRecycled) {
            _recycler?.removeReflectionCache(src)
            reflection = ImageUtils.createReflectedBitmap(src, _reflectHeightFraction)
            if (reflection != null) {
                _recycler?.buildReflectionCache(src, reflection)
                return reflection
            }
        }
        return reflection
    }

    private fun makeChildTransformer(child: Bitmap?, mid: Int, position: Int, offset: Float) {
        _childTransformer?.reset()
        _reflectionTransformer?.reset()
        var scale: Float
        if (position != mid) {
            scale = 1 - Math.abs(offset) * 0.25f
        } else {
            scale = 1 - Math.abs(offset) * CARD_SCALE
        }
        var translateX: Float
        val originalChildHeight = (_childHeight - _childHeight * _reflectHeightFraction - _reflectGap).toInt()
        val childTotalHeight = (child!!.height + child.height * _reflectHeightFraction + _reflectGap).toInt()
        val originalChildHeightScale = originalChildHeight * 1.0f / child.height
        val childHeightScale = originalChildHeightScale * scale
        val childWidth = (child.width * childHeightScale).toInt()
        val centerChildWidth = (child.width * originalChildHeightScale).toInt()
        val leftSpace = ((_width shr 1) - _coverFlowPadding!!.left)- (centerChildWidth shr 1)
        val rightSpace = ((_width shr 1) - _coverFlowPadding!!.right) - (centerChildWidth shr 1)
        if (offset <= 0) {
            translateX = (leftSpace * 1.0f / _visibleImages) * (_visibleImages + offset) + _coverFlowPadding!!.left
        } else {
            translateX = _width - (rightSpace * 1.0f / _visibleImages) * (_visibleImages - offset) - childWidth - _coverFlowPadding!!.right
        }
        var alpha = (254 - Math.abs(offset) * _standardAlpha).toFloat()
        if (alpha < 0) {
            alpha = 0.0f
        } else if (alpha > 254) {
            alpha = 254.0f
        }
        _drawChildPaint?.alpha = alpha.toInt()
        _childTransformer?.preTranslate(0.0f, -(childTotalHeight shr 1).toFloat())
        _childTransformer?.postScale(childHeightScale, childHeightScale)
        var adjustedChildTranslateY = 0.0f
        if (childHeightScale != 1.0f) {
            adjustedChildTranslateY = ((_childHeight - childTotalHeight) shr 1).toFloat()
        }
        _childTransformer?.postTranslate(translateX, _childTranslateY + adjustedChildTranslateY)
        getCustomTransformMatrix(_childTransformer, _drawChildPaint, child, position, offset)
        _childTransformer?.postTranslate(0.0f, (childTotalHeight shr 1).toFloat())
        _reflectionTransformer?.preTranslate(0.0f, -(childTotalHeight shr 1).toFloat())
        _reflectionTransformer?.postScale(childHeightScale, childHeightScale)
        _reflectionTransformer?.postTranslate(translateX, _reflectionTranslateY * scale + adjustedChildTranslateY)
        getCustomTransformMatrix(_reflectionTransformer, _drawChildPaint, child, position, offset)
        _reflectionTransformer?.postTranslate(0.0f, (childTotalHeight shr 1).toFloat())
    }

    open fun getCustomTransformMatrix(transfromer: Matrix?, drawChildPaint: Paint?, child: Bitmap?, position: Int, offset: Float) { }

    private fun stopLongClick() {
        if (_longClickRunnable != null) {
            removeCallbacks(_longClickRunnable)
            _longClickPosted = false
            _longClickTriggled = false
        }
    }

    private fun triggleLongClick(x: Float, y: Float) {
        if (_touchRect!!.contains(x, y) && _longClickListener != null && _topImageClickEnable && !_longClickPosted) {
            val actuallyPosition = _topImageIndex
            _longClickRunnable?.setPosition(actuallyPosition)
            postDelayed(_longClickRunnable, LONG_CLICK_DELAY.toLong())
        }
    }

    override fun onTouchEvent(event: MotionEvent?): Boolean {
        if (parent != null) {
            parent.requestDisallowInterceptTouchEvent(true)
        }
        val action = event!!.action
        when (action) {
            MotionEvent.ACTION_DOWN ->  {
                if (_scroller!!.computeScrollOffset()) {
                    _scroller?.abortAnimation()
                    invalidate()
                }
                stopLongClick()
                triggleLongClick(event.x, event.y)
                touchBegan(event)
                return true
            }
            MotionEvent.ACTION_MOVE -> {
                touchMoved(event)
                return true
            }
            MotionEvent.ACTION_UP -> {
                touchEnded(event)
                stopLongClick()
                return true
            }
        }
        return false
    }

    private fun touchBegan(event: MotionEvent?) {
        endAnimation()
        val x = event!!.x
        _touchStartX = x
        _touchStartY = event.y
        _startTime = AnimationUtils.currentAnimationTimeMillis()
        _startOffset = _offset
        _touchMoved = false
        _touchStartPos = (x / _width) * MOVE_POS_MULTIPLE - 5
        _touchStartPos /= 2
        _velocity = VelocityTracker.obtain()
        _velocity?.addMovement(event)
    }

    private fun endAnimation() {
        if (_animationRunnable != null) {
            _offset = Math.floor(_offset + 0.5).toFloat()
            invalidate()
            removeCallbacks(_animationRunnable)
            _animationRunnable = null
        }
    }

    private fun touchMoved(event: MotionEvent?) {
        var pos = (event!!.x / _width) * MOVE_POS_MULTIPLE - 5
        pos /= 2
        if (!_touchMoved) {
            val dx = Math.abs(event.x - _touchStartX)
            val dy = Math.abs(event.y - _touchStartY)
            if (dx < TOUCH_MINIMUM_MOVE && dy < TOUCH_MINIMUM_MOVE) {
                return
            }
            _touchMoved = true
            stopLongClick()
        }
        _offset = _startOffset + _touchStartPos - pos
        invalidate()
        _velocity?.addMovement(event)
    }

    private fun startAnimation(speed: Double) {
        if (_animationRunnable != null) {
            return
        }
        var delta = speed * speed / (FRICTION * 2)
        if (speed < 0) {
            delta = -delta
        }
        var nearest = _startOffset + delta
        nearest = Math.floor(nearest + 0.5)
        _startSpeed = Math.sqrt(Math.abs(nearest - _startOffset) * FRICTION * 2).toFloat()
        if (nearest < _startOffset) {
            _startSpeed = -_startSpeed
        }
        _duration = Math.abs(_startSpeed / FRICTION)
        _startTime = AnimationUtils.currentAnimationTimeMillis()
        _animationRunnable = Runnable {
            driveAnimation()
        }
        post(_animationRunnable)
    }

    private fun driveAnimation() {
        val elapsed = (AnimationUtils.currentAnimationTimeMillis() - _startTime) / 1000.0f
        if (elapsed >= _duration)
            endAnimation()
        else {
            updateAnimationAtElapsed(elapsed)
            post(_animationRunnable)
        }
    }

    private fun updateAnimationAtElapsed(elapsed: Float) {
        var nelapsed = elapsed
        if (nelapsed > _duration) {
            nelapsed = _duration
        }
        var delta = Math.abs(_startSpeed) * nelapsed - FRICTION * nelapsed * nelapsed / 2
        if (_startSpeed < 0) {
            delta = -delta
        }
        _offset = _startOffset + delta
        invalidate()
    }

    private fun touchEnded(event: MotionEvent?) {
        var pos = (event!!.x / _width) * MOVE_POS_MULTIPLE - 5
        pos /= 2
        if (_touchMoved || (_offset - Math.floor(_offset.toDouble())) != 0.toDouble()) {
            _startOffset += _touchStartPos - pos
            _offset = _startOffset
            _velocity?.addMovement(event)
            _velocity?.computeCurrentVelocity(1000)
            var speed = _velocity!!.xVelocity
            speed = (speed / _width) * MOVE_SPEED_MULTIPLE
            if (speed > MAX_SPEED) {
                speed = MAX_SPEED
            } else if (speed < -MAX_SPEED) {
                speed = -MAX_SPEED
            }
            startAnimation(-speed.toDouble())
        } else {
            if (_touchRect != null) {
                if (_touchRect!!.contains(event.x, event.y) && _coverFlowListener != null && _topImageClickEnable && !_longClickTriggled) {
                    val actuallyPosition = _topImageIndex
                    _coverFlowListener?.topImageClicked(this, actuallyPosition)
                }
            }
        }
        _velocity?.clear()
        _velocity?.recycle()
    }

    override fun computeScroll() {
        super.computeScroll()
        if (_scroller!!.computeScrollOffset()) {
            val currX = _scroller!!.currX
            _offset = currX * 1.0f / 100
            invalidate()
        }
    }

    inner class RecycleBin {

        val bitmapCache = object: LruCache<Int, Bitmap?>(getCacheSize(context)) {

            override fun sizeOf(key: Int?, value: Bitmap?): Int {
                if (Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB_MR1) {
                    return value!!.rowBytes * value.height
                } else {
                    return value!!.byteCount
                }
            }

            override fun entryRemoved(evicted: Boolean, key: Int?, oldValue: Bitmap?, newValue: Bitmap?) {
                if (evicted && oldValue != null && !oldValue.isRecycled) {
                    oldValue.recycle()
                }
            }
        }

        fun getCachedReflectiuon(origin: Bitmap?): Bitmap? = bitmapCache.get(origin?.hashCode())

        fun buildReflectionCache(origin: Bitmap?, b: Bitmap?) {
            bitmapCache.put(origin?.hashCode(), b)
            Runtime.getRuntime().gc()
        }

        fun removeReflectionCache(origin: Bitmap?): Bitmap? {
            if (origin == null) {
                return null
            }
            return bitmapCache.remove(origin.hashCode())
        }

        fun clear() {
            bitmapCache.evictAll()
        }

        fun getCacheSize(context: Context): Int {
            val am = context.getSystemService(Context.ACTIVITY_SERVICE) as ActivityManager
            val memClass = am.memoryClass
            val cacheSize = 1024 * 1024 * memClass / 21
            return cacheSize
        }
    }

    inner class LongClickRunnable: Runnable {
        var _position = 0

        fun setPosition(position: Int) {
            _position = position
        }

        override fun run() {
            if (_longClickListener != null) {
                _longClickListener?.onLongClick(_position)
                _longClickTriggled = true
            }
        }
    }

    interface TopImageLongClickListener {
        fun onLongClick(position: Int)
    }

    interface CoverFlowListener<V: CoverFlowAdapter> {
        fun imageOnTop(coverFlowView: CoverFlowView<V>?, position: Int, left: Float, top: Float, right: Float, bottom: Float)
        fun topImageClicked(coverFlowView: CoverFlowView<V>?, position: Int)
        fun invalidationCompleted()
    }

    fun setAdapter(adapter: T?) {
        _adapter?.unregisterDataSetObserver(_dataSetObserver)
        _adapter = adapter
        if (_adapter != null) {
            _adapter?.registerDataSetObserver(_dataSetObserver)
            _itemCount = _adapter!!.getCount()
            if (_recycler != null) {
                _recycler?.clear()
            } else {
                _recycler = RecycleBin()
            }
        }
        _offset = 0.0f
        resetCoverFlow()
        requestLayout()
    }

    fun getAdapter(): T? = _adapter

    fun setCoverFlowListener(l: CoverFlowListener<T>?) {
        _coverFlowListener = l
    }

    fun setCoverFlowGravity(gravity: CoverFlowGravity?) {
        _gravity = gravity
    }

    fun setCoverFlowLayoutMode(mode: CoverFlowLayoutMode?) {
        _layoutMode = mode
    }

    fun setReflectionHeight(fraction: Int) {
        var nfraction = fraction
        if (nfraction < 0) {
            nfraction = 0
        } else if (nfraction > 100) {
            nfraction = 100
        }
        _reflectHeightFraction = nfraction.toFloat()
    }

    fun setReflectionGap(gap: Int) {
        var ngap = gap
        if (ngap < 0) {
            ngap = 0
        }
        _reflectGap = ngap
    }

    fun disableTopImageClick() {
        _topImageClickEnable = false
    }

    fun enableTopImageClick() {
        _topImageClickEnable = true
    }

    fun setSelection(position: Int) {
        val max = _adapter!!.getCount()
        if (position < 0 || position >= max) {
            throw IllegalArgumentException("Position want to select can not less than 0 or larger than max of adapter provide!")
        }
        if (_topImageIndex != position) {
            if (_scroller!!.computeScrollOffset()) {
                _scroller?.abortAnimation()
            }
            val from = (_offset * 100).toInt()
            val disX = ((position - _visibleImages) * 100).toInt() - from
            _scroller?.startScroll(from, 0, disX, 0, (DURATION * Math.min(Math.abs(position + max - _topImageIndex), Math.abs(position - _topImageIndex))).toInt())
            invalidate()
        }
    }

    fun setTopImageLongClickListener(listener: TopImageLongClickListener?) {
        _longClickListener = listener
        if (listener == null) {
            _longClickRunnable = null
        } else {
            if (_longClickRunnable == null) {
                _longClickRunnable = LongClickRunnable()
            }
        }
    }

    fun getTopImageIndex(): Int {
        if (_topImageIndex == INVALID_POSITION) {
            return -1
        }
        return _topImageIndex
    }
}