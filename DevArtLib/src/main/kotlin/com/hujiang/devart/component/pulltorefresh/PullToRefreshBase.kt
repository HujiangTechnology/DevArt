package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.content.res.TypedArray
import android.graphics.Paint
import android.graphics.drawable.Drawable
import android.os.Build
import android.os.Bundle
import android.os.Parcelable
import android.util.AttributeSet
import android.view.*
import android.view.animation.DecelerateInterpolator
import android.view.animation.Interpolator
import android.widget.FrameLayout
import android.widget.LinearLayout
import com.hujiang.devart.R

/**
 * Created by rarnu on 3/30/16.
 */
abstract class PullToRefreshBase<T : View> : LinearLayout, IPullToRefresh<T> {

    companion object {
        val USE_HW_LAYERS = false
        val FRICTION = 2.0f
        val SMOOTH_SCROLL_DURATION_MS = 200L
        val SMOOTH_SCROLL_LONG_DURATION_MS = 325L
        val DEMO_SCROLL_INTERVAL = 225L

        val STATE_STATE = "ptr_state"
        val STATE_MODE = "ptr_mode"
        val STATE_CURRENT_MODE = "ptr_current_mode"
        val STATE_SCROLLING_REFRESHING_ENABLED = "ptr_disable_scrolling"
        val STATE_SHOW_REFRESHING_VIEW = "ptr_show_refreshing_view"
        val STATE_SUPER = "ptr_super"
    }

    private var _touchSlop = 0
    private var _lastMotionX = 0.0f
    private var _lastMotionY = 0.0f
    private var _initialMotionX = 0.0f
    private var _initialMotionY = 0.0f

    private var _isBeingDragged = false
    private var _state = State.RESET
    private var _mode = Mode.getDefault()

    private var _currentMode: Mode = Mode.getDefault()
    protected var _refreshableView: T? = null
    private var _refreshableViewWrapper: FrameLayout? = null

    private var _showViewWhileRefreshing = true
    private var _scrollingWhileRefreshingEnabled = false
    private var _filterTouchEvents = true
    private var _overScrollEnabled = true
    private var _layoutVisibilityChangesEnabled = true

    private var _scrollAnimationInterpolator: Interpolator? = null
    private var _loadingAnimationStyle = AnimationStyle.getDefault()

    private var _headerLayout: LoadingLayout? = null
    private var _footerLayout: LoadingLayout? = null

    private var _onRefreshListener: OnRefreshListener<T>? = null
    private var _onRefreshListener2: OnRefreshListener2<T>? = null
    private var _onPullEventListener: OnPullEventListener<T>? = null

    private var _currentSmoothScrollRunnable: SmoothScrollRunnable? = null

    abstract fun getPullToRefreshScrollDirection(): Orientation
    protected abstract fun createRefreshableView(context: Context, attrs: AttributeSet?): T
    protected abstract fun isReadyForPullEnd(): Boolean
    protected abstract fun isReadyForPullStart(): Boolean

    constructor(context: Context) : super(context) {
        init(context, null)
    }

    constructor(context: Context, attrs: AttributeSet?) : super(context, attrs) {
        init(context, attrs)
    }

    constructor(context: Context, mode: Mode) : super(context) {
        _mode = mode
        init(context, null)
    }

    constructor(context: Context, mode: Mode, animStyle: AnimationStyle) : super(context) {
        _mode = mode
        _loadingAnimationStyle = animStyle
        init(context, null)
    }

    private fun init(context: Context, attrs: AttributeSet?) {
        when (getPullToRefreshScrollDirection()) {
            Orientation.HORIZONTAL -> setOrientation(LinearLayout.HORIZONTAL)
            else -> setOrientation(LinearLayout.VERTICAL)
        }
        setGravity(Gravity.CENTER)
        val config = ViewConfiguration.get(context)
        _touchSlop = config.scaledTouchSlop
        val a = context.obtainStyledAttributes(attrs, R.styleable.PullToRefresh)
        if (a.hasValue(R.styleable.PullToRefresh_ptrMode)) {
            _mode = Mode.mapIntToValue(a.getInteger(R.styleable.PullToRefresh_ptrMode, 0))
        }
        if (a.hasValue(R.styleable.PullToRefresh_ptrAnimationStyle)) {
            _loadingAnimationStyle = AnimationStyle.mapIntToValue(a.getInteger(R.styleable.PullToRefresh_ptrAnimationStyle, 0))
        }
        _refreshableView = createRefreshableView(context, attrs)
        addRefreshableView(context, _refreshableView)
        _headerLayout = createLoadingLayout(context, Mode.PULL_FROM_START, a)
        _footerLayout = createLoadingLayout(context, Mode.PULL_FROM_END, a)
        if (a.hasValue(R.styleable.PullToRefresh_ptrRefreshableViewBackground)) {
            val background = a.getDrawable(R.styleable.PullToRefresh_ptrRefreshableViewBackground)
            if (background != null) {
                _refreshableView?.background = background
            }
        } else if (a.hasValue(R.styleable.PullToRefresh_ptrAdapterViewBackground)) {
            val background = a.getDrawable(R.styleable.PullToRefresh_ptrAdapterViewBackground)
            if (background != null) {
                _refreshableView?.background = background
            }
        }
        if (a.hasValue(R.styleable.PullToRefresh_ptrOverScroll)) {
            _overScrollEnabled = a.getBoolean(R.styleable.PullToRefresh_ptrOverScroll, true)
        }
        if (a.hasValue(R.styleable.PullToRefresh_ptrScrollingWhileRefreshingEnabled)) {
            _scrollingWhileRefreshingEnabled = a.getBoolean(R.styleable.PullToRefresh_ptrScrollingWhileRefreshingEnabled, false)
        }
        handleStyledAttributes(a)
        a.recycle()
        updateUIForMode()
    }

    protected open fun updateUIForMode() {
        val lp = getLoadingLayoutLayoutParams()

        if (this == _headerLayout!!.parent) {
            removeView(_headerLayout)
        }
        if (_mode.showHeaderLoadingLayout()) {
            addViewInternal(_headerLayout, 0, lp)
        }
        if (this == _footerLayout!!.parent) {
            removeView(_footerLayout)
        }
        if (_mode.showFooterLoadingLayout()) {
            addViewInternal(_footerLayout, lp)
        }
        refreshLoadingViewsSize()
        _currentMode = if (_mode != Mode.BOTH) _mode else Mode.PULL_FROM_START
    }

    protected fun refreshLoadingViewsSize() {
        val maximumPullScroll = (getMaximumPullScroll() * 1.2f).toInt()
        var pLeft = paddingLeft
        var pTop = paddingTop
        var pRight = paddingRight
        var pBottom = paddingBottom
        when (getPullToRefreshScrollDirection()) {
            Orientation.HORIZONTAL -> {
                if (_mode.showHeaderLoadingLayout()) {
                    _headerLayout?.width = maximumPullScroll
                    pLeft = -maximumPullScroll
                } else {
                    pLeft = 0
                }
                if (_mode.showFooterLoadingLayout()) {
                    _footerLayout?.width = maximumPullScroll
                    pRight = -maximumPullScroll
                } else {
                    pRight = 0
                }
            }
            Orientation.VERTICAL -> {
                if (_mode.showHeaderLoadingLayout()) {
                    _headerLayout?.height = maximumPullScroll
                    pTop = -maximumPullScroll
                } else {
                    pTop = 0
                }
                if (_mode.showFooterLoadingLayout()) {
                    _footerLayout?.height = maximumPullScroll
                    pBottom = -maximumPullScroll
                } else {
                    pBottom = 0
                }
            }
        }
        setPadding(pLeft, pTop, pRight, pBottom)
    }

    protected open fun handleStyledAttributes(a: TypedArray?) {
    }

    private fun addRefreshableView(context: Context, refreshableView: T?) {
        _refreshableViewWrapper = FrameLayout(context)
        _refreshableViewWrapper?.addView(refreshableView, ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT)
        addViewInternal(_refreshableViewWrapper, LinearLayout.LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT))
    }

    protected fun addViewInternal(child: View?, index: Int, params: ViewGroup.LayoutParams?) = super.addView(child, index, params)

    protected fun addViewInternal(child: View?, params: ViewGroup.LayoutParams?) = super.addView(child, -1, params)

    private fun getLoadingLayoutLayoutParams(): LinearLayout.LayoutParams =
            when (getPullToRefreshScrollDirection()) {
                Orientation.HORIZONTAL -> LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.MATCH_PARENT)
                else -> LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT, LinearLayout.LayoutParams.WRAP_CONTENT)
            }


    protected fun createLoadingLayout(context: Context, mode: Mode, attrs: TypedArray?): LoadingLayout? {
        val layout = _loadingAnimationStyle.createLoadingLayout(context, mode, getPullToRefreshScrollDirection(), attrs)
        layout?.visibility = View.INVISIBLE
        return layout
    }

    fun setHeaderScroll(value: Int) {
        val maximumPullScroll = getMaximumPullScroll()
        val nvalue = Math.min(maximumPullScroll, Math.max(-maximumPullScroll, value))

        if (_layoutVisibilityChangesEnabled) {
            if (value < 0) {
                _headerLayout?.visibility = View.VISIBLE
            } else if (value > 0) {
                _footerLayout?.visibility = View.VISIBLE
            } else {
                _headerLayout?.visibility = View.INVISIBLE
                _footerLayout?.visibility = View.INVISIBLE
            }
        }

        if (USE_HW_LAYERS) {
            _refreshableViewWrapper?.setLayerType(if (value != 0) View.LAYER_TYPE_HARDWARE else View.LAYER_TYPE_NONE, Paint())
        }

        when (getPullToRefreshScrollDirection()) {
            Orientation.VERTICAL -> scrollTo(0, value)
            Orientation.HORIZONTAL -> scrollTo(value, 0)
        }
    }

    private fun getMaximumPullScroll(): Int =
            when (getPullToRefreshScrollDirection()) {
                Orientation.HORIZONTAL -> Math.round(width / FRICTION)
                else -> Math.round(height / FRICTION)
            }


    override fun addView(child: View?, index: Int, params: ViewGroup.LayoutParams?) {
        val refreshableView = getRefreshableView()
        if (refreshableView is ViewGroup) {
            refreshableView.addView(child, index, params)
        } else {
            throw UnsupportedOperationException("Refreshable View is not a ViewGroup so can't addView")
        }
    }

    override fun getCurrentMode(): Mode = _currentMode

    override fun demo(): Boolean {
        if (_mode.showHeaderLoadingLayout() && isReadyForPullStart()) {
            smoothScrollToAndBack(-getHeaderSize() * 2)
            return true
        } else if (_mode.showFooterLoadingLayout() && isReadyForPullEnd()) {
            smoothScrollToAndBack(getFooterSize() * 2)
            return true
        }
        return false
    }

    protected fun getFooterSize(): Int = _footerLayout!!.getContentSize()

    protected fun getHeaderSize(): Int = _headerLayout!!.getContentSize()

    private fun smoothScrollTo(scrollValue: Int, duration: Long) = smoothScrollTo(scrollValue, duration, 0, null)

    private fun smoothScrollTo(newScrollValue: Int, duration: Long, delayMillis: Long, listener: OnSmoothScrollFinishedListener?) {
        if (_currentSmoothScrollRunnable != null) {
            _currentSmoothScrollRunnable?.stop()
        }
        val oldScrollValue = when (getPullToRefreshScrollDirection()) {
            Orientation.HORIZONTAL -> scrollX
            else -> scrollY
        }
        if (oldScrollValue != newScrollValue) {
            if (_scrollAnimationInterpolator == null) {
                _scrollAnimationInterpolator = DecelerateInterpolator()
            }
            _currentSmoothScrollRunnable = SmoothScrollRunnable(oldScrollValue, newScrollValue, duration, listener)
            if (delayMillis > 0) {
                postDelayed(_currentSmoothScrollRunnable, delayMillis)
            } else {
                post(_currentSmoothScrollRunnable)
            }
        }
    }

    private fun smoothScrollToAndBack(y: Int) =
            smoothScrollTo(y, SMOOTH_SCROLL_DURATION_MS, 0, object : OnSmoothScrollFinishedListener {
                override fun onSmoothScrollFinished() {
                    smoothScrollTo(0, SMOOTH_SCROLL_DURATION_MS, DEMO_SCROLL_INTERVAL, null)
                }
            })


    override fun getFilterTouchEvents(): Boolean = _filterTouchEvents

    override fun getLoadingLayoutProxy(): ILoadingLayout? = getLoadingLayoutProxy(true, true)

    override fun getLoadingLayoutProxy(includeStart: Boolean, includeEnd: Boolean): ILoadingLayout? = createLoadingLayoutProxy(includeStart, includeEnd)

    override fun getMode(): Mode = _mode

    override fun getRefreshableView(): T? = _refreshableView

    override fun getShowViewWhileRefreshing(): Boolean = _showViewWhileRefreshing

    override fun getState(): State = _state

    fun isDisableScrollingWhileRefreshing(): Boolean = !isScrollingWhileRefreshingEnabled()

    override fun isPullToRefreshEnabled(): Boolean = _mode.permitsPullToRefresh()

    override fun isPullToRefreshOverScrollEnabled(): Boolean =
            Build.VERSION.SDK_INT >= Build.VERSION_CODES.GINGERBREAD && _overScrollEnabled && OverscrollHelper.isAndroidOverScrollEnabled(_refreshableView)

    fun setState(state: State, vararg params: Boolean) {
        _state = state
        when (_state) {
            State.RESET -> onReset()
            State.PULL_TO_REFRESH -> onPullToRefresh()
            State.RELEASE_TO_REFRESH -> onReleaseToRefresh()
            State.REFRESHING,
            State.MANUAL_REFRESHING -> onRefreshing(params[0])
            State.OVERSCROLLING -> {
            }
        }
        _onPullEventListener?.onPullEvent(this, _state, _currentMode)
    }

    protected open fun onReset() {
        _isBeingDragged = false
        _layoutVisibilityChangesEnabled = true
        _headerLayout?.reset()
        _footerLayout?.reset()
        smoothScrollTo(0)
    }

    protected open fun onPullToRefresh() {
        when (_currentMode) {
            Mode.PULL_FROM_END -> _footerLayout?.pullToRefresh()
            Mode.PULL_FROM_START -> _headerLayout?.pullToRefresh()
        }
    }

    protected open fun onReleaseToRefresh() {
        when (_currentMode) {
            Mode.PULL_FROM_END -> _footerLayout?.releaseToRefresh()
            Mode.PULL_FROM_START -> _headerLayout?.releaseToRefresh()
            else -> {
            }
        }
    }

    protected open fun onRefreshing(doScroll: Boolean) {
        if (_mode.showHeaderLoadingLayout()) {
            _headerLayout?.refreshing()
        }
        if (_mode.showFooterLoadingLayout()) {
            _footerLayout?.refreshing()
        }
        if (doScroll) {
            if (_showViewWhileRefreshing) {
                val listener = object : OnSmoothScrollFinishedListener {
                    override fun onSmoothScrollFinished() {
                        callRefreshListener()
                    }
                }
                when (_currentMode) {
                    Mode.MANUAL_REFRESH_ONLY,
                    Mode.PULL_FROM_END -> smoothScrollTo(getFooterSize(), listener)
                    else -> smoothScrollTo(-getHeaderSize(), listener)
                }
            } else {
                smoothScrollTo(0)
            }
        } else {
            callRefreshListener()
        }
    }

    private fun callRefreshListener() {
        if (_onRefreshListener != null) {
            _onRefreshListener?.onRefresh(this)
        } else if (_onRefreshListener2 != null) {
            if (_currentMode == Mode.PULL_FROM_START) {
                _onRefreshListener2?.onPullDownToRefresh(this)
            } else if (_currentMode == Mode.PULL_FROM_END) {
                _onRefreshListener2?.onPullUpToRefresh(this)
            }
        }
    }

    protected fun smoothScrollTo(scrollValue: Int) {
        smoothScrollTo(scrollValue, getPullToRefreshScrollDuration())
    }

    protected fun smoothScrollTo(scrollValue: Int, listener: OnSmoothScrollFinishedListener?) {
        smoothScrollTo(scrollValue, getPullToRefreshScrollDuration(), 0, listener)
    }

    protected fun smoothScrollToLonger(scrollValue: Int) {
        smoothScrollTo(scrollValue, getPullToRefreshScrollDurationLonger())
    }

    protected fun getPullToRefreshScrollDuration(): Long = SMOOTH_SCROLL_DURATION_MS

    protected fun getPullToRefreshScrollDurationLonger(): Long = SMOOTH_SCROLL_LONG_DURATION_MS

    protected open fun createLoadingLayoutProxy(includeStart: Boolean, includeEnd: Boolean): LoadingLayoutProxy? {
        val proxy = LoadingLayoutProxy()
        if (includeStart && _mode.showHeaderLoadingLayout()) {
            proxy.addLayout(_headerLayout)
        }
        if (includeEnd && _mode.showFooterLoadingLayout()) {
            proxy.addLayout(_footerLayout)
        }
        return proxy
    }

    private fun isReadyForPull(): Boolean =
            when (_mode) {
                Mode.PULL_FROM_START -> isReadyForPullStart()
                Mode.PULL_FROM_END -> isReadyForPullEnd()
                Mode.BOTH -> isReadyForPullEnd() || isReadyForPullStart()
                else -> {
                    false
                }
            }

    override fun onInterceptTouchEvent(event: MotionEvent?): Boolean {
        if (!isPullToRefreshEnabled()) {
            return false
        }
        val action = event!!.action
        if (action == MotionEvent.ACTION_CANCEL || action == MotionEvent.ACTION_UP) {
            _isBeingDragged = false
            return false
        }
        if (action != MotionEvent.ACTION_DOWN && _isBeingDragged) {
            return true
        }
        when (action) {
            MotionEvent.ACTION_MOVE -> {
                if (!_scrollingWhileRefreshingEnabled && isRefreshing()) {
                    return true
                }
                if (isReadyForPull()) {
                    val y = event.y
                    val x = event.x
                    var diff: Float
                    var oppositeDiff: Float
                    var absDiff: Float
                    when (getPullToRefreshScrollDirection()) {
                        Orientation.HORIZONTAL -> {
                            diff = x - _lastMotionX
                            oppositeDiff = y - _lastMotionY
                        }
                        else -> {
                            diff = y - _lastMotionY
                            oppositeDiff = x - _lastMotionX
                        }
                    }
                    absDiff = Math.abs(diff)
                    if (absDiff > _touchSlop && (!_filterTouchEvents || absDiff > Math.abs(oppositeDiff))) {
                        if (_mode.showHeaderLoadingLayout() && diff >= 1f && isReadyForPullStart()) {
                            _lastMotionY = y
                            _lastMotionX = x
                            _isBeingDragged = true
                            if (_mode == Mode.BOTH) {
                                _currentMode = Mode.PULL_FROM_START
                            }
                        } else if (_mode.showFooterLoadingLayout() && diff <= -1f && isReadyForPullEnd()) {
                            _lastMotionY = y
                            _lastMotionX = x
                            _isBeingDragged = true
                            if (_mode == Mode.BOTH) {
                                _currentMode = Mode.PULL_FROM_END
                            }
                        }
                    }
                }
            }
            MotionEvent.ACTION_DOWN -> {
                if (isReadyForPull()) {
                    _lastMotionY = event.y
                    _initialMotionY = _lastMotionY
                    _lastMotionX = event.x
                    _initialMotionX = _lastMotionX
                    _isBeingDragged = false
                }
            }
        }
        return _isBeingDragged
    }

    override fun onTouchEvent(event: MotionEvent?): Boolean {
        if (!isPullToRefreshEnabled()) {
            return false
        }
        if (!_scrollingWhileRefreshingEnabled && isRefreshing()) {
            return true
        }
        if (event!!.action == MotionEvent.ACTION_DOWN && event.edgeFlags != 0) {
            return false
        }
        when (event.action) {
            MotionEvent.ACTION_MOVE -> {
                if (_isBeingDragged) {
                    _lastMotionY = event.y
                    _lastMotionX = event.x
                    pullEvent()
                    return true
                }
            }
            MotionEvent.ACTION_DOWN -> {
                if (isReadyForPull()) {
                    _lastMotionY = event.y
                    _initialMotionY = _lastMotionY
                    _lastMotionX = event.x
                    _initialMotionX = _lastMotionX
                    return true
                }
            }
            MotionEvent.ACTION_CANCEL,
            MotionEvent.ACTION_UP -> {
                if (_isBeingDragged) {
                    _isBeingDragged = false
                    if (_state == State.RELEASE_TO_REFRESH && (_onRefreshListener != null || _onRefreshListener2 != null)) {
                        setState(State.REFRESHING, true)
                        return true
                    }
                    if (isRefreshing()) {
                        smoothScrollTo(0)
                        return true
                    }
                    setState(State.RESET)
                    return true
                }
            }
        }
        return false
    }

    private fun pullEvent() {
        var newScrollValue: Int
        var itemDimension: Int
        var initialMotionValue: Float
        var lastMotionValue: Float

        when (getPullToRefreshScrollDirection()) {
            Orientation.HORIZONTAL -> {
                initialMotionValue = _initialMotionX
                lastMotionValue = _lastMotionX
            }
            else -> {
                initialMotionValue = _initialMotionY
                lastMotionValue = _lastMotionY
            }
        }
        when (_currentMode) {
            Mode.PULL_FROM_END -> {
                newScrollValue = Math.round(Math.max(initialMotionValue - lastMotionValue, 0.0f) / FRICTION).toInt()
                itemDimension = getFooterSize()
            }
            else -> {
                newScrollValue = Math.round(Math.min(initialMotionValue - lastMotionValue, 0.0f) / FRICTION).toInt()
                itemDimension = getHeaderSize()
            }
        }
        setHeaderScroll(newScrollValue)
        if (newScrollValue != 0 && !isRefreshing()) {
            val scale = Math.abs(newScrollValue) * 1.0f / itemDimension
            when (_currentMode) {
                Mode.PULL_FROM_END -> _footerLayout?.onPull(scale)
                else -> _headerLayout?.onPull(scale)
            }
            if (_state != State.PULL_TO_REFRESH && itemDimension >= Math.abs(newScrollValue)) {
                setState(State.PULL_TO_REFRESH)
            } else if (_state == State.PULL_TO_REFRESH && itemDimension < Math.abs(newScrollValue)) {
                setState(State.RELEASE_TO_REFRESH)
            }
        }
    }

    override fun isRefreshing(): Boolean = _state == State.REFRESHING || _state == State.MANUAL_REFRESHING

    override fun isScrollingWhileRefreshingEnabled(): Boolean = _scrollingWhileRefreshingEnabled

    override fun onRefreshComplete() {
        if (isRefreshing()) {
            setState(State.RESET)
        }
    }

    override fun setScrollingWhileRefreshingEnabled(scrollingWhileRefreshingEnabled: Boolean) {
        _scrollingWhileRefreshingEnabled = scrollingWhileRefreshingEnabled
    }

    fun setDisableScrollingWhileRefreshing(disableScrollingWhileRefreshing: Boolean) =
            setScrollingWhileRefreshingEnabled(!disableScrollingWhileRefreshing)

    override fun setFilterTouchEvents(filterEvents: Boolean) {
        _filterTouchEvents = filterEvents
    }

    fun setLastUpdatedLabel(label: CharSequence?) = getLoadingLayoutProxy()?.setLastUpdatedLabel(label)

    fun setLoadingDrawable(drawable: Drawable?) = getLoadingLayoutProxy()?.setLoadingDrawable(drawable)

    fun setLoadingDrawable(drawable: Drawable?, mode: Mode) =
            getLoadingLayoutProxy(mode.showHeaderLoadingLayout(), mode.showFooterLoadingLayout())?.setLoadingDrawable(drawable)

    override fun setLongClickable(longClickable: Boolean) {
        getRefreshableView()?.isLongClickable = longClickable
    }

    override fun setMode(mode: Mode) {
        if (mode != _mode) {
            _mode = mode
            updateUIForMode()
        }
    }

    override fun onSizeChanged(w: Int, h: Int, oldw: Int, oldh: Int) {
        super.onSizeChanged(w, h, oldw, oldh)
        refreshLoadingViewsSize()
        refreshRefreshableViewSize(w, h)
        post({ requestLayout() })
    }

    protected fun refreshRefreshableViewSize(width: Int, height: Int) {
        val lp = _refreshableViewWrapper?.layoutParams as LinearLayout.LayoutParams
        when (getPullToRefreshScrollDirection()) {
            Orientation.HORIZONTAL -> {
                if (lp.width != width) {
                    lp.width = width
                    _refreshableViewWrapper?.requestLayout()
                }
            }
            Orientation.VERTICAL -> {
                if (lp.height != height) {
                    lp.height = height
                    _refreshableViewWrapper?.requestLayout()
                }
            }
        }
    }

    override fun onSaveInstanceState(): Parcelable? {
        val bundle = Bundle()
        onPtrSaveInstanceState(bundle)
        bundle.putInt(STATE_STATE, _state.intValue)
        bundle.putInt(STATE_MODE, _mode.intValue)
        bundle.putInt(STATE_CURRENT_MODE, _currentMode.intValue)
        bundle.putBoolean(STATE_SCROLLING_REFRESHING_ENABLED, _scrollingWhileRefreshingEnabled)
        bundle.putBoolean(STATE_SHOW_REFRESHING_VIEW, _showViewWhileRefreshing)
        bundle.putParcelable(STATE_SUPER, super.onSaveInstanceState())
        return bundle
    }

    override fun onRestoreInstanceState(state: Parcelable?) {
        if (state is Bundle) {
            val bundle = state
            setMode(Mode.mapIntToValue(bundle.getInt(STATE_MODE, 0)))
            _currentMode = Mode.mapIntToValue(bundle.getInt(STATE_CURRENT_MODE, 0))
            _scrollingWhileRefreshingEnabled = bundle.getBoolean(STATE_SCROLLING_REFRESHING_ENABLED, false)
            _showViewWhileRefreshing = bundle.getBoolean(STATE_SHOW_REFRESHING_VIEW, true)
            super.onRestoreInstanceState(bundle.getParcelable(STATE_SUPER))
            val viewState = State.mapIntToValue(bundle.getInt(STATE_STATE, 0))
            if (viewState == State.REFRESHING || viewState == State.MANUAL_REFRESHING) {
                setState(viewState, true)
            }
            onPtrRestoreInstanceState(bundle)
            return
        }
        super.onRestoreInstanceState(state)
    }

    protected open fun onPtrRestoreInstanceState(savedInstanceState: Bundle?) { }

    protected open fun onPtrSaveInstanceState(saveState: Bundle?) { }

    override fun setOnPullEventListener(listener: OnPullEventListener<T>?) {
        _onPullEventListener = listener
    }

    override fun setOnRefreshListener(listener: OnRefreshListener<T>?) {
        _onRefreshListener = listener
        _onRefreshListener2 = null
    }

    override fun setOnRefreshListener(listener: OnRefreshListener2<T>?) {
        _onRefreshListener2 = listener
        _onRefreshListener = null
    }

    override fun setPullToRefreshOverScrollEnabled(enabled: Boolean) {
        _overScrollEnabled = enabled
    }

    override fun setRefreshing() {
        setRefreshing(true)
    }

    override fun setRefreshing(doScroll: Boolean) {
        if (!isRefreshing()) {
            setState(State.MANUAL_REFRESHING, doScroll)
        }
    }

    override fun setShowViewWhileRefreshing(showView: Boolean) {
        _showViewWhileRefreshing = showView
    }

    override fun setScrollAnimationInterpolator(interpolator: Interpolator?) {
        _scrollAnimationInterpolator = interpolator
    }

    fun setPullLabel(pullLabel: CharSequence?) = getLoadingLayoutProxy()?.setPullLabel(pullLabel)

    fun setPullLabel(pullLabel: CharSequence?, mode: Mode) =
            getLoadingLayoutProxy(mode.showHeaderLoadingLayout(), mode.showFooterLoadingLayout())?.setPullLabel(pullLabel)

    fun setPullToRefreshEnabled(enable: Boolean) = setMode(if (enable) Mode.getDefault() else Mode.DISABLED)

    fun setRefreshingLabel(refreshingLabel: CharSequence?) = getLoadingLayoutProxy()?.setRefreshingLabel(refreshingLabel)

    fun setRefreshingLabel(refreshingLabel: CharSequence?, mode: Mode) =
            getLoadingLayoutProxy(mode.showHeaderLoadingLayout(), mode.showFooterLoadingLayout())?.setRefreshingLabel(refreshingLabel)

    fun setReleaseLabel(releaseLabel: CharSequence?) = setReleaseLabel(releaseLabel, Mode.BOTH)

    fun setReleaseLabel(releaseLabel: CharSequence?, mode: Mode) =
            getLoadingLayoutProxy(mode.showHeaderLoadingLayout(), mode.showFooterLoadingLayout())?.setReleaseLabel(releaseLabel)

    protected fun disableLoadingLayoutVisibilityChanges() {
        _layoutVisibilityChangesEnabled = false
    }

    protected fun getFooterLayout(): LoadingLayout? = _footerLayout

    protected fun getHeaderLayout(): LoadingLayout? = _headerLayout

    protected fun getRefreshableViewWrapper(): FrameLayout? = _refreshableViewWrapper

    enum class Mode {
        DISABLED(0x0), PULL_FROM_START(0x1), PULL_FROM_END(0x2), BOTH(0x3), MANUAL_REFRESH_ONLY(0x4);

        companion object {
            var PULL_DOWN_TO_REFRESH = Mode.PULL_FROM_START
            var PULL_UP_TO_REFRESH = Mode.PULL_FROM_END
            fun mapIntToValue(modeInt: Int): Mode {
                for (value in Mode.values()) {
                    if (modeInt == value.intValue) {
                        return value
                    }
                }
                return getDefault()
            }

            fun getDefault(): Mode = PULL_FROM_START
        }

        private var _intValue: Int
        var intValue: Int = 0
            get() = _intValue

        constructor(modeInt: Int) {
            _intValue = modeInt
        }

        fun permitsPullToRefresh(): Boolean = !(this == DISABLED || this == MANUAL_REFRESH_ONLY)

        fun showHeaderLoadingLayout(): Boolean = this == PULL_FROM_START || this == BOTH

        fun showFooterLoadingLayout(): Boolean = this == PULL_FROM_END || this == BOTH || this == MANUAL_REFRESH_ONLY

    }

    enum class State {
        RESET(0x0), PULL_TO_REFRESH(0x1), RELEASE_TO_REFRESH(0x2), REFRESHING(0x8), MANUAL_REFRESHING(0x9), OVERSCROLLING(0x10);

        companion object {
            fun mapIntToValue(stateInt: Int): State {
                for (value in State.values()) {
                    if (stateInt == value.intValue) {
                        return value
                    }
                }
                return RESET
            }
        }

        private var _intValue: Int
        var intValue: Int = 0
            get() = _intValue

        constructor(intValue: Int) {
            _intValue = intValue
        }
    }

    inner class SmoothScrollRunnable : Runnable {

        private var _interpolator: Interpolator? = null
        private var _scrollToY = 0
        private var _scrollFromY = 0
        private var _duration = 0L
        private var _listener: OnSmoothScrollFinishedListener? = null
        private var _continueRunning = true
        private var _startTime = -1L
        private var _currentY = -1

        constructor(fromY: Int, toY: Int, duration: Long, listener: OnSmoothScrollFinishedListener?) {
            _scrollFromY = fromY
            _scrollToY = toY
            _interpolator = _scrollAnimationInterpolator
            _duration = duration
            _listener = listener
        }

        override fun run() {
            if (_startTime == -1L) {
                _startTime = System.currentTimeMillis()
            } else {
                var normalizedTime = (1000 * (System.currentTimeMillis() - _startTime)) / _duration
                normalizedTime = Math.max(Math.min(normalizedTime, 1000), 0)

                val deltaY = Math.round((_scrollFromY - _scrollToY) * _interpolator!!.getInterpolation(normalizedTime / 1000.0f))
                _currentY = _scrollFromY - deltaY
                setHeaderScroll(_currentY)
            }

            if (_continueRunning && _scrollToY != _currentY) {
                this@PullToRefreshBase.postOnAnimation(this)
            } else {
                _listener?.onSmoothScrollFinished()
            }
        }

        fun stop() {
            _continueRunning = false
            removeCallbacks(this)
        }
    }

    enum class AnimationStyle {
        ROTATE, FLIP;

        companion object {
            fun getDefault(): AnimationStyle = ROTATE
            fun mapIntToValue(modeInt: Int): AnimationStyle =
                    when (modeInt) {
                        0x1 -> FLIP
                        else -> ROTATE
                    }
        }

        fun createLoadingLayout(context: Context, mode: Mode, scrollDirection: Orientation, attrs: TypedArray?): LoadingLayout? =
                when (this) {
                    FLIP -> FlipLoadingLayout(context, mode, scrollDirection, attrs)
                    else -> RotateLoadingLayout(context, mode, scrollDirection, attrs)
                }
    }

    interface OnSmoothScrollFinishedListener {
        fun onSmoothScrollFinished()
    }

    interface OnLastItemVisibleListener {
        fun onLastItemVisible()
    }

    interface OnPullEventListener<T : View> {
        fun onPullEvent(refreshView: PullToRefreshBase<T>?, state: State, direction: Mode)
    }

    interface OnRefreshListener<T : View> {
        fun onRefresh(refreshView: PullToRefreshBase<T>?)
    }

    interface OnRefreshListener2<T : View> {
        fun onPullDownToRefresh(refreshView: PullToRefreshBase<T>?)
        fun onPullUpToRefresh(refreshView: PullToRefreshBase<T>?)
    }

    enum class Orientation { VERTICAL, HORIZONTAL; }

}