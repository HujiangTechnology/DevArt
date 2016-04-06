package com.hujiang.devart.component.calendar

import android.content.Context
import android.graphics.Color
import android.graphics.drawable.ColorDrawable
import android.util.AttributeSet
import android.view.GestureDetector
import android.view.Gravity
import android.view.MotionEvent
import android.view.View
import android.view.animation.AnimationUtils
import android.widget.*
import com.hujiang.devart.R
import com.hujiang.devart.utils.UIUtils
import java.util.*

/**
 * Created by rarnu on 4/5/16.
 */
class CalendarView: LinearLayout {

    companion object {
        val GESTURE_HORZ = 0
        val GESTURE_VERT = 1
        private var weekString = arrayOf("SUN", "MON", "TUE", "WED", "TUR", "FRI", "SAT")
    }

    private var _tvWeek = arrayOfNulls<TextView>(7)
    private var _flipper: ViewFlipper? = null
    private var _gestureDetector: GestureDetector? = null
    private var _gestureMethod = 0
    private var _calV: CalendarAdapter? = null
    private var _gridView: GridView? = null
    private var _cYear = 0
    private var _cMonth = 0
    private var _cDay = 0
    private var _jumpMonth = 0
    private var _jumpYear = 0
    private var _itemHeight = UIUtils.dip2px(40)
    private var _currentPosition = -1
    private var _isMondayFirstDay = false
    private var _showLunar = true
    private var _currentDay = 0
    private var _callback: CalendarViewCallback? = null
    private var _adapterCallback: CalendarAdapter.CalendarAdapterCallback? = null


    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        init()
        initCalendar()
    }

    constructor(context: Context, attrs: AttributeSet?): this(context, attrs, 0)

    constructor(context: Context): this(context, null)

    private fun init() {
        val c = Calendar.getInstance()
        _cYear = c.get(Calendar.YEAR)
        _cMonth = c.get(Calendar.MONTH) + 1
        _cDay = c.get(Calendar.DAY_OF_MONTH)
        orientation = VERTICAL
        val layWeek = LinearLayout(context)
        layWeek.orientation = HORIZONTAL
        layWeek.layoutParams = LayoutParams(LayoutParams.MATCH_PARENT, UIUtils.dip2px(20))
        layWeek.setBackgroundColor(resources.getColor(R.color.white))
        val lllp = LayoutParams(0, LayoutParams.MATCH_PARENT)
        lllp.weight = 1.toFloat()
        lllp.gravity = Gravity.CENTER
        for (i in 0..weekString.size - 1) {
            _tvWeek[i] = TextView(context)
            _tvWeek[i]?.layoutParams = lllp
            _tvWeek[i]?.gravity = Gravity.CENTER
            _tvWeek[i]?.textSize = 12.toFloat()
            _tvWeek[i]?.text = weekString[i]
            _tvWeek[i]?.setTextColor(resources.getColor(if (i == (if (_isMondayFirstDay) 5 else 0) || i == 6) android.R.color.holo_blue_light else R.color.black))
            layWeek.addView(_tvWeek[i])
        }
        addView(layWeek)
        val vSplit = View(context)
        vSplit.layoutParams = LayoutParams(LayoutParams.MATCH_PARENT, UIUtils.dip2px(1))
        vSplit.setBackgroundColor(resources.getColor(android.R.color.darker_gray))
        addView(vSplit)
        _flipper = ViewFlipper(context)
        _flipper?.layoutParams = LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT)
        addView(_flipper)
    }

    private fun initCalendar() {
        _gestureDetector = GestureDetector(context, CalendarGestureListener())
        _flipper?.removeAllViews()
        _calV = CalendarAdapter(context, _jumpYear, _jumpMonth, _cYear, _cMonth, _cDay, null)
        _calV?.setItemHeight(_itemHeight)
        _calV?.setMondayFirstDay(_isMondayFirstDay)
        _calV?.setShowLunar(_showLunar)
        addGridView()
        _gridView?.adapter = _calV
        _flipper?.addView(_gridView, 0)
        _callback?.onMonthChanged(this, _calV!!.getShowYear().toInt(), _calV!!.getShowMonth().toInt())
    }

    private fun addGridView() {
        val params = LayoutParams(AbsListView.LayoutParams.MATCH_PARENT, AbsListView.LayoutParams.MATCH_PARENT)
        _gridView = GridView(context)
        _gridView?.numColumns = 7
        _gridView?.gravity = Gravity.CENTER_VERTICAL
        _gridView?.selector = ColorDrawable(Color.TRANSPARENT)
        _gridView?.verticalSpacing = 1
        _gridView?.horizontalSpacing = 1
        _gridView?.setOnTouchListener { v, event -> _gestureDetector!!.onTouchEvent(event) }
        _gridView?.onItemClickListener = AdapterView.OnItemClickListener { parent, view, position, id ->
            _currentPosition = position
            val dayNumber = _calV!!.getItem(position).toString()
            val dv = dayNumber.split(".")[1]
            val scheduleDay = _calV!!.getDateByClickItem(position)!!.split(".")[0]
            val scheduleYear = _calV!!.getShowYear()
            val scheduleMonth = _calV!!.getShowMonth()
            _currentDay = scheduleDay.toInt()
            _callback?.onDayClicked(this@CalendarView, scheduleYear.toInt(), scheduleMonth.toInt(), scheduleDay.toInt(), dv)
            _calV?.setCurrentSelected(_currentPosition)
        }
        _gridView?.layoutParams = params
    }

    private fun enterNextMonth(gvFlag: Int) {
        addGridView()
        _jumpMonth++
        _currentPosition = _calV!!.getCurrentDay()
        _calV = CalendarAdapter(context, _jumpMonth, _jumpYear, _cYear, _cMonth, _cDay, _adapterCallback)
        _calV?.setItemHeight(_itemHeight)
        _calV?.setMondayFirstDay(_isMondayFirstDay)
        if (_currentPosition != -1) {
            _calV?.setCurrentSelected(_currentPosition)
        }
        _calV?.setShowLunar(_showLunar)
        _gridView?.adapter = _calV
        _callback?.onMonthChanged(this, _calV!!.getShowYear().toInt(), _calV!!.getShowMonth().toInt())
        _flipper?.addView(_gridView, gvFlag + 1)
        when (_gestureMethod) {
            GESTURE_HORZ -> {
                _flipper?.inAnimation = AnimationUtils.loadAnimation(context, R.anim.push_left_in)
                _flipper?.outAnimation = AnimationUtils.loadAnimation(context, R.anim.push_left_out)
            }
            GESTURE_VERT -> {
                _flipper?.inAnimation = AnimationUtils.loadAnimation(context, R.anim.push_bottom_in)
                _flipper?.outAnimation = AnimationUtils.loadAnimation(context, R.anim.push_bottom_out)
            }
        }
        _flipper?.showNext()
        _flipper?.removeViewAt(0)
    }

    private fun enterPrevMonth(gvFlag: Int) {
        addGridView()
        _jumpMonth--
        _currentPosition = _calV!!.getCurrentDay()
        _calV = CalendarAdapter(context, _jumpMonth, _jumpYear, _cYear, _cMonth, _cDay, _adapterCallback)
        _calV?.setItemHeight(_itemHeight)
        _calV?.setMondayFirstDay(_isMondayFirstDay)
        if (_currentPosition != -1) {
            _calV?.setCurrentSelected(_currentPosition)
        }
        _calV?.setShowLunar(_showLunar)
        _gridView?.adapter = _calV
        _callback?.onMonthChanged(this, _calV!!.getShowYear().toInt(), _calV!!.getShowMonth().toInt())
        _flipper?.addView(_gridView, gvFlag + 1)
        when (_gestureMethod) {
            GESTURE_HORZ -> {
                _flipper?.inAnimation = AnimationUtils.loadAnimation(context, R.anim.push_right_in)
                _flipper?.outAnimation = AnimationUtils.loadAnimation(context, R.anim.push_right_out)
            }
            GESTURE_VERT -> {
                _flipper?.inAnimation = AnimationUtils.loadAnimation(context, R.anim.push_top_in)
                _flipper?.outAnimation = AnimationUtils.loadAnimation(context, R.anim.push_top_out)
            }
        }
        _flipper?.showPrevious()
        _flipper?.removeViewAt(0)
    }

    fun refreshT() {
        _calV = CalendarAdapter(context, _jumpMonth, _jumpYear, _cYear, _cMonth, _cDay, _adapterCallback)
        _calV?.setItemHeight(_itemHeight)
        _calV?.setMondayFirstDay(_isMondayFirstDay)
        if (_currentPosition != -1) {
            _calV?.setCurrentSelected(_currentPosition)
        }
        _calV?.setShowLunar(_showLunar)
        _gridView?.adapter = _calV
    }

    fun nextMonth() = enterNextMonth(0)

    fun prevMonth() = enterPrevMonth(0)

    fun setDate(jumpYear: Int, jumpMonth: Int, year: Int, month: Int, day: Int, position: Int) {
        _jumpMonth = jumpMonth
        _jumpYear = jumpYear
        _currentPosition = position
        addGridView()
        _calV = CalendarAdapter(context, _jumpMonth, _jumpYear, year, month, day, _adapterCallback)
        _calV?.setItemHeight(_itemHeight)
        _calV?.setMondayFirstDay(_isMondayFirstDay)
        if (_currentPosition != -1) {
            _calV?.setCurrentSelected(_currentPosition)
        }
        _calV?.setShowLunar(_showLunar)
        _gridView?.adapter = _calV
        _callback?.onMonthChanged(this, _calV!!.getShowYear().toInt(), _calV!!.getShowMonth().toInt())
        _flipper?.addView(_gridView, 1)
        _flipper?.displayedChild = 1
        _flipper?.removeViewAt(0)
    }

    fun getCalendarViewHeight(): Int {
        var h = UIUtils.dip2px(20)
        h += UIUtils.dip2px(1)
        h += (_itemHeight + 1) * 6
        return h
    }

    fun setShowLunar(b: Boolean) {
        _showLunar = b
        _calV?.setShowLunar(b)
    }

    fun setGestureMethod(gestureMethod: Int) {
        _gestureMethod = gestureMethod
    }

    fun setCalendarViewCallback(callback: CalendarViewCallback?) {
        _callback = callback
        _callback?.onMonthChanged(this, _calV!!.getShowYear().toInt(), _calV!!.getShowMonth().toInt())
    }

    fun setItemHeight(height: Int) {
        _itemHeight = height
        _calV?.setItemHeight(height)
        _calV?.notifyDataSetChanged()
    }

    fun setMondayFirstDay(b: Boolean) {
        if (_isMondayFirstDay != b) {
            _isMondayFirstDay = b
            _calV?.setMondayFirstDay(_isMondayFirstDay)
            for (i in 0.._tvWeek.size - 1) {
                if (_isMondayFirstDay) {
                    if (i == 6) {
                        _tvWeek[i]?.text = weekString[0]
                    } else {
                        _tvWeek[i]?.text = weekString[i + 1]
                    }
                } else {
                    _tvWeek[i]?.text = weekString[i]
                }
                _tvWeek[i]?.setTextColor(resources.getColor(if (i == (if (_isMondayFirstDay) 5 else 0) || i == 6) android.R.color.holo_blue_light else R.color.black))
            }
        }
    }

    fun setAdapterCallback(callback: CalendarAdapter.CalendarAdapterCallback?) {
        _adapterCallback = callback
        _calV?.setCalendarAdapterCallback(_adapterCallback)
        _calV?.reloadData()
    }

    fun getCurrentPosition(): Int = _calV!!.getCurrentSelected()

    fun setCurrentPosition(pos: Int) {
        _currentPosition = pos
        _calV?.setCurrentSelected(_currentPosition)
    }

    fun getYear(): Int = _calV!!.getShowYear().toInt()

    fun getMonth(): Int = _calV!!.getShowMonth().toInt()

    fun getDay(): Int = _calV!!.getCurrentDay()

    fun getToday(): Int = _calV!!.getToday()

    fun getJumpYear(): Int = _calV!!.getJumpYear()

    fun getJumpMonth(): Int = _calV!!.getJumpMonth()

    inner class CalendarGestureListener: GestureDetector.SimpleOnGestureListener() {

        override fun onDoubleTap(e: MotionEvent?): Boolean {
            _callback?.onCalendarDoubleTap(this@CalendarView)
            return super.onDoubleTap(e)
        }

        override fun onFling(e1: MotionEvent?, e2: MotionEvent?, velocityX: Float, velocityY: Float): Boolean {
            var gvFlag = 0
            when (_gestureMethod) {
                GESTURE_HORZ -> {
                    if (e1!!.x - e2!!.x > 120) {
                        // left
                        enterNextMonth(gvFlag)
                        return true
                    } else if (e1.x - e2.x < -120) {
                        // right
                        enterPrevMonth(gvFlag)
                        return true
                    }
                }
                GESTURE_VERT -> {
                    if (e1!!.y - e2!!.y > 120) {
                        // up
                        enterNextMonth(gvFlag)
                        return true
                    } else if (e1.y - e2.y < -120) {
                        // down
                        enterPrevMonth(gvFlag)
                        return true
                    }
                }
            }
            return false
        }

    }

}