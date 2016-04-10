package com.hujiang.devart.component.miscviews

import android.content.Context
import android.graphics.Canvas
import android.graphics.drawable.Drawable
import android.util.AttributeSet
import android.util.SparseArray
import android.view.View
import android.view.ViewGroup
import android.view.animation.Animation
import android.view.animation.Transformation
import android.widget.AbsListView

/**
 * Created by rarnu on 4/6/16.
 */
class ExpandableListView: android.widget.ExpandableListView {

    private var _animationDuration = 500
    private var _adapter: BaseExpandableListAdapter? = null

    fun getAnimationDuration(): Int = _animationDuration

    fun setAnimationDuration(d: Int) {
        _animationDuration = d
    }

    constructor(context: Context): super(context)

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs)

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle)

    fun setAdapter(adapter: BaseExpandableListAdapter?) {
        super.setAdapter(adapter)
        _adapter = adapter
        _adapter?.setParent(this)
    }

    fun expandGroupWithAnimation(groupPos: Int): Boolean {
        val groupFlatPos = getFlatListPosition(getPackedPositionForGroup(groupPos))
        if (groupFlatPos != -1) {
            val childIndex = groupFlatPos -firstVisiblePosition
            if (childIndex < childCount) {
                val v = getChildAt(childIndex)
                if (v.bottom >= bottom) {
                    _adapter?.notifyGroupExpanded(groupPos)
                    return expandGroup(groupPos)
                }
            }
        }
        _adapter?.startExpandAnimation(groupPos, 0)
        return expandGroup(groupPos)
    }

    fun collapseGroupWithAnimation(groupPos: Int): Boolean {
        val groupFlatPos = getFlatListPosition(getPackedPositionForGroup(groupPos))
        if (groupFlatPos != -1) {
            val childIndex = groupFlatPos - firstVisiblePosition
            if (childIndex >= 0 && childIndex < childCount) {
                val v = getChildAt(childIndex)
                if (v.bottom >= bottom) {
                    return collapseGroup(groupPos)
                }
            } else {
                return collapseGroup(groupPos)
            }
        }
        val packedPos = getExpandableListPosition(firstVisiblePosition)
        var firstChildPos = getPackedPositionChild(packedPos)
        val firstGroupPos = getPackedPositionGroup(packedPos)
        firstChildPos = if (firstChildPos == -1 || firstGroupPos != groupPos) 0 else firstChildPos
        _adapter?.startCollapseAnimation(groupPos, firstChildPos)
        _adapter?.notifyDataSetChanged()
        return isGroupExpanded(groupPos)
    }

    class DummyView: View {
        private var _views = arrayListOf<View>()
        private var _divider: Drawable? = null
        private var _dividerWidth = 0
        private var _dividerHeight = 0

        constructor(context: Context): super(context)

        fun setDivider(divider: Drawable?, dividerWidth: Int, dividerHeight: Int) {
            if (divider != null) {
                _divider = divider
                _dividerWidth = dividerWidth
                _dividerHeight = dividerHeight
                divider.setBounds(0, 0, dividerWidth, dividerHeight)
            }
        }

        fun addFakeView(childView: View?) {
            childView?.layout(0, 0, width, childView.measuredHeight)
            _views.add(childView!!)
        }

        override fun onLayout(changed: Boolean, left: Int, top: Int, right: Int, bottom: Int) {
            super.onLayout(changed, left, top, right, bottom)
            val len = _views.size
            for (i in 0..len - 1) {
                val v = _views[i]
                v.layout(left, top, left + v.measuredWidth, top + v.measuredHeight)
            }
        }

        fun clearViews() = _views.clear()

        override fun dispatchDraw(canvas: Canvas?) {
            canvas?.save()
            _divider?.setBounds(0, 0, _dividerWidth, _dividerHeight)
            val len = _views.size
            for (i in 0..len - 1) {
                val v = _views[i]
                canvas?.save()
                canvas?.clipRect(0, 0, width, v.measuredHeight)
                v.draw(canvas)
                canvas?.restore()
                if (_divider != null) {
                    _divider?.draw(canvas)
                    canvas?.translate(0.0f, _dividerHeight.toFloat())
                }

                canvas?.translate(0.0f, v.measuredHeight.toFloat())
            }
            canvas?.restore()
        }
    }

    class GroupInfo {
        var animating = false
        var expanding = false
        var firstChildPosition = 0
        var dummyHeight = -1
    }

    class ExpandAnimation: Animation {
        private var _baseHeight = 0
        private var _delta = 0
        private var _view: View? = null
        private var _groupInfo: GroupInfo? = null

        constructor(v: View?, startHeight: Int, endHeight: Int, info: GroupInfo?) {
            _baseHeight = startHeight
            _delta = endHeight - startHeight
            _view = v
            _groupInfo = info
            _view?.layoutParams?.height = startHeight
            _view?.requestLayout()
        }

        override fun applyTransformation(interpolatedTime: Float, t: Transformation?) {
            super.applyTransformation(interpolatedTime, t)
            if (interpolatedTime < 1.0f) {
                val value = _baseHeight + (_delta * interpolatedTime).toInt()
                _view?.layoutParams?.height = value
                _groupInfo?.dummyHeight = value
                _view?.requestLayout()
            } else {
                val value = _baseHeight + _delta
                _view?.layoutParams?.height = value
                _groupInfo?.dummyHeight = value
                _view?.requestLayout()
            }
        }

    }

    abstract class BaseExpandableListAdapter: android.widget.BaseExpandableListAdapter() {

        companion object {
            private val STATE_IDLE = 0
            private val STATE_EXPANDING = 1
            private val STATE_COLLAPSING = 2
        }
        private var _groupInfo = SparseArray<GroupInfo>()
        private var _parent: ExpandableListView? = null

        fun setParent(parent: ExpandableListView?) {
            _parent = parent
        }

        open fun getRealChildType(groupPosition: Int, childPosition: Int): Int {
            return 0
        }

        open fun getRealChildTypeCount(): Int {
            return 1
        }

        abstract fun getRealChildView(groupPosition: Int, childPosition: Int, isLastChild: Boolean, convertView: View?, parent: ViewGroup?): View?

        abstract fun getRealChildrenCount(groupPosition: Int): Int

        private fun getGroupInfo(groupPosition: Int): GroupInfo? {
            var info = _groupInfo.get(groupPosition)
            if (info == null) {
                info = GroupInfo()
                _groupInfo.put(groupPosition, info)
            }
            return info
        }

        fun notifyGroupExpanded(groupPosition: Int) {
            val info = getGroupInfo(groupPosition)
            info?.dummyHeight = -1
        }

        fun startExpandAnimation(groupPosition: Int, firstChildPosition: Int) {
            val info = getGroupInfo(groupPosition)
            info?.animating = true
            info?.firstChildPosition = firstChildPosition
            info?.expanding = true
        }

        fun startCollapseAnimation(groupPosition: Int, firstChildPosition: Int) {
            val info = getGroupInfo(groupPosition)
            info?.animating = true
            info?.firstChildPosition = firstChildPosition
            info?.expanding = false
        }

        private fun stopAnimation(groupPosition: Int) {
            val info = getGroupInfo(groupPosition)
            info?.animating = false
        }

        override fun getChildType(groupPosition: Int, childPosition: Int): Int {
            val info = getGroupInfo(groupPosition)
            if (info!!.animating) {
                return 0
            } else {
                return getRealChildType(groupPosition, childPosition) + 1
            }
        }

        override fun getChildTypeCount(): Int = getRealChildTypeCount() + 1

        protected fun generateDefaultLayoutParams(): ViewGroup.LayoutParams? = AbsListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.WRAP_CONTENT, 0)

        override fun getChildView(groupPosition: Int, childPosition: Int, isLastChild: Boolean, convertView: View?, parent: ViewGroup?): View? {
            val info = getGroupInfo(groupPosition)
            if (info!!.animating) {
                var v = convertView
                if (v !is DummyView) {
                    v = DummyView(parent!!.context)
                    v.setLayoutParams(AbsListView.LayoutParams(LayoutParams.MATCH_PARENT, 0))
                }
                if (childPosition < info.firstChildPosition) {
                    v.getLayoutParams().height = 0
                    return v
                }
                val listView = parent as ExpandableListView
                val dummyView = v
                dummyView.clearViews()
                dummyView.setDivider(listView.divider, parent.getMeasuredWidth(), listView.dividerHeight)
                val measureSpecW = MeasureSpec.makeMeasureSpec(parent.getWidth(), MeasureSpec.EXACTLY)
                val measureSpecH = MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED)
                var totalHeight = 0
                var clipHeight = parent.getHeight()
                val len = getRealChildrenCount(groupPosition)
                for (i in info.firstChildPosition..len - 1) {
                    val childView = getRealChildView(groupPosition, i, (i == len - 1), null, parent)
                    var p = childView?.layoutParams as LayoutParams?
                    if (p == null) {
                        p = generateDefaultLayoutParams() as AbsListView.LayoutParams
                        childView?.layoutParams = p
                    }
                    val lpHeight = p.height
                    var childHeightSpec: Int
                    if (lpHeight > 0) {
                        childHeightSpec = MeasureSpec.makeMeasureSpec(lpHeight, MeasureSpec.EXACTLY)
                    } else {
                        childHeightSpec = measureSpecH
                    }
                    childView?.measure(measureSpecW, childHeightSpec)
                    totalHeight += childView!!.measuredHeight
                    if (totalHeight < clipHeight) {
                        dummyView.addFakeView(childView)
                    } else {
                        dummyView.addFakeView(childView)
                        val averageHeight = totalHeight / (i + 1)
                        totalHeight += (len - i - 1) * averageHeight
                        break
                    }
                }
                val o = dummyView.getTag()
                val state = if (o == null) STATE_IDLE else o as Int
                if (info.expanding && state != STATE_EXPANDING) {
                    val ani = ExpandAnimation(dummyView, 0, totalHeight, info)
                    ani.duration = _parent!!.getAnimationDuration().toLong()
                    ani.setAnimationListener(object : Animation.AnimationListener {
                        override fun onAnimationRepeat(animation: Animation?) { }

                        override fun onAnimationEnd(animation: Animation?) {
                            stopAnimation(groupPosition)
                            notifyDataSetChanged()
                            dummyView.setTag(STATE_IDLE)
                        }

                        override fun onAnimationStart(animation: Animation?) { }
                    })
                    dummyView.startAnimation(ani)
                    dummyView.setTag(STATE_EXPANDING)
                } else if (!info.expanding && state != STATE_COLLAPSING) {
                    if (info.dummyHeight == -1) {
                        info.dummyHeight = totalHeight
                    }
                    val ani = ExpandAnimation(dummyView, info.dummyHeight, 0, info)
                    ani.duration = _parent!!.getAnimationDuration().toLong()
                    ani.setAnimationListener(object : Animation.AnimationListener {
                        override fun onAnimationRepeat(animation: Animation?) { }

                        override fun onAnimationEnd(animation: Animation?) {
                            stopAnimation(groupPosition)
                            listView.collapseGroup(groupPosition)
                            notifyDataSetChanged()
                            info.dummyHeight = -1
                            dummyView.setTag(STATE_IDLE)
                        }

                        override fun onAnimationStart(animation: Animation?) { }
                    })
                    dummyView.startAnimation(ani)
                    dummyView.setTag(STATE_COLLAPSING)
                }

                return convertView
            } else {
                return getRealChildView(groupPosition, childPosition, isLastChild, convertView, parent)
            }
        }

        override fun getChildrenCount(groupPosition: Int): Int {
            val info = getGroupInfo(groupPosition)
            if (info!!.animating) {
                return info.firstChildPosition + 1
            } else {
                return getRealChildrenCount(groupPosition)
            }
        }

    }

}