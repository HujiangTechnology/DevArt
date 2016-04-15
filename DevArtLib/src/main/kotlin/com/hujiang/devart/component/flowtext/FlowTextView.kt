package com.hujiang.devart.component.flowtext

import android.content.Context
import android.content.res.Configuration
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Paint
import android.graphics.Typeface
import android.text.Spannable
import android.text.Spanned
import android.text.TextPaint
import android.util.AttributeSet
import android.view.View
import android.widget.RelativeLayout

/**
 * Created by rarnu on 4/15/16.
 */
class FlowTextView: RelativeLayout {

    private val _paintHelper = PaintHelper()
    private val _spanParser = SpanParser(this, _paintHelper)
    private val _clickHandler = ClickHandler(_spanParser)
    private var _color = Color.BLACK
    private var _pageHeight = 0
    private var _textPaint: TextPaint? = null
    private var _linkPaint: TextPaint? = null
    private var _textSize = resources.displayMetrics.scaledDensity * 20.0f
    private var _textColor = Color.BLACK
    private var _typeFace: Typeface? = null
    private var _desiredHeight = 100
    private var _needsMeasure = true
    private val _obstacles = arrayListOf<Obstacle>()
    private var _text: CharSequence? = ""
    private var _isHtml = false
    private var _spacingMult = 0.0f
    private var _spacingAdd = 0.0f
    private var _lineObjects = arrayListOf<HtmlObject>()
    private var _htmlLine = HtmlObject("", 0, 0, 0.0f, null)

    constructor(context: Context, attrs: AttributeSet?, defStyle: Int): super(context, attrs, defStyle) {
        init(context, attrs)
    }

    constructor(context: Context, attrs: AttributeSet?): super(context, attrs) {
        init(context, attrs)
    }

    constructor(context: Context): super(context) {
        init(context, null)
    }

    private fun init(context: Context, attrs: AttributeSet?) {
        if (attrs != null) {
            readAttrs(context, attrs)
        }
        _textPaint = TextPaint(Paint.ANTI_ALIAS_FLAG)
        _textPaint?.density = resources.displayMetrics.density
        _textPaint?.textSize = _textSize
        _textPaint?.color = _textColor
        _linkPaint = TextPaint(Paint.ANTI_ALIAS_FLAG)
        _linkPaint?.density = resources.displayMetrics.density
        _linkPaint?.textSize = _textSize
        _linkPaint?.color = Color.BLUE
        _linkPaint?.isUnderlineText = true
        this.setBackgroundColor(Color.TRANSPARENT)
    }

    private fun readAttrs(context: Context, attrs: AttributeSet?) {
        val attrsArray = intArrayOf(android.R.attr.lineSpacingExtra, android.R.attr.lineSpacingMultiplier, android.R.attr.textSize, android.R.attr.textColor)
        val ta = context.obtainStyledAttributes(attrs, attrsArray)
        _spacingAdd = ta.getDimensionPixelSize(0, 0).toFloat()
        _spacingMult = ta.getFloat(1, 1.0f)
        _textSize = ta.getDimension(2, _textSize)
        _textColor = ta.getColor(3, Color.BLACK)
        ta.recycle()
    }

    fun getTextPaint(): TextPaint? = _textPaint

    fun setTextPaint(textPaint: TextPaint?) {
        _textPaint = textPaint
        invalidate()
    }

    fun getLinkPaint(): TextPaint? = _linkPaint

    fun setLinkPaint(linkPaint: TextPaint?) {
        _linkPaint = linkPaint
        invalidate()
    }

    fun getTextSize(): Float = _textSize

    fun setTextSize(textSize: Float) {
        _textSize = textSize
        _textPaint?.textSize = _textSize
        _linkPaint?.textSize = _textSize
        invalidate()
    }

    fun getColor(): Int = _color

    fun setColor(color: Int) {
        _color = color
        _textPaint?.color = _color
        _paintHelper.setColor(_color)
        invalidate()
    }

    private fun findBoxesAndReturnLowestObstacleYCoord(): Int {
        var lowestYCoord = 0
        var layoutParams: LayoutParams
        for (i in 0..childCount - 1) {
            val child = getChildAt(i)
            if (child.visibility != View.GONE) {
                layoutParams = child.layoutParams as LayoutParams
                val obstacle = Obstacle()
                obstacle.topLeftx = child.left - layoutParams.leftMargin
                val top = child.top
                obstacle.topLefty = child.top
                obstacle.bottomRightx = obstacle.topLeftx + layoutParams.leftMargin + child.width + layoutParams.rightMargin
                obstacle.bottomRighty = obstacle.topLefty + child.height + layoutParams.bottomMargin
                _obstacles.add(obstacle)
                if (obstacle.bottomRighty > lowestYCoord) {
                    lowestYCoord = obstacle.bottomRighty
                }
            }
        }
        return lowestYCoord
    }

    fun getLineHeight(): Int = Math.round(_textPaint!!.getFontMetricsInt(null) * _spacingMult + _spacingAdd)

    fun getChunk(text: String?, maxWidth: Float): Int {
        val length = _textPaint!!.breakText(text, true, maxWidth, null)
        if (length <= 0 || length >= text!!.length || text[length - 1] == ' ') {
            return length
        } else if (text.length > length && text[length] == ' ') {
            return length + 1
        }
        var tempLength = length - 1
        while (text[tempLength] != ' ') {
            tempLength--
            if (tempLength <= 0) {
                return length
            }
        }
        return tempLength + 1
    }

    override fun onDraw(canvas: Canvas?) {
        super.onDraw(canvas)
        val viewWidth = width.toFloat()
        _obstacles.clear()
        val lowestYCoord = findBoxesAndReturnLowestObstacleYCoord()
        val blocks = _text.toString().split("\n")
        var charOffsetStart = 0
        var charOffsetEnd = 0
        var lineIndex = 0
        var xOffset = 0.0f
        var maxWidth = 0.0f
        var yOffset = 0.0f
        var thisLineStr: String
        var chunkSize = 0
        var lineHeight = getLineHeight()
        // var paddingTop = paddingTop
        _lineObjects.clear()
        var spans: Array<Any?>?
        _spanParser.reset()
        for (i in 0..blocks.size - 1){
            var thisBlock = blocks[i]
            if (thisBlock.length <= 0) {
                lineIndex++
                charOffsetEnd += 2
                charOffsetStart = charOffsetEnd
            } else {
                while (thisBlock.length > 0) {
                    lineIndex++
                    yOffset = paddingTop + lineIndex * lineHeight - (getLineHeight() + _textPaint!!.fontMetrics.ascent)
                    val thisLine = CollisionHelper.calculateLineSpaceForGivenYOffset(yOffset, lineHeight, viewWidth, _obstacles)
                    xOffset = thisLine.leftBound;
                    maxWidth = thisLine.rightBound - thisLine.leftBound
                    var actualWidth: Float
                    do {
                        chunkSize = getChunk(thisBlock, maxWidth)
                        val thisCharOffset = charOffsetEnd + chunkSize
                        if (chunkSize > 1) {
                            thisLineStr = thisBlock.substring(0, chunkSize)
                        } else {
                            thisLineStr = ""
                        }
                        _lineObjects.clear()
                        if (_isHtml) {
                            spans = (_text as Spanned).getSpans(charOffsetStart, thisCharOffset, Any::class.java)
                            if (spans.size > 0) {
                                actualWidth = _spanParser.parseSpans(_lineObjects, spans, charOffsetStart, thisCharOffset, xOffset)
                            } else {
                                actualWidth = maxWidth
                            }
                        } else {
                            actualWidth = maxWidth
                        }
                        if (actualWidth > maxWidth) {
                            maxWidth -= 5
                        }
                    } while (actualWidth > maxWidth)
                    charOffsetEnd += chunkSize
                    if (_lineObjects.size <= 0) {
                        _htmlLine.content = thisLineStr
                        _htmlLine.start = 0
                        _htmlLine.end = 0
                        _htmlLine.xOffset = xOffset
                        _htmlLine.paint = _textPaint
                        _lineObjects.add(_htmlLine)
                    }
                    for (thisHtmlObject in _lineObjects) {
                        if (thisHtmlObject is HtmlLink) {
                            val thisLinkWidth = thisHtmlObject.paint!!.measureText(thisHtmlObject.content)
                            _spanParser.addLink(thisHtmlObject, yOffset, thisLinkWidth, lineHeight.toFloat())
                        }
                        paintObject(canvas, thisHtmlObject.content, thisHtmlObject.xOffset, yOffset, thisHtmlObject.paint)
                        if (thisHtmlObject.recycle) {
                            _paintHelper.recyclePaint(thisHtmlObject.paint)
                        }
                    }
                    if (chunkSize >= 1) {
                        thisBlock = thisBlock.substring(chunkSize, thisBlock.length)
                    }
                    charOffsetStart = charOffsetEnd
                }
            }
        }
        yOffset += (lineHeight / 2)
        val child = getChildAt(childCount - 1)
        if (child != null && child.tag != null && child.tag.toString().toLowerCase()== "hideable") {
            if (yOffset > _pageHeight) {
                if (yOffset < _obstacles[_obstacles.size - 1].topLefty - getLineHeight()) {
                    child.visibility = View.GONE
                } else {
                    child.visibility = View.VISIBLE
                }
            } else {
                child.visibility = View.GONE
            }
        }
        _desiredHeight = Math.max(lowestYCoord, yOffset.toInt())
        if (_needsMeasure) {
            _needsMeasure = false
            requestLayout()
        }
    }

    private fun paintObject(canvas: Canvas?, thisLineStr: String?, xOffset: Float, yOffset: Float, paint: Paint?) = canvas?.drawText(thisLineStr, xOffset, yOffset, paint)

    override fun onConfigurationChanged(newConfig: Configuration?) {
        super.onConfigurationChanged(newConfig)
        invalidate()
    }

    override fun invalidate() {
        _needsMeasure = true
        super.invalidate()
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        super.onMeasure(widthMeasureSpec, heightMeasureSpec)
        val widthMode = MeasureSpec.getMode(widthMeasureSpec)
        val heightMode = MeasureSpec.getMode(heightMeasureSpec)
        val widthSize = MeasureSpec.getSize(widthMeasureSpec)
        val heightSize = MeasureSpec.getSize(heightMeasureSpec)
        var width: Int
        var height: Int
        if (widthMode == MeasureSpec.EXACTLY) {
            width = widthSize
        } else {
            width = this.width
        }
        if (heightMode == MeasureSpec.EXACTLY) {
            height = heightSize
        } else {
            height = _desiredHeight
        }
        setMeasuredDimension(width, height + getLineHeight())
    }

    fun getTextColor(): Int = _textColor

    fun setTextColor(color: Int) {
        _textColor = color
        _textPaint?.color = _textColor
        invalidate()
    }

    fun getTypeFace(): Typeface? = _typeFace

    fun setTypeface(type: Typeface?) {
        _typeFace = type
        _textPaint?.typeface = _typeFace
        _linkPaint?.typeface = _typeFace
        invalidate()
    }

    fun getText(): CharSequence? = _text

    fun setText(text: CharSequence?) {
        _text = text
        if (text is Spannable) {
            _isHtml = true
            _spanParser.setSpannable(text)
        } else {
            _isHtml = false
        }
        invalidate()
    }

    fun getOnLinkClickListener(): OnLinkClickListener? = _clickHandler.getOnLinkClickListener()

    fun setOnLinkClickListener(onLinkClickListener: OnLinkClickListener?) = _clickHandler.setOnLinkClickListener(onLinkClickListener)

    fun setPageHeight(pageHeight: Int) {
        _pageHeight = pageHeight
        invalidate()
    }

}