package com.hujiang.devart.sample.fragment

import android.graphics.Bitmap
import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.Button
import android.widget.ImageView
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R
import com.hujiang.devart.utils.ImageUtils

/**
 * Created by rarnu on 3/29/16.
 */
class ImageFragment: BaseFragment(), View.OnClickListener {

    private var _ivOrigin: ImageView? = null
    private var _ivChanged: ImageView? = null
    private var _btnRotate: Button? = null
    private var _btnFlip: Button? = null
    private var _btnRound: Button? = null
    private var _btnBlackWhite: Button? = null
    private var _btnBlur: Button? = null
    private var _bmpOrigin: Bitmap? = null
    private var _bmpChanged: Bitmap? = null

    override fun getBarTitle(): Int = R.string.image_name

    override fun getBarTitleWithPath(): Int = R.string.image_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _ivOrigin = innerView?.findViewById(R.id.ivOrigin) as ImageView
        _ivChanged = innerView?.findViewById(R.id.ivChanged) as ImageView
        _btnRotate = innerView?.findViewById(R.id.btnRotate) as Button
        _btnFlip = innerView?.findViewById(R.id.btnFlip) as Button
        _btnRound = innerView?.findViewById(R.id.btnRound) as Button
        _btnBlackWhite = innerView?.findViewById(R.id.btnBlackWhite) as Button
        _btnBlur = innerView?.findViewById(R.id.btnBlur) as Button
    }

    override fun initEvents() {
        _btnRotate?.setOnClickListener(this)
        _btnFlip?.setOnClickListener(this)
        _btnRound?.setOnClickListener(this)
        _btnBlackWhite?.setOnClickListener(this)
        _btnBlur?.setOnClickListener(this)
    }

    override fun initLogic() {
        _bmpOrigin = ImageUtils.getBitmapFromAssets(activity, "test.jpg")
        _ivOrigin?.setImageBitmap(_bmpOrigin)
        _ivChanged?.setImageBitmap(_bmpOrigin)
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_image

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.btnRotate -> _bmpChanged = ImageUtils.rotateBmp(_bmpOrigin, 90.0f)
            R.id.btnFlip -> _bmpChanged = ImageUtils.flipBmp(_bmpOrigin, 0)
            R.id.btnRound -> _bmpChanged = ImageUtils.roundedCornerBitmap(_bmpOrigin, 24.0f)
            R.id.btnBlackWhite -> _bmpChanged = ImageUtils.blackWhiteBmp(_bmpOrigin)
            R.id.btnBlur -> _bmpChanged = ImageUtils.blurBmp(_bmpOrigin, 4)
        }
        _ivChanged?.setImageBitmap(_bmpChanged)
    }
}