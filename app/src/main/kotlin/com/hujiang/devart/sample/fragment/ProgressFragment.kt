package com.hujiang.devart.sample.fragment

import android.content.Context
import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.Button
import android.widget.SeekBar
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.progress.WheelProgressBar
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 4/15/16.
 */
class ProgressFragment: BaseFragment(), SeekBar.OnSeekBarChangeListener, View.OnClickListener {

    companion object {
        private fun styleRandom(pb: WheelProgressBar?, ctx: Context) {
            pb?.setRimShader(null)
            pb?.setRimColor(0xFFFFFFFF.toInt())
            pb?.setCircleColor(0x00000000.toInt())
            pb?.setBarColor(0xFF000000.toInt())
            pb?.setContourColor(0xFFFFFFFF.toInt())
            pb?.setBarWidth(pxFromDp(ctx, 1.0f))
            pb?.setBarLength(pxFromDp(ctx, 100.0f))
            pb?.setSpinSpeed(4.0f)
            pb?.setDelayMillis(3)
        }

        fun pxFromDp(context: Context, dp: Float): Int = (dp * context.resources.displayMetrics.density).toInt()
    }

    private var _pb1: WheelProgressBar? = null
    private var _wasSpinning = false
    private var _sbAmount: SeekBar? = null
    private var _btnSpin: Button? = null
    private var _btnIncrement: Button? = null
    private var _btnRandom: Button? = null
    private var _isRunning = false
    private var _cachedProgress = 0

    override fun getBarTitle(): Int = R.string.progress_name

    override fun getBarTitleWithPath(): Int = R.string.progress_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _pb1 = innerView?.findViewById(R.id.pb1) as WheelProgressBar
        _sbAmount = innerView?.findViewById(R.id.sbAmount) as SeekBar
        _btnSpin = innerView?.findViewById(R.id.btnSpin) as Button
        _btnIncrement = innerView?.findViewById(R.id.btnIncrement) as Button
        _btnRandom = innerView?.findViewById(R.id.btnRandom) as Button
    }

    override fun initEvents() {
        _sbAmount?.setOnSeekBarChangeListener(this)
        _btnSpin?.setOnClickListener(this)
        _btnIncrement?.setOnClickListener(this)
        _btnRandom?.setOnClickListener(this)
    }

    override fun initLogic() { }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_progress

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onProgressChanged(seekBar: SeekBar?, progress: Int, fromUser: Boolean) {
        val nprogress = 360.0 * (seekBar!!.progress / 100.0)
        _pb1?.setProgress(nprogress.toInt())
    }

    override fun onStartTrackingTouch(seekBar: SeekBar?) { }

    override fun onStopTrackingTouch(seekBar: SeekBar?) { }

    override fun onClick(v: View?) {
        when (v!!.id) {
            R.id.btnSpin -> {
                _isRunning = !_isRunning
                if(_isRunning) {
                    _cachedProgress = _pb1!!.getProgress()
                    _pb1?.resetCount()
                    _pb1?.setText("Spinning...")
                    _pb1?.startSpinning()
                    _btnSpin?.text = "Stop spinning"
                } else {
                    _btnSpin?.text = "Start spinning"
                    _pb1?.setText("")
                    _pb1?.stopSpinning()
                    _pb1?.setProgress(_cachedProgress)
                }
                _sbAmount?.isEnabled = !_isRunning
                _btnIncrement?.isEnabled = !_isRunning
            }
            R.id.btnIncrement -> _pb1?.incrementProgress(36)
            R.id.btnRandom -> styleRandom(_pb1, activity)
        }
    }


    override fun onPause() {
        super.onPause()
        _wasSpinning = _pb1!!.isSpinning()
        if(_wasSpinning) {
            _pb1?.stopSpinning()
        }
    }

    override fun onResume() {
        super.onResume()
        if(_wasSpinning) {
            _pb1?.startSpinning()
        }
        _wasSpinning = false
    }

}