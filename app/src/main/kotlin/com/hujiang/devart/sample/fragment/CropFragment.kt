package com.hujiang.devart.sample.fragment

import android.os.Bundle
import android.view.Menu
import android.view.View
import android.widget.*
import com.hujiang.devart.base.BaseFragment
import com.hujiang.devart.component.cropper.CropImageView
import com.hujiang.devart.sample.MainActivity
import com.hujiang.devart.sample.R

/**
 * Created by rarnu on 4/26/16.
 */
class CropFragment: BaseFragment(), CompoundButton.OnCheckedChangeListener, SeekBar.OnSeekBarChangeListener, AdapterView.OnItemSelectedListener, View.OnClickListener {


    companion object {
        private val GUIDELINES_ON_TOUCH = 1
    }

    private var _fixedAspectRatioToggleButton: ToggleButton? = null
    private var _aspectRatioXTextView: TextView? = null
    private var _aspectRatioXSeekBar: SeekBar? = null
    private var _aspectRatioYTextView: TextView? = null
    private var _aspectRatioYSeekBar: SeekBar? = null
    private var _guidelinesSpinner: Spinner? = null
    private var _cropImageView: CropImageView? = null
    private var _croppedImageView: ImageView? = null
    private var _cropButton: Button? = null


    override fun getBarTitle(): Int = R.string.crop_name

    override fun getBarTitleWithPath(): Int = R.string.crop_name_with_path

    override fun getCustomTitle(): String? = null

    override fun initComponents() {
        _fixedAspectRatioToggleButton = innerView?.findViewById(R.id.fixedAspectRatioToggle) as ToggleButton
        _aspectRatioXTextView = innerView?.findViewById(R.id.aspectRatioX) as TextView
        _aspectRatioXSeekBar = innerView?.findViewById(R.id.aspectRatioXSeek) as SeekBar
        _aspectRatioYTextView = innerView?.findViewById(R.id.aspectRatioY) as TextView
        _aspectRatioYSeekBar = innerView?.findViewById(R.id.aspectRatioYSeek) as SeekBar
        _guidelinesSpinner = innerView?.findViewById(R.id.showGuidelinesSpin) as Spinner
        _cropImageView = innerView?.findViewById(R.id.cropImageView) as CropImageView
        _croppedImageView = innerView?.findViewById(R.id.croppedImageView) as ImageView
        _cropButton = innerView?.findViewById(R.id.Button_crop) as Button

        _aspectRatioXSeekBar?.isEnabled = false
        _aspectRatioYSeekBar?.isEnabled = false
        _aspectRatioXTextView?.text = _aspectRatioXSeekBar!!.progress.toString()
        _aspectRatioYTextView?.text = _aspectRatioXSeekBar!!.progress.toString()
    }

    override fun initEvents() {
        _fixedAspectRatioToggleButton?.setOnCheckedChangeListener(this)
        _aspectRatioXSeekBar?.setOnSeekBarChangeListener(this)
        _aspectRatioYSeekBar?.setOnSeekBarChangeListener(this)
        _guidelinesSpinner?.onItemSelectedListener = this
        _cropButton?.setOnClickListener(this)
    }

    override fun initLogic() {
        _guidelinesSpinner?.setSelection(GUIDELINES_ON_TOUCH)
    }

    override fun getFragmentLayoutResId(): Int = R.layout.fragment_crop

    override fun getMainActivityName(): String? = MainActivity::class.java.name

    override fun initMenu(menu: Menu?) { }

    override fun onGetNewArguments(bn: Bundle?) { }

    override fun getFragmentState(): Bundle? = null

    override fun onCheckedChanged(buttonView: CompoundButton?, isChecked: Boolean) {
        _cropImageView?.setFixedAspectRatio(isChecked)
        _cropImageView?.setAspectRatio(_aspectRatioXSeekBar!!.progress, _aspectRatioYSeekBar!!.progress)
        _aspectRatioXSeekBar?.isEnabled = isChecked
        _aspectRatioYSeekBar?.isEnabled = isChecked
    }

    override fun onProgressChanged(seekBar: SeekBar?, progress: Int, fromUser: Boolean) {
        when (seekBar!!.id) {
            R.id.aspectRatioXSeek -> {
                if (progress < 1) {
                    _aspectRatioXSeekBar?.setProgress(1)
                }
                _cropImageView?.setAspectRatio(_aspectRatioXSeekBar!!.progress, _aspectRatioYSeekBar!!.progress)
                _aspectRatioXTextView?.text = _aspectRatioXSeekBar!!.progress.toString()
            }
            R.id.aspectRatioYSeek -> {
                if (progress < 1) {
                    _aspectRatioYSeekBar?.setProgress(1)
                }
                _cropImageView?.setAspectRatio(_aspectRatioXSeekBar!!.progress, _aspectRatioYSeekBar!!.progress)
                _aspectRatioYTextView?.text = _aspectRatioYSeekBar!!.progress.toString()
            }
        }

    }

    override fun onStartTrackingTouch(seekBar: SeekBar?) { }

    override fun onStopTrackingTouch(seekBar: SeekBar?) { }

    override fun onItemSelected(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
        _cropImageView?.setGuidelines(position)
    }

    override fun onNothingSelected(parent: AdapterView<*>?) { }

    override fun onClick(v: View?) {
        val croppedImage = _cropImageView?.getCroppedImage()
        _croppedImageView?.setImageBitmap(croppedImage)
    }

}