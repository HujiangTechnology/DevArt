package com.hujiang.devart.component.pulltorefresh

import android.content.Context
import android.media.MediaPlayer
import android.view.View

/**
 * Created by rarnu on 3/31/16.
 */
class SoundPullEventListener<T: View>: PullToRefreshBase.OnPullEventListener<T> {

    private var _context: Context? = null
    private var _soundMap: MutableMap<PullToRefreshBase.State, Int>? = null
    private var _currentMediaPlayer: MediaPlayer? = null

    constructor(context: Context) {
        _context = context
        _soundMap = hashMapOf<PullToRefreshBase.State, Int>()
    }

    override fun onPullEvent(refreshView: PullToRefreshBase<T>?, state: PullToRefreshBase.State, direction: PullToRefreshBase.Mode) {
        val soundResId = _soundMap!![state]
        if (soundResId != null) {
            playSound(soundResId)
        }
    }

    private fun playSound(resId: Int) {
        _currentMediaPlayer?.stop()
        _currentMediaPlayer?.release()
        _currentMediaPlayer = MediaPlayer.create(_context, resId)
        _currentMediaPlayer?.start()
    }

    fun addSoundEvent(state: PullToRefreshBase.State, resId: Int) = _soundMap?.put(state, resId)

    fun clearSounds() = _soundMap?.clear()

    fun getCurrentMediaPlayer(): MediaPlayer? = _currentMediaPlayer

}