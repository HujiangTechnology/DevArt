package com.hujiang.devart.component.emulator

/**
 * Created by rarnu on 4/10/16.
 */
class ModifierKey {

    companion object {
        private val UNPRESSED = 0
        private val PRESSED = 1
        private val RELEASED = 2
        private val USED = 3
        private val LOCKED = 4
    }

    private var _state = 0

    constructor() {
        _state = UNPRESSED
    }

    fun onPress() {
        when (_state) {
            PRESSED, USED -> {
            }
            RELEASED -> _state = LOCKED
            LOCKED -> _state = UNPRESSED
            else -> _state = PRESSED
        }
    }

    fun onRelease() {
        when (_state) {
            USED -> _state = UNPRESSED
            PRESSED -> _state = RELEASED
        }
    }

    fun adjustAfterKeypress() {
        when (_state) {
            PRESSED -> _state = USED
            RELEASED -> _state = UNPRESSED
        }
    }

    fun isActive(): Boolean = _state != UNPRESSED

    fun getUIMode(): Int = when (_state) {
        PRESSED, RELEASED, USED -> TextRenderer.MODE_ON
        LOCKED -> TextRenderer.MODE_LOCKED
        else -> TextRenderer.MODE_OFF
    }

}