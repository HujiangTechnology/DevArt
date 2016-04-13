package com.hujiang.devart.component.emulator

import android.view.KeyCharacterMap
import android.view.KeyEvent

/**
 * Created by rarnu on 4/10/16.
 */
class TermKeyListener {

    companion object {
        val KEYCODE_UNKNOWN = 0
        val KEYCODE_SOFT_LEFT = 1
        val KEYCODE_SOFT_RIGHT = 2
        val KEYCODE_HOME = 3
        val KEYCODE_BACK = 4
        val KEYCODE_CALL = 5
        val KEYCODE_ENDCALL = 6
        val KEYCODE_0 = 7
        val KEYCODE_1 = 8
        val KEYCODE_2 = 9
        val KEYCODE_3 = 10
        val KEYCODE_4 = 11
        val KEYCODE_5 = 12
        val KEYCODE_6 = 13
        val KEYCODE_7 = 14
        val KEYCODE_8 = 15
        val KEYCODE_9 = 16
        val KEYCODE_STAR = 17
        val KEYCODE_POUND = 18
        val KEYCODE_DPAD_UP = 19
        val KEYCODE_DPAD_DOWN = 20
        val KEYCODE_DPAD_LEFT = 21
        val KEYCODE_DPAD_RIGHT = 22
        val KEYCODE_DPAD_CENTER = 23
        val KEYCODE_VOLUME_UP = 24
        val KEYCODE_VOLUME_DOWN = 25
        val KEYCODE_POWER = 26
        val KEYCODE_CAMERA = 27
        val KEYCODE_CLEAR = 28
        val KEYCODE_A = 29
        val KEYCODE_B = 30
        val KEYCODE_C = 31
        val KEYCODE_D = 32
        val KEYCODE_E = 33
        val KEYCODE_F = 34
        val KEYCODE_G = 35
        val KEYCODE_H = 36
        val KEYCODE_I = 37
        val KEYCODE_J = 38
        val KEYCODE_K = 39
        val KEYCODE_L = 40
        val KEYCODE_M = 41
        val KEYCODE_N = 42
        val KEYCODE_O = 43
        val KEYCODE_P = 44
        val KEYCODE_Q = 45
        val KEYCODE_R = 46
        val KEYCODE_S = 47
        val KEYCODE_T = 48
        val KEYCODE_U = 49
        val KEYCODE_V = 50
        val KEYCODE_W = 51
        val KEYCODE_X = 52
        val KEYCODE_Y = 53
        val KEYCODE_Z = 54
        val KEYCODE_COMMA = 55
        val KEYCODE_PERIOD = 56
        val KEYCODE_ALT_LEFT = 57
        val KEYCODE_ALT_RIGHT = 58
        val KEYCODE_SHIFT_LEFT = 59
        val KEYCODE_SHIFT_RIGHT = 60
        val KEYCODE_TAB = 61
        val KEYCODE_SPACE = 62
        val KEYCODE_SYM = 63
        val KEYCODE_EXPLORER = 64
        val KEYCODE_ENVELOPE = 65
        val KEYCODE_ENTER = 66
        val KEYCODE_DEL = 67
        val KEYCODE_GRAVE = 68
        val KEYCODE_MINUS = 69
        val KEYCODE_EQUALS = 70
        val KEYCODE_LEFT_BRACKET = 71
        val KEYCODE_RIGHT_BRACKET = 72
        val KEYCODE_BACKSLASH = 73
        val KEYCODE_SEMICOLON = 74
        val KEYCODE_APOSTROPHE = 75
        val KEYCODE_SLASH = 76
        val KEYCODE_AT = 77
        val KEYCODE_NUM = 78
        val KEYCODE_HEADSETHOOK = 79
        val KEYCODE_FOCUS = 80
        val KEYCODE_PLUS = 81
        val KEYCODE_MENU = 82
        val KEYCODE_NOTIFICATION = 83
        val KEYCODE_SEARCH = 84
        val KEYCODE_MEDIA_PLAY_PAUSE = 85
        val KEYCODE_MEDIA_STOP = 86
        val KEYCODE_MEDIA_NEXT = 87
        val KEYCODE_MEDIA_PREVIOUS = 88
        val KEYCODE_MEDIA_REWIND = 89
        val KEYCODE_MEDIA_FAST_FORWARD = 90
        val KEYCODE_MUTE = 91
        val KEYCODE_PAGE_UP = 92
        val KEYCODE_PAGE_DOWN = 93
        val KEYCODE_PICTSYMBOLS = 94
        val KEYCODE_SWITCH_CHARSET = 95
        val KEYCODE_BUTTON_A = 96
        val KEYCODE_BUTTON_B = 97
        val KEYCODE_BUTTON_C = 98
        val KEYCODE_BUTTON_X = 99
        val KEYCODE_BUTTON_Y = 100
        val KEYCODE_BUTTON_Z = 101
        val KEYCODE_BUTTON_L1 = 102
        val KEYCODE_BUTTON_R1 = 103
        val KEYCODE_BUTTON_L2 = 104
        val KEYCODE_BUTTON_R2 = 105
        val KEYCODE_BUTTON_THUMBL = 106
        val KEYCODE_BUTTON_THUMBR = 107
        val KEYCODE_BUTTON_START = 108
        val KEYCODE_BUTTON_SELECT = 109
        val KEYCODE_BUTTON_MODE = 110
        val KEYCODE_ESCAPE = 111
        val KEYCODE_FORWARD_DEL = 112
        val KEYCODE_CTRL_LEFT = 113
        val KEYCODE_CTRL_RIGHT = 114
        val KEYCODE_CAPS_LOCK = 115
        val KEYCODE_SCROLL_LOCK = 116
        val KEYCODE_META_LEFT = 117
        val KEYCODE_META_RIGHT = 118
        val KEYCODE_FUNCTION = 119
        val KEYCODE_SYSRQ = 120
        val KEYCODE_BREAK = 121
        val KEYCODE_MOVE_HOME = 122
        val KEYCODE_MOVE_END = 123
        val KEYCODE_INSERT = 124
        val KEYCODE_FORWARD = 125
        val KEYCODE_MEDIA_PLAY = 126
        val KEYCODE_MEDIA_PAUSE = 127
        val KEYCODE_MEDIA_CLOSE = 128
        val KEYCODE_MEDIA_EJECT = 129
        val KEYCODE_MEDIA_RECORD = 130
        val KEYCODE_F1 = 131
        val KEYCODE_F2 = 132
        val KEYCODE_F3 = 133
        val KEYCODE_F4 = 134
        val KEYCODE_F5 = 135
        val KEYCODE_F6 = 136
        val KEYCODE_F7 = 137
        val KEYCODE_F8 = 138
        val KEYCODE_F9 = 139
        val KEYCODE_F10 = 140
        val KEYCODE_F11 = 141
        val KEYCODE_F12 = 142
        val KEYCODE_NUM_LOCK = 143
        val KEYCODE_NUMPAD_0 = 144
        val KEYCODE_NUMPAD_1 = 145
        val KEYCODE_NUMPAD_2 = 146
        val KEYCODE_NUMPAD_3 = 147
        val KEYCODE_NUMPAD_4 = 148
        val KEYCODE_NUMPAD_5 = 149
        val KEYCODE_NUMPAD_6 = 150
        val KEYCODE_NUMPAD_7 = 151
        val KEYCODE_NUMPAD_8 = 152
        val KEYCODE_NUMPAD_9 = 153
        val KEYCODE_NUMPAD_DIVIDE = 154
        val KEYCODE_NUMPAD_MULTIPLY = 155
        val KEYCODE_NUMPAD_SUBTRACT = 156
        val KEYCODE_NUMPAD_ADD = 157
        val KEYCODE_NUMPAD_DOT = 158
        val KEYCODE_NUMPAD_COMMA = 159
        val KEYCODE_NUMPAD_ENTER = 160
        val KEYCODE_NUMPAD_EQUALS = 161
        val KEYCODE_NUMPAD_LEFT_PAREN = 162
        val KEYCODE_NUMPAD_RIGHT_PAREN = 163
        val KEYCODE_VOLUME_MUTE = 164
        val KEYCODE_INFO = 165
        val KEYCODE_CHANNEL_UP = 166
        val KEYCODE_CHANNEL_DOWN = 167
        val KEYCODE_ZOOM_IN = 168
        val KEYCODE_ZOOM_OUT = 169
        val KEYCODE_TV = 170
        val KEYCODE_WINDOW = 171
        val KEYCODE_GUIDE = 172
        val KEYCODE_DVR = 173
        val KEYCODE_BOOKMARK = 174
        val KEYCODE_CAPTIONS = 175
        val KEYCODE_SETTINGS = 176
        val KEYCODE_TV_POWER = 177
        val KEYCODE_TV_INPUT = 178
        val KEYCODE_STB_POWER = 179
        val KEYCODE_STB_INPUT = 180
        val KEYCODE_AVR_POWER = 181
        val KEYCODE_AVR_INPUT = 182
        val KEYCODE_PROG_RED = 183
        val KEYCODE_PROG_GREEN = 184
        val KEYCODE_PROG_YELLOW = 185
        val KEYCODE_PROG_BLUE = 186
        val KEYCODE_OFFSET = 0xA00000

        private val SUPPORT_8_BIT_META = false
        private val META_ALT_ON = 2
        private val META_CTRL_ON = 0x1000
        private val META_CTRL_MASK = 0x7000

        private fun getCursorModeHelper(key: ModifierKey, shift: Int): Int = key.getUIMode() shl shift

        fun isEventFromToggleDevice(event: KeyEvent): Boolean {
            val kcm = KeyCharacterMap.load(event.deviceId)
            return kcm.modifierBehavior == KeyCharacterMap.MODIFIER_BEHAVIOR_CHORDED_OR_TOGGLED
        }
    }



    private var _keyCodes = arrayOfNulls<String>(256)
    private var _appKeyCodes = arrayOfNulls<String>(256)
    private var _altKey = ModifierKey()
    private var _capKey = ModifierKey()
    private var _controlKey = ModifierKey()
    private var _fnKey = ModifierKey()
    private var _cursorMode = 0
    private var _hardwareControlKey = false
    private var _termSession: TermSession? = null
    private var _backKeyCode = 0
    private var _altSendsEsc = false
    private var _combiningAccent = 0

    constructor(termSession: TermSession?) {
        _termSession = termSession
        initKeyCodes()
        updateCursorMode()
    }

    private fun initKeyCodes() {
        _keyCodes[KEYCODE_DPAD_CENTER] = 0xD.toChar().toString()
        _keyCodes[KEYCODE_DPAD_UP] = "${0x1B.toChar()}[A"
        _keyCodes[KEYCODE_DPAD_DOWN] = "${0x1B.toChar()}[B"
        _keyCodes[KEYCODE_DPAD_RIGHT] = "${0x1B.toChar()}[C"
        _keyCodes[KEYCODE_DPAD_LEFT] = "${0x1B.toChar()}[D"
        setFnKeys("vt100")
        _keyCodes[KEYCODE_SYSRQ] = "${0x1B.toChar()}[32~"
        _keyCodes[KEYCODE_BREAK] = "${0x1B.toChar()}[34~"
        _keyCodes[KEYCODE_TAB] = 0x9.toChar().toString()
        _keyCodes[KEYCODE_ENTER] = 0xD.toChar().toString()
        _keyCodes[KEYCODE_ESCAPE] = 0x1B.toChar().toString()
        _keyCodes[KEYCODE_INSERT] = "${0x1B.toChar()}[2~"
        _keyCodes[KEYCODE_FORWARD_DEL] = "${0x1B.toChar()}[3~"
        _keyCodes[KEYCODE_MOVE_HOME] = "${0x1B.toChar()}[1~"
        _keyCodes[KEYCODE_MOVE_END] = "${0x1B.toChar()}[4~"
        _keyCodes[KEYCODE_PAGE_UP] = "${0x1B.toChar()}[5~"
        _keyCodes[KEYCODE_PAGE_DOWN] = "${0x1B.toChar()}[6~"
        _keyCodes[KEYCODE_DEL] = 0x7F.toChar().toString()
        _keyCodes[KEYCODE_NUM_LOCK] = "${0x1B.toChar()}OP"
        _keyCodes[KEYCODE_NUMPAD_DIVIDE] = "/"
        _keyCodes[KEYCODE_NUMPAD_MULTIPLY] = "*"
        _keyCodes[KEYCODE_NUMPAD_SUBTRACT] = "-"
        _keyCodes[KEYCODE_NUMPAD_ADD] = "+"
        _keyCodes[KEYCODE_NUMPAD_ENTER] = 0xD.toChar().toString()
        _keyCodes[KEYCODE_NUMPAD_EQUALS] = "="
        _keyCodes[KEYCODE_NUMPAD_DOT] = "."
        _keyCodes[KEYCODE_NUMPAD_COMMA] = ","
        _keyCodes[KEYCODE_NUMPAD_0] = "0"
        _keyCodes[KEYCODE_NUMPAD_1] = "1"
        _keyCodes[KEYCODE_NUMPAD_2] = "2"
        _keyCodes[KEYCODE_NUMPAD_3] = "3"
        _keyCodes[KEYCODE_NUMPAD_4] = "4"
        _keyCodes[KEYCODE_NUMPAD_5] = "5"
        _keyCodes[KEYCODE_NUMPAD_6] = "6"
        _keyCodes[KEYCODE_NUMPAD_7] = "7"
        _keyCodes[KEYCODE_NUMPAD_8] = "8"
        _keyCodes[KEYCODE_NUMPAD_9] = "9"
        _appKeyCodes[KEYCODE_DPAD_UP] = "${0x1B.toChar()}OA"
        _appKeyCodes[KEYCODE_DPAD_DOWN] = "${0x1B.toChar()}OB"
        _appKeyCodes[KEYCODE_DPAD_RIGHT] = "${0x1B.toChar()}OC"
        _appKeyCodes[KEYCODE_DPAD_LEFT] = "${0x1B.toChar()}OD"
        _appKeyCodes[KEYCODE_NUMPAD_DIVIDE] = "${0x1B.toChar()}Oo"
        _appKeyCodes[KEYCODE_NUMPAD_MULTIPLY] = "${0x1B.toChar()}Oj"
        _appKeyCodes[KEYCODE_NUMPAD_SUBTRACT] = "${0x1B.toChar()}Om"
        _appKeyCodes[KEYCODE_NUMPAD_ADD] = "${0x1B.toChar()}Ok"
        _appKeyCodes[KEYCODE_NUMPAD_ENTER] = "${0x1B.toChar()}OM"
        _appKeyCodes[KEYCODE_NUMPAD_EQUALS] = "${0x1B.toChar()}OX"
        _appKeyCodes[KEYCODE_NUMPAD_DOT] = "${0x1B.toChar()}On"
        _appKeyCodes[KEYCODE_NUMPAD_COMMA] = "${0x1B.toChar()}Ol"
        _appKeyCodes[KEYCODE_NUMPAD_0] = "${0x1B.toChar()}Op"
        _appKeyCodes[KEYCODE_NUMPAD_1] = "${0x1B.toChar()}Oq"
        _appKeyCodes[KEYCODE_NUMPAD_2] = "${0x1B.toChar()}Or"
        _appKeyCodes[KEYCODE_NUMPAD_3] = "${0x1B.toChar()}Os"
        _appKeyCodes[KEYCODE_NUMPAD_4] = "${0x1B.toChar()}Ot"
        _appKeyCodes[KEYCODE_NUMPAD_5] = "${0x1B.toChar()}Ou"
        _appKeyCodes[KEYCODE_NUMPAD_6] = "${0x1B.toChar()}Ov"
        _appKeyCodes[KEYCODE_NUMPAD_7] = "${0x1B.toChar()}Ow"
        _appKeyCodes[KEYCODE_NUMPAD_8] = "${0x1B.toChar()}Ox"
        _appKeyCodes[KEYCODE_NUMPAD_9] = "${0x1B.toChar()}Oy"
    }

    fun setBackKeyCharacter(code: Int) {
        _backKeyCode = code
    }

    fun handleHardwareControlKey(down: Boolean) {
        _hardwareControlKey = down
    }

    fun handleControlKey(down: Boolean) {
        if (down) {
            _controlKey.onPress()
        } else {
            _controlKey.onRelease()
        }
        updateCursorMode()
    }

    fun handleFnKey(down: Boolean) {
        if (down) {
            _fnKey.onPress()
        } else {
            _fnKey.onRelease()
        }
        updateCursorMode()
    }

    fun setTermType(termType: String) = setFnKeys(termType)

    private fun setFnKeys(termType: String) {
        if (termType == "vt100") {
            _keyCodes[KEYCODE_F1] = "${0x1B.toChar()}OP"
            _keyCodes[KEYCODE_F2] = "${0x1B.toChar()}OQ"
            _keyCodes[KEYCODE_F3] = "${0x1B.toChar()}OR"
            _keyCodes[KEYCODE_F4] = "${0x1B.toChar()}OS"
            _keyCodes[KEYCODE_F5] = "${0x1B.toChar()}Ot"
            _keyCodes[KEYCODE_F6] = "${0x1B.toChar()}Ou"
            _keyCodes[KEYCODE_F7] = "${0x1B.toChar()}Ov"
            _keyCodes[KEYCODE_F8] = "${0x1B.toChar()}Ol"
            _keyCodes[KEYCODE_F9] = "${0x1B.toChar()}Ow"
            _keyCodes[KEYCODE_F10] = "${0x1B.toChar()}Ox"
            _keyCodes[KEYCODE_F11] = "${0x1B.toChar()}[23~"
            _keyCodes[KEYCODE_F12] = "${0x1B.toChar()}[24~"
        } else if (termType.startsWith("linux")) {
            _keyCodes[KEYCODE_F1] = "${0x1B.toChar()}[[A"
            _keyCodes[KEYCODE_F2] = "${0x1B.toChar()}[[B"
            _keyCodes[KEYCODE_F3] = "${0x1B.toChar()}[[C"
            _keyCodes[KEYCODE_F4] = "${0x1B.toChar()}[[D"
            _keyCodes[KEYCODE_F5] = "${0x1B.toChar()}[[E"
            _keyCodes[KEYCODE_F6] = "${0x1B.toChar()}[17~"
            _keyCodes[KEYCODE_F7] = "${0x1B.toChar()}[18~"
            _keyCodes[KEYCODE_F8] = "${0x1B.toChar()}[19~"
            _keyCodes[KEYCODE_F9] = "${0x1B.toChar()}[20~"
            _keyCodes[KEYCODE_F10] = "${0x1B.toChar()}[21~"
            _keyCodes[KEYCODE_F11] = "${0x1B.toChar()}[23~"
            _keyCodes[KEYCODE_F12] = "${0x1B.toChar()}[24~"
        } else {
            _keyCodes[KEYCODE_F1] = "${0x1B.toChar()}OP"
            _keyCodes[KEYCODE_F2] = "${0x1B.toChar()}OQ"
            _keyCodes[KEYCODE_F3] = "${0x1B.toChar()}OR"
            _keyCodes[KEYCODE_F4] = "${0x1B.toChar()}OS"
            _keyCodes[KEYCODE_F5] = "${0x1B.toChar()}[15~"
            _keyCodes[KEYCODE_F6] = "${0x1B.toChar()}[17~"
            _keyCodes[KEYCODE_F7] = "${0x1B.toChar()}[18~"
            _keyCodes[KEYCODE_F8] = "${0x1B.toChar()}[19~"
            _keyCodes[KEYCODE_F9] = "${0x1B.toChar()}[20~"
            _keyCodes[KEYCODE_F10] = "${0x1B.toChar()}[21~"
            _keyCodes[KEYCODE_F11] = "${0x1B.toChar()}[23~"
            _keyCodes[KEYCODE_F12] = "${0x1B.toChar()}[24~"
        }
    }

    fun mapControlChar(ch: Int): Int = mapControlChar(_hardwareControlKey || _controlKey.isActive(), _fnKey.isActive(), ch)

    fun mapControlChar(control: Boolean, fn: Boolean, ch: Int): Int {
        var result = ch
        if (control) {
            if (result >= 'a'.toInt() && result <= 'z'.toInt()) {
                result = result - 'a'.toInt() + 0x1
            } else if (result >= 'A'.toInt() && result <= 'Z'.toInt()) {
                result = result - 'A'.toInt() + 0x1
            } else if (result == ' '.toInt() || result == '2'.toInt()) {
                result = 0
            } else if (result == '['.toInt() || result == '3'.toInt()) {
                result = 27
            } else if (result == '\\'.toInt() || result == '4'.toInt()) {
                result = 28
            } else if (result == ']'.toInt() || result == '5'.toInt()) {
                result = 29
            } else if (result == '^'.toInt() || result == '6'.toInt()) {
                result = 30
            } else if (result == '_'.toInt() || result == '7'.toInt()) {
                result = 31
            } else if (result == '8'.toInt()) {
                result = 127
            } else if (result == '9'.toInt()) {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_F11
            } else if (result == '0'.toInt()) {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_F12
            }
        } else if (fn) {
            if (result == 'w'.toInt() || result == 'W'.toInt()) {
                result = KEYCODE_OFFSET + KeyEvent.KEYCODE_DPAD_UP
            } else if (result == 'a'.toInt() || result == 'A'.toInt()) {
                result = KEYCODE_OFFSET + KeyEvent.KEYCODE_DPAD_LEFT
            } else if (result == 's'.toInt() || result == 'S'.toInt()) {
                result = KEYCODE_OFFSET + KeyEvent.KEYCODE_DPAD_DOWN
            } else if (result == 'd'.toInt() || result == 'D'.toInt()) {
                result = KEYCODE_OFFSET + KeyEvent.KEYCODE_DPAD_RIGHT
            } else if (result == 'p'.toInt() || result == 'P'.toInt()) {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_PAGE_UP
            } else if (result == 'n'.toInt() || result == 'N'.toInt()) {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_PAGE_DOWN
            } else if (result == 't'.toInt() || result == 'T'.toInt()) {
                result = KEYCODE_OFFSET + KeyEvent.KEYCODE_TAB
            } else if (result == 'l'.toInt() || result == 'L'.toInt()) {
                result = '|'.toInt()
            } else if (result == 'u'.toInt() || result == 'U'.toInt()) {
                result = '_'.toInt()
            } else if (result == 'e'.toInt() || result == 'E'.toInt()) {
                result = 27
            } else if (result == '.'.toInt()) {
                result = 28
            } else if (result > '0'.toInt() && result <= '9'.toInt()) {
                result = result + KEYCODE_OFFSET + TermKeyListener.KEYCODE_F1 - 1
            } else if (result == '0'.toInt()) {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_F10
            } else if (result == 'i'.toInt() || result == 'I'.toInt()) {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_INSERT
            } else if (result == 'x'.toInt() || result == 'X'.toInt()) {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_FORWARD_DEL
            } else if (result == 'h'.toInt() || result == 'H'.toInt()) {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_MOVE_HOME
            } else if (result == 'f'.toInt() || result == 'F'.toInt()) {
                result = KEYCODE_OFFSET + TermKeyListener.KEYCODE_MOVE_END
            }
        }
        if (result > -1) {
            _altKey.adjustAfterKeypress()
            _capKey.adjustAfterKeypress()
            _controlKey.adjustAfterKeypress()
            _fnKey.adjustAfterKeypress()
            updateCursorMode()
        }

        return result
    }

    fun keyDown(keyCode: Int, event: KeyEvent, appMode: Boolean, allowToggle: Boolean) {
        if (handleKeyCode(keyCode, appMode)) {
            return
        }
        var result = -1
        var chordedCtrl = false
        var setHighBit = false
        when (keyCode) {
            KeyEvent.KEYCODE_ALT_RIGHT, KeyEvent.KEYCODE_ALT_LEFT -> {
                if (allowToggle) {
                    _altKey.onPress()
                    updateCursorMode()
                }
            }
            KeyEvent.KEYCODE_SHIFT_LEFT, KeyEvent.KEYCODE_SHIFT_RIGHT -> {
                if (allowToggle) {
                    _capKey.onPress()
                    updateCursorMode()
                }
            }
            KEYCODE_CTRL_LEFT, KEYCODE_CTRL_RIGHT, KEYCODE_CAPS_LOCK, KEYCODE_FUNCTION -> return
            KeyEvent.KEYCODE_BACK -> result = _backKeyCode
            else -> {
                val metaState = event.metaState
                chordedCtrl = (META_CTRL_ON and metaState) != 0
                val effectiveCaps = allowToggle && (_capKey.isActive())
                var effectiveAlt = allowToggle && _altKey.isActive()
                var effectiveMetaState = metaState and META_CTRL_MASK.inv()
                if (effectiveCaps) {
                    effectiveMetaState = effectiveMetaState or KeyEvent.META_SHIFT_ON
                }
                if (!allowToggle && (effectiveMetaState and META_ALT_ON) != 0) {
                    effectiveAlt = true
                }
                if (effectiveAlt) {
                    if (_altSendsEsc) {
                        _termSession?.write(byteArrayOf(0x1B), 0, 1)
                        effectiveMetaState = effectiveMetaState and KeyEvent.META_ALT_MASK.inv()
                    } else if (SUPPORT_8_BIT_META) {
                        setHighBit = true
                        effectiveMetaState = effectiveMetaState and KeyEvent.META_ALT_MASK.inv()
                    } else {
                        effectiveMetaState = effectiveMetaState or KeyEvent.META_ALT_ON
                    }
                }
                if ((metaState and KeyEvent.META_META_ON) != 0) {
                    if (_altSendsEsc) {
                        _termSession?.write(byteArrayOf(0x1B), 0, 1)
                        effectiveMetaState = effectiveMetaState and KeyEvent.META_META_MASK.inv()
                    } else {
                        if (SUPPORT_8_BIT_META) {
                            setHighBit = true
                            effectiveMetaState = effectiveMetaState and KeyEvent.META_META_MASK.inv()
                        }
                    }
                }
                result = event.getUnicodeChar(effectiveMetaState)
                if ((result and KeyCharacterMap.COMBINING_ACCENT) != 0) {
                    _combiningAccent = result and KeyCharacterMap.COMBINING_ACCENT_MASK
                    return
                }
                if (_combiningAccent != 0) {
                    val unaccentedChar = result
                    result = KeyCharacterMap.getDeadChar(_combiningAccent, unaccentedChar)
                    _combiningAccent = 0
                }
            }
        }
        val effectiveControl = chordedCtrl || _hardwareControlKey || (allowToggle && _controlKey.isActive())
        val effectiveFn = allowToggle && _fnKey.isActive()
        result = mapControlChar(effectiveControl, effectiveFn, result)
        if (result >= KEYCODE_OFFSET) {
            handleKeyCode(result - KEYCODE_OFFSET, appMode)
        } else if (result >= 0) {
            if (setHighBit) {
                result = result or 0x80
            }
            _termSession?.write(result)
        }
    }

    fun getCombiningAccent(): Int = _combiningAccent

    fun getCursorMode(): Int = _cursorMode

    private fun updateCursorMode() {
        _cursorMode = getCursorModeHelper(_capKey, TextRenderer.MODE_SHIFT_SHIFT) or getCursorModeHelper(_altKey, TextRenderer.MODE_ALT_SHIFT) or getCursorModeHelper(_controlKey, TextRenderer.MODE_CTRL_SHIFT) or getCursorModeHelper(_fnKey, TextRenderer.MODE_FN_SHIFT)
    }

    fun handleKeyCode(keyCode: Int, appMode: Boolean): Boolean {
        if (keyCode >= 0 && keyCode < _keyCodes.size) {
            var code: String? = null
            if (appMode) {
                code = _appKeyCodes[keyCode]
            }
            if (code == null) {
                code = _keyCodes[keyCode]
            }
            if (code != null) {
                _termSession?.write(code)
                return true
            }
        }
        return false
    }

    fun keyUp(keyCode: Int, event: KeyEvent) {
        val allowToggle = isEventFromToggleDevice(event)
        when (keyCode) {
            KeyEvent.KEYCODE_ALT_LEFT, KeyEvent.KEYCODE_ALT_RIGHT -> {
                if (allowToggle) {
                    _altKey.onRelease()
                    updateCursorMode()
                }
            }
            KeyEvent.KEYCODE_SHIFT_LEFT, KeyEvent.KEYCODE_SHIFT_RIGHT -> {
                if (allowToggle) {
                    _capKey.onRelease()
                    updateCursorMode()
                }
            }
        }
    }

    fun getAltSendsEsc(): Boolean = _altSendsEsc

    fun setAltSendsEsc(flag: Boolean) {
        _altSendsEsc = flag
    }

    fun isAltActive(): Boolean = _altKey.isActive()

    fun isCtrlActive(): Boolean = _controlKey.isActive()

}