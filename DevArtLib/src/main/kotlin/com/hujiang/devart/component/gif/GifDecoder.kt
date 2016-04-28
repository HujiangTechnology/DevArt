package com.hujiang.devart.component.gif

import android.content.Context
import android.graphics.Bitmap
import android.os.Environment
import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileOutputStream
import java.io.InputStream

/**
 * Created by rarnu on 3/29/16.
 */
class GifDecoder : Thread {

    companion object {
        val STATUS_PARSING = 0
        val STATUS_FORMAT_ERROR = 1
        val STATUS_OPEN_ERROR = 2
        val STATUS_FINISH = -1
        private val _maxStackSize = 4096
    }

    var width = 0
    var height = 0

    private var _ins: InputStream? = null
    private var _status = 0
    var status: Int = 0
        get() = _status
    private var _gctFlag = false
    private var _gctSize = 0
    private var _loopCount = 1
    var loopCount: Int = 0
        get() = _loopCount
    private var _gct: IntArray? = null
    private var _lct: IntArray? = null
    private var _act: IntArray? = null
    private var _bgIndex = 0
    private var _bgColor = 0
    private var _lastBgColor = 0
    private var _pixelAspect = 0
    private var _lctFlag = false
    private var _interlace = false
    private var _lctSize = 0
    private var _ix = 0
    private var _iy = 0
    private var _iw = 0
    private var _ih = 0
    private var _lrx = 0
    private var _lry = 0
    private var _lrw = 0
    private var _lrh = 0
    private var _image: Bitmap? = null
    private var _lastImage: Bitmap? = null
    private var _currentFrame: GifFrame? = null
    var currentFrame: GifFrame? = null
        get() = _currentFrame
    private var _isShow = false
    private var _block = ByteArray(256)
    private var _blockSize = 0
    private var _dispose = 0
    private var _lastDispose = 0
    private var _transparency = false
    private var _delay = 0
    private var _transIndex = 0
    private var _prefix: ShortArray? = null
    private var _suffix: ByteArray? = null
    private var _pixelStack: ByteArray? = null
    private var _pixels: ByteArray? = null
    private var _gifFrame: GifFrame? = null
    private var _frameCount = 0
    var frameCount: Int = 0
        get() = _frameCount
    private var _action: GifAction? = null
    private var _gifData: ByteArray? = null
    private var _imagePath: String? = null
    private var _cacheImage = false

    constructor(action: GifAction?) {
        _action = action
    }

    fun setGifImage(dat: ByteArray?) {
        _gifData = dat
    }

    fun setGifImage(ins: InputStream?) {
        _ins = ins
    }

    fun setCacheImage(context: Context, cache: Boolean) = try {
        _cacheImage = cache
        if (_cacheImage) {
            var f: Boolean
            if (Environment.getExternalStorageState() == Environment.MEDIA_MOUNTED) {
                _imagePath = Environment.getExternalStorageDirectory().path + File.separator + "gifView_tmp_dir" + File.separator + getDir()
                f = !createDir(_imagePath)
            } else {
                f = true
            }
            if (f) {
                _imagePath = context.filesDir.absolutePath + File.separator + getDir()
                if (!createDir(_imagePath)) {
                    _cacheImage = false
                } else {

                }
            } else {
            }
        } else {
        }
    } catch (e: Exception) {
        _cacheImage = false
    }


    private fun getDir(): String = System.currentTimeMillis().toString()

    private fun createDir(path: String?): Boolean = try {
        val file = File(path)
        if (!file.exists()) {
            file.mkdirs()
        } else {
            true
        }
    } catch (e: Exception) {
        false
    }

    private fun delDir(folderPath: String?, dirDel: Boolean) {
        try {
            delAllFile(folderPath)
            if (dirDel) {
                File(folderPath).delete()
            }
        } catch (e: Exception) {
        }
    }

    private fun delAllFile(path: String?): Boolean {
        var bea = false
        val file = File(path)
        if (!file.exists()) {
            return bea
        }
        if (!file.isDirectory) {
            return bea
        }
        val tempList = file.list()
        var temp: File
        for (i in 0..tempList.size - 1) {
            temp = File(path, tempList[i])
            if (temp.isFile) {
                temp.delete()
            } else if (temp.isDirectory) {
                delAllFile(temp.absolutePath)
                delDir(temp.absolutePath, true)
                bea = true
            }
        }
        return bea
    }

    private fun saveImage(image: Bitmap?, @Suppress("UNUSED_PARAMETER") name: String?) = try {
        val fos = FileOutputStream("$_imagePath/${getDir()}.png")
        image?.compress(Bitmap.CompressFormat.PNG, 100, fos)
    } catch (e: Exception) {

    }

    override fun run() {
        if (_ins != null) {
            readStream()
        } else if (_gifData != null) {
            readByte()
        }
    }

    private fun readByte(): Int {
        _ins = ByteArrayInputStream(_gifData)
        _gifData = null
        return readStream()
    }

    private fun readStream(): Int {
        init()
        if (_ins != null) {
            readHeader()
            if (!err()) {
                readContents()
                if (_frameCount < 0) {
                    _status = STATUS_FORMAT_ERROR
                    _action?.parseOk(false, -1)
                } else {
                    _status = STATUS_FINISH
                    _action?.parseOk(true, -1)
                }
            }
            _ins?.close()
        } else {
            _status = STATUS_OPEN_ERROR
            _action?.parseOk(false, -1)
        }
        return _status
    }

    private fun err(): Boolean = _status != STATUS_PARSING

    private fun init() {
        _status = STATUS_PARSING
        _frameCount = 0
        _gifFrame = null
        _gct = null
        _lct = null
    }

    private fun readHeader() {
        var id = ""
        for (i in 0..5) {
            id += read().toChar()
        }
        if (!id.startsWith("GIF")) {
            _status = STATUS_FORMAT_ERROR
            return
        }
        readLSD()
        if (_gctFlag && !err()) {
            _gct = readColorTable(_gctSize)
            _bgColor = _gct!![_bgIndex]
        }
    }

    private fun read(): Int {
        var curByte = 0
        try {
            curByte = _ins!!.read()
        } catch (e: Exception) {
            _status = STATUS_FORMAT_ERROR
        }
        return curByte
    }

    private fun readLSD() {
        width = readShort()
        height = readShort()
        val packed = read()
        _gctFlag = (packed and 0x80) != 0
        _gctSize = 2 shl (packed and 7)
        _bgIndex = read()
        _pixelAspect = read()
    }

    private fun readColorTable(ncolors: Int): IntArray? {
        val nbytes = 3 * ncolors
        var tab: IntArray? = null
        val c = ByteArray(nbytes)
        var n = 0
        try {
            n = _ins!!.read(c)
        } catch (e: Exception) {

        }
        if (n < nbytes) {
            _status = STATUS_FORMAT_ERROR
        } else {
            tab = IntArray(256)
            var i = 0
            var j = 0
            while (i < ncolors) {
                val r = c[j++].toInt() and 0xff
                val g = c[j++].toInt() and 0xff
                val b = c[j++].toInt() and 0xff
                tab[i++] = 0xff000000.toInt() or (r shl 16) or (g shl 8) or b
            }
        }
        return tab
    }

    private fun readShort(): Int = read() or (read() shl 8)

    private fun readContents() {
        var done = false
        while (!(done || err())) {
            var code = read()
            when (code) {
                0x2C -> readImage()
                0x21 -> {
                    code = read()
                    when (code) {
                        0xf9 -> readGraphicControlExt()
                        0xff -> {
                            readBlock()
                            var app = ""
                            for (i in 0..10) {
                                app += _block[i].toChar()
                            }
                            if (app == "NETSCAPE2.0") {
                                readNetscapeExt()
                            } else {
                                skip()
                            }
                        }
                        else -> skip()
                    }
                }
                0x3b -> done = true
                0x00 -> {
                }
                else -> _status = STATUS_FORMAT_ERROR
            }
        }
    }

    private fun skip() {
        do {
            readBlock()
        } while ((_blockSize > 0) && !err())
    }

    private fun readBlock(): Int {
        _blockSize = read()
        var n = 0
        if (_blockSize > 0) {
            try {
                var count: Int
                while (n < _blockSize) {
                    count = _ins!!.read(_block, n, _blockSize - n)
                    if (count == -1) {
                        break
                    }
                    n += count
                }
            } catch (e: Exception) {
            }
            if (n < _blockSize) {
                _status = STATUS_FORMAT_ERROR
            }
        }
        return n
    }

    private fun readImage() {
        _ix = readShort()
        _iy = readShort()
        _iw = readShort()
        _ih = readShort()
        val packed = read()
        _lctFlag = (packed and 0x80) != 0
        _interlace = (packed and 0x40) != 0
        _lctSize = 2 shl (packed and 7)
        if (_lctFlag) {
            _lct = readColorTable(_lctSize)
            _act = _lct
        } else {
            _act = _gct
            if (_bgIndex == _transIndex) {
                _bgColor = 0
            }
        }
        var save = 0
        if (_transparency) {
            save = _act!![_transIndex]
            _act!![_transIndex] = 0
        }
        if (_act == null) {
            _status = STATUS_FORMAT_ERROR
        }
        if (err()) {
            return
        }
        decodeImageData()
        skip()
        if (err()) {
            return
        }
        _frameCount++
        _image = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_4444)
        setPixels()
        if (_gifFrame == null) {
            if (_cacheImage) {
                val name = getDir()
                _gifFrame = GifFrame("$_imagePath/$name.png", _delay)
                saveImage(_image, name)
            } else {
                _gifFrame = GifFrame(_image, _delay)
            }
            _currentFrame = _gifFrame
        } else {
            var f = _gifFrame
            while (f?.nextFrame != null) {
                f = f?.nextFrame
            }
            if (_cacheImage) {
                val name = getDir()
                f?.nextFrame = GifFrame("$_imagePath/$name.png", _delay)
                saveImage(_image, name)
            } else {
                f?.nextFrame = GifFrame(_image, _delay)
            }
        }

        if (_transparency) {
            _act!![_transIndex] = save
        }
        resetFrame()
        _action?.parseOk(true, _frameCount)
    }

    private fun readGraphicControlExt() {
        read()
        val packed = read()
        _dispose = (packed and 0x1c) shr 2
        if (_dispose == 0) {
            _dispose = 1
        }
        _transparency = (packed and 1) != 0
        _delay = readShort() * 10
        _transIndex = read()
        read()
    }

    private fun readNetscapeExt() {
        do {
            readBlock()
            if (_block[0] == 1.toByte()) {
                val b1 = _block[1].toInt() and 0xff
                val b2 = _block[2].toInt() and 0xff
                _loopCount = (b2 shl 8) or b1
            }
        } while ((_blockSize > 0) && !err())
    }

    private fun decodeImageData() {
        val NullCode = -1
        val npix = _iw * _ih
        if (_pixels == null || _pixels!!.size < npix) {
            _pixels = ByteArray(npix)
        }
        if (_prefix == null) {
            _prefix = ShortArray(_maxStackSize)
        }
        if (_suffix == null) {
            _suffix = ByteArray(_maxStackSize)
        }
        if (_pixelStack == null) {
            _pixelStack = ByteArray(_maxStackSize + 1)
        }
        var dataSize = read()
        var clear = 1 shl dataSize
        var endOfInformation = clear + 1
        var available = clear + 2
        var oldCode = NullCode
        var codeSize = dataSize + 1
        var codeMask = (1 shl codeSize) - 1
        for (code in 0..clear - 1) {
            _prefix!![code] = 0
            _suffix!![code] = code.toByte()
        }
        var datum = 0
        var bits = 0
        var count = 0
        var first = 0
        var top = 0
        var pi = 0
        var bi = 0
        var code: Int
        var inCode: Int
        var i = 0
        while (i < npix) {
            if (top == 0) {
                if (bits < codeSize) {
                    if (count == 0) {
                        count = readBlock()
                        if (count <= 0) {
                            break
                        }
                        bi = 0
                    }
                    datum += (_block[bi].toInt() and 0xff) shl bits
                    bits += 8
                    bi++
                    count--
                    continue
                }
                code = datum and codeMask
                datum = datum shr codeSize
                bits -= codeSize
                if ((code > available) || (code == endOfInformation)) {
                    break
                }
                if (code == clear) {
                    codeSize = dataSize + 1
                    codeMask = (1 shl codeSize) - 1
                    available = clear + 2
                    oldCode = NullCode
                    continue
                }
                if (oldCode == NullCode) {
                    _pixelStack!![top++] = _suffix!![code]
                    oldCode = code
                    first = code
                    continue
                }
                inCode = code
                if (code == available) {
                    _pixelStack!![top++] = first.toByte()
                    code = oldCode
                }
                while (code > clear) {
                    _pixelStack!![top++] = _suffix!![code]
                    code = _prefix!![code].toInt()
                }
                first = _suffix!![code].toInt() and 0xff
                if (available >= _maxStackSize) {
                    break
                }
                _pixelStack!![top++] = first.toByte()
                _prefix!![available] = oldCode.toShort()
                _suffix!![available] = first.toByte()
                available++
                if ((available and codeMask) == 0 && available < _maxStackSize) {
                    codeSize++
                    codeMask += available
                }
                oldCode = inCode
            }
            top--
            _pixels!![pi++] = _pixelStack!![top]
            i++
        }
        for (ii in pi..npix - 1) {
            _pixels!![ii] = 0
        }
    }

    private fun setPixels() {
        val dest = IntArray(width * height)
        if (_lastDispose > 0) {
            if (_lastDispose == 3) {
                val n = _frameCount - 2
                if (n > 0) {
                    _lastImage = getFrameImage(n - 1)
                } else {
                    _lastImage = null
                }
            }
            if (_lastImage != null) {
                _lastImage?.getPixels(dest, 0, width, 0, 0, width, height)
                if (_lastDispose == 2) {
                    var c = 0
                    if (!_transparency) {
                        c = _lastBgColor
                    }
                    for (i in 0.._lrh - 1) {
                        val n1 = (_lry + i) * width + _lrx
                        val n2 = n1 + _lrw
                        for (k in n1..n2 - 1) {
                            dest[k] = c
                        }
                    }
                }
            }
        }
        var pass = 1
        var inc = 8
        var iline = 0
        for (i in 0.._ih - 1) {
            var line = i
            if (_interlace) {
                if (iline >= _ih) {
                    pass++
                    when (pass) {
                        2 -> iline = 4
                        3 -> {
                            iline = 2
                            inc = 4
                        }
                        4 -> {
                            iline = 1
                            inc = 2
                        }
                    }
                }
                line = iline
                iline += inc
            }
            line += _iy
            if (line < height) {
                val k = line * width
                var dx = k + _ix
                var dlim = dx + _iw
                if ((k + width) < dlim) {
                    dlim = k + width
                }
                var sx = i * _iw
                while (dx < dlim) {
                    val index = _pixels!![sx++].toInt() and 0xff
                    val c = _act!![index]
                    if (c != 0) {
                        dest[dx] = c
                    }
                    dx++
                }
            }
        }
        _image = Bitmap.createBitmap(dest, width, height, Bitmap.Config.ARGB_4444)
    }

    private fun resetFrame() {
        _lastDispose = _dispose
        _lrx = _ix
        _lry = _iy
        _lrw = _iw
        _lrh = _ih
        _lastImage = _image
        _lastBgColor = _bgColor
        _dispose = 0
        _transparency = false
        _delay = 0
        _lct = null
    }

    fun getFrameImage(n: Int): Bitmap? {
        val frame = getFrame(n)
        if (frame == null) {
            return null
        } else {
            return frame.image
        }
    }

    fun getFrame(n: Int): GifFrame? {
        var frame = _gifFrame
        var i = 0
        while (frame != null) {
            if (i == n) {
                return frame
            } else {
                frame = frame.nextFrame
            }
            i++
        }
        return null
    }

    fun next(): GifFrame? {
        if (_isShow == false) {
            _isShow = true
            return _gifFrame
        } else {
            if (_currentFrame == null) {
                return null
            }
            if (_status == STATUS_PARSING) {
                if (_currentFrame?.nextFrame != null)
                    _currentFrame = _currentFrame?.nextFrame
            } else {
                _currentFrame = _currentFrame?.nextFrame
                if (_currentFrame == null) {
                    _currentFrame = _gifFrame
                }
            }
            return _currentFrame
        }
    }

    fun reset() {
        _currentFrame = _gifFrame
    }

    fun getDelay(n: Int): Int {
        _delay = -1
        if (n >= 0 && n < _frameCount) {
            val f = getFrame(n)
            if (f != null) {
                _delay = f.delay
            }
        }
        return _delay
    }

    fun getDelays(): IntArray? {
        var f = _gifFrame
        val d = IntArray(_frameCount)
        var i = 0
        while (f != null && i < _frameCount) {
            d[i] = f.delay
            f = f.nextFrame
            i++
        }
        return d
    }

    fun free() {
        var fg = _gifFrame
        if (_cacheImage == false) {
            while (fg != null) {
                if (fg.image != null && !fg.image!!.isRecycled) {
                    fg.image?.recycle()
                }
                fg.image = null
                _gifFrame = _gifFrame?.nextFrame
                fg = _gifFrame
            }
        } else {
            delDir(_imagePath, true)
        }
        _ins?.close()
        _ins = null
        _gifData = null
        _status = 0
        _currentFrame = null
    }

    fun parseOk(): Boolean = _status == STATUS_FINISH

    fun getImage(): Bitmap ? = getFrameImage(0)
}