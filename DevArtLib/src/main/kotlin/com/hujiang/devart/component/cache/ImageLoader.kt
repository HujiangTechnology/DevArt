package com.hujiang.devart.component.cache

import android.content.Context
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.os.Handler
import android.widget.ImageView
import java.io.*
import java.net.HttpURLConnection
import java.net.URL
import java.util.*
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

/**
 * Created by rarnu on 3/28/16.
 */
object ImageLoader {

    private var _executorService: ExecutorService? = null
    private val handler = Handler()
    private val imageViews = Collections.synchronizedMap(WeakHashMap<ImageView, String>())

    fun initImageLoader(context: Context) {
        MemoryCache.initMemoryCache()
        FileCache.initFileCache(context)
        _executorService = Executors.newFixedThreadPool(5)

    }

    private fun imageViewReused(photoToLoad: PhotoToLoad?): Boolean {
        val tag = imageViews[photoToLoad!!.imageView]
        return tag == null || tag != photoToLoad.url
    }

    fun clearCache() {
        MemoryCache.clearCache()
        FileCache.clearCache()
    }

    private fun queuePhoto(url: String?, iv: ImageView?) = _executorService?.submit(PhotosLoader(PhotoToLoad(url, iv)))

    private fun getBitmap(url: String): Bitmap? {
        val f = FileCache.getFile(url)
        val b = decodeFile(f)
        if (b != null) {
            return b
        }
        try {
            val imageUrl = URL(url)
            val conn = imageUrl.openConnection() as HttpURLConnection
            conn.connectTimeout = 30000
            conn.readTimeout = 30000
            conn.instanceFollowRedirects = true
            val ins = conn.inputStream
            val ous = FileOutputStream(f)
            copyStream(ins, ous)
            ins.close()
            ous.close()
            val bitmap = decodeFile(f)
            return bitmap
        } catch (t: Throwable) {
            if (t is OutOfMemoryError) {
                MemoryCache.clearCache()
            }
            return null
        }
    }

    private fun decodeFile(f: File?): Bitmap? =
            try {
                val o = BitmapFactory.Options()
                o.inJustDecodeBounds = true
                val fis = FileInputStream(f)
                BitmapFactory.decodeStream(fis, null, o)
                fis.close()
                val REQUIRED_SIZE = 70
                var width_tmp = o.outWidth
                var height_tmp = o.outHeight
                var scale = 1
                while (true) {
                    if (width_tmp / 2 < REQUIRED_SIZE || height_tmp / 2 < REQUIRED_SIZE) {
                        break
                    }
                    width_tmp /= 2
                    height_tmp /= 2
                    scale *= 2
                }
                if (scale >= 2) {
                    scale /= 2
                }
                val o2 = BitmapFactory.Options()
                o2.inSampleSize = scale
                val fisn = FileInputStream(f)
                val bitmap = BitmapFactory.decodeStream(fisn, null, o2)
                fisn.close()
                bitmap
            } catch (e: Exception) {
                null
            }

    private fun copyStream(ins: InputStream?, ous: OutputStream?) = try {
        val bufferSize = 1024
        val bytes = ByteArray(bufferSize)
        var count: Int
        while (true) {
            count = ins!!.read(bytes, 0, bufferSize)
            if (count != -1) {
                ous?.write(bytes, 0, count)
            } else {
                break
            }
        }
    } catch (e: Exception) {
    }


    fun displayImage(url: String?, iv: ImageView?) {
        imageViews.put(iv, url)
        val bitmap = MemoryCache.get(url!!)
        if (bitmap != null) {
            iv?.setImageBitmap(bitmap)
        } else {
            queuePhoto(url, iv)
            iv?.setImageDrawable(null)
        }
    }

    class PhotoToLoad {
        var url: String? = null
        var imageView: ImageView? = null

        constructor(u: String?, iv: ImageView?) {
            url = u
            imageView = iv
        }
    }

    class PhotosLoader : Runnable {

        private var _photoToLoad: PhotoToLoad? = null

        constructor(load: PhotoToLoad?) {
            _photoToLoad = load
        }

        override fun run() {
            try {
                if (imageViewReused(_photoToLoad)) {
                    return
                }
                val bmp = getBitmap(_photoToLoad?.url!!)
                MemoryCache.put(_photoToLoad?.url!!, bmp)
                if (imageViewReused(_photoToLoad)) {
                    return
                }
                val bd = BitmapDisplayer(bmp, _photoToLoad)
                handler.post(bd)
            } catch (t: Throwable) {
            }
        }
    }

    class BitmapDisplayer : Runnable {

        private var _bitmap: Bitmap? = null
        private var _photoToLoad: PhotoToLoad? = null

        constructor(b: Bitmap?, p: PhotoToLoad?) {
            _bitmap = b
            _photoToLoad = p
        }

        override fun run() {
            if (imageViewReused(_photoToLoad)) {
                return
            }
            if (_bitmap != null) {
                _photoToLoad?.imageView?.setImageBitmap(_bitmap)
            } else {
                _photoToLoad?.imageView?.setImageDrawable(null)
            }
        }
    }
}