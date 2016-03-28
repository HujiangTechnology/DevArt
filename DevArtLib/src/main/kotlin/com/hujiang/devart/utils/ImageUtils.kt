package com.hujiang.devart.utils

import android.content.ContentUris
import android.content.Context
import android.graphics.*
import android.graphics.drawable.BitmapDrawable
import android.graphics.drawable.Drawable
import android.net.Uri
import android.os.Build
import android.os.Environment
import android.provider.DocumentsContract
import android.provider.MediaStore
import java.io.BufferedInputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

/**
 * Created by rarnu on 3/27/16.
 */
object ImageUtils {

    fun getBitmapFromAssets(context: Context, path: String): Bitmap? {
        val ins = context.resources.assets.open(path)
        val bitmap = BitmapFactory.decodeStream(ins)
        ins.close()
        return bitmap
    }

    fun roundedCornerBitmap(bitmap: Bitmap?, radis: Float): Bitmap? {
        val output = Bitmap.createBitmap(bitmap!!.width, bitmap.height, Bitmap.Config.ARGB_8888)
        val canvas = Canvas(output)
        val paint = Paint()
        val rect = Rect(0, 0, bitmap.width, bitmap.height)
        val rectF = RectF(rect)
        val roundPx = radis
        paint.isAntiAlias = true
        canvas.drawARGB(0, 0, 0, 0)
        paint.color = Color.WHITE
        canvas.drawRoundRect(rectF, roundPx, roundPx, paint)
        paint.xfermode = PorterDuffXfermode(PorterDuff.Mode.SRC_IN)
        canvas.drawBitmap(bitmap, rect, rect, paint)
        return output
    }

    fun blackWhiteBmp(bmp: Bitmap?): Bitmap? {
        val width = bmp!!.width
        val height = bmp.height
        val matrix = ColorMatrix()
        val src = floatArrayOf(
                0.308f, 0.609f, 0.082f, 0.0f, 0.0f,
                0.308f, 0.609f, 0.082f, 0.0f, 0.0f,
                0.308f, 0.609f, 0.082f, 0.0f, 0.0f,
                0.000f, 0.000f, 0.000f, 1.0f, 0.0f)
        matrix.set(src)
        val filter = ColorMatrixColorFilter(matrix)
        val p = Paint()
        p.colorFilter = filter
        val colorBmp = Bitmap.createBitmap(width, height, bmp.config)
        val c = Canvas(colorBmp)
        c.drawBitmap(bmp, 0.0f, 0.0f, p)
        return colorBmp
    }

    fun rotateBmp(bmp: Bitmap?, angle: Float): Bitmap? {
        val width = bmp!!.width
        val height = bmp.height
        val matrix = Matrix()
        matrix.postRotate(angle)
        return Bitmap.createBitmap(bmp, 0, 0, width, height, matrix, true)
    }

    fun flipBmp(bmp: Bitmap?, mode: Int): Bitmap? {
        val width = bmp!!.width
        val height = bmp.height
        val matrix = Matrix()
        val temp = Matrix()
        val mirrorY = floatArrayOf(-1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f)
        temp.setValues(mirrorY)
        matrix.postConcat(temp)
        if (mode == 2) {
            matrix.setRotate(180.0f, width * 1.0f / 2, height * 1.0f / 2)
        }
        return Bitmap.createBitmap(bmp, 0, 0, width, height, matrix, true)
    }

    fun matrixBmp(bmp: Bitmap?, margin: Int): Bitmap? {
        val width = bmp!!.width
        val height = bmp.height
        val px = UIUtils.dip2px(margin)
        val matrix = Matrix()
        if (width > (UIUtils.width!! - px)) {
            val scale = ((UIUtils.width!! - px) * 1.0f / width)
            matrix.postScale(scale, scale)
        } else {
            matrix.postScale(1.0f, 1.0f)
        }
        return Bitmap.createBitmap(bmp, 0, 0, width, height, matrix, true)
    }

    fun colorMatrixBmp(bmp: Bitmap?, matrixSrc: FloatArray?): Bitmap? {
        val width = bmp!!.width
        val height = bmp.height
        val matrix = ColorMatrix()
        matrix.set(matrixSrc)
        val filter = ColorMatrixColorFilter(matrix)
        val p = Paint()
        p.colorFilter = filter
        val colorBmp = Bitmap.createBitmap(width, height, bmp.config)
        val c = Canvas(colorBmp)
        c.drawBitmap(bmp, 0.0f, 0.0f, p)
        return colorBmp
    }

    fun zoomImage(bmp: Bitmap?, newWidth: Float, newHeight: Float): Bitmap? {
        val width = bmp!!.width
        val height = bmp.height
        val matrix = Matrix()
        val scaleWidth = newWidth * 1.0f / width
        val scaleHeight = newHeight * 1.0f / height
        matrix.postScale(scaleWidth, scaleHeight)
        val bitmap = Bitmap.createBitmap(bmp, 0, 0, width, height, matrix, true)
        return bitmap
    }

    fun blurBmp(bmp: Bitmap?, blur: Int): Bitmap? {
        val pixels = IntArray(bmp!!.width * bmp.height)
        val pixelsRawSource = IntArray(bmp.width * bmp.height * 3)
        val pixelsRawNew = IntArray(bmp.width * bmp.height * 3)
        bmp.getPixels(pixels, 0, bmp.width, 0, 0, bmp.width, bmp.height)
        for (k in 1..blur) {
            for (i in 0..pixels.size - 1) {
                pixelsRawSource[i * 3 + 0] = Color.red(pixels[i])
                pixelsRawSource[i * 3 + 1] = Color.green(pixels[i])
                pixelsRawSource[i * 3 + 2] = Color.blue(pixels[i])
            }
            var currentPixel = bmp.width * 3 + 3
            for (i in 0..bmp.height - 3 - 1) {
                for (j in 0..bmp.width * 3 - 1) {
                    currentPixel += 1
                    var sumColor: Int
                    sumColor = pixelsRawSource[currentPixel - bmp.width * 3]
                    sumColor += pixelsRawSource[currentPixel - 3]
                    sumColor += pixelsRawSource[currentPixel + 3]
                    sumColor += pixelsRawSource[currentPixel + bmp.width * 3]
                    pixelsRawNew[currentPixel] = Math.round(sumColor * 1.0f / 4)
                }
            }
            for (i in 0..pixels.size - 1) {
                pixels[i] = Color.rgb(pixelsRawNew[i * 3 + 0], pixelsRawNew[i * 3 + 1], pixelsRawNew[i * 3 + 2])
            }
        }
        val bmpReturn = Bitmap.createBitmap(bmp.width, bmp.height, Bitmap.Config.ARGB_8888)
        bmpReturn.setPixels(pixels, 0, bmp.width, 0, 0, bmp.width, bmp.height)
        return bmpReturn
    }

    fun drawableToBitmap(drawable: Drawable?): Bitmap? {
        val w = drawable!!.intrinsicWidth
        val h = drawable.intrinsicHeight
        val config = if (drawable.opacity != PixelFormat.OPAQUE) {
            Bitmap.Config.ARGB_8888
        } else {
            Bitmap.Config.RGB_565
        }
        val bitmap = Bitmap.createBitmap(w, h, config)
        val canvas = Canvas(bitmap)
        drawable.setBounds(0, 0, w, h)
        drawable.draw(canvas)
        return bitmap
    }

    fun zoomDrawable(context: Context, drawable: Drawable?, w: Int, h: Int): Drawable? {
        val width = drawable!!.intrinsicWidth;
        val height = drawable.intrinsicHeight
        val oldbmp = drawableToBitmap(drawable)
        val matrix = Matrix()
        val sx = w * 1.0f / width
        val sy = h * 1.0f / height
        matrix.postScale(sx, sy)
        val newbmp = Bitmap.createBitmap(oldbmp, 0, 0, width, height, matrix, true)
        return BitmapDrawable(context.resources, newbmp)
    }

    fun loadActionBarIcon(context: Context, res: Int): Drawable? {
        val bmp = BitmapFactory.decodeResource(context.resources, res)
        val d = BitmapDrawable(context.resources, bmp)
        return resizeActionIcon(context, d)
    }

    private fun resizeActionIcon(context: Context, drawable: Drawable?): Drawable? {
        val heightBase = UIUtils.dip2px(24)
        val height = (heightBase * UIUtils.density!!).toInt()
        return zoomDrawable(context, drawable, height, height)
    }

    fun readFileImage(filename: String): ByteArray? {
        val ins = BufferedInputStream(FileInputStream(filename))
        val len = ins.available()
        val buffer = ByteArray(len)
        val r = ins.read(buffer)
        if (len != r) {
            throw Exception("read image failed.")
        }
        ins.close()
        return buffer
    }

    fun saveBitmapToFile(bmp: Bitmap?, fileName: String, format: Bitmap.CompressFormat) {
        val f = File(fileName)
        f.createNewFile()
        val fOut = FileOutputStream(f)
        bmp?.compress(format, 100, fOut)
        fOut.flush()
        fOut.close()
    }

    fun getSmallBitmap(path: String, reqSize: Int): Bitmap {
        val options = BitmapFactory.Options()
        options.inJustDecodeBounds = true
        BitmapFactory.decodeFile(path, options)
        var scale = 1

        while (true) {
            if (options.outWidth <= reqSize || options.outHeight <= reqSize) {
                break
            } else {
                options.outWidth = options.outWidth / 2
                options.outHeight = options.outHeight / 2
                scale++
            }
        }
        val newoptions = BitmapFactory.Options()
        newoptions.inSampleSize = scale
        return BitmapFactory.decodeFile(path, newoptions)
    }

    val AUTH_DOCUMENT = "com.android.externalstorage.documents"
    val AUTH_DOWNLOADS = "com.android.providers.downloads.documents"
    val AUTH_MEDIA = "com.android.providers.media.documents"
    val AUTH_PHOTO = "com.google.android.apps.photos.content"

    fun isExternalStorageDocument(uri: Uri?): Boolean = uri?.authority == AUTH_DOCUMENT

    fun isDownloadsDocument(uri: Uri?): Boolean = uri?.authority == AUTH_DOWNLOADS

    fun isMediaDocument(uri: Uri?): Boolean = uri?.authority == AUTH_MEDIA

    fun isGooglePhotosUri(uri: Uri?): Boolean = uri?.authority == AUTH_PHOTO

    fun getDataColumn(context: Context, uri: Uri?, selection: String?, selectionArgs: Array<String>?): String? {
        var ret: String? = null
        val column = MediaStore.Images.Media.DATA
        val cursor = context.contentResolver.query(uri, arrayOf(column), selection, selectionArgs, null)
        if (cursor != null && cursor.moveToFirst()) {
            val index = cursor.getColumnIndexOrThrow(column)
            ret = cursor.getString(index)
        }
        cursor.close()
        return ret
    }

    fun getImageAbsolutePath(context: Context, imageUri: Uri): String? =
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT && DocumentsContract.isDocumentUri(context, imageUri)) {
                if (isExternalStorageDocument(imageUri)) {
                    val split = DocumentsContract.getDocumentId(imageUri).split(":")
                    val type = split[0].toLowerCase()
                    if (type == "primary") {
                        "${Environment.getExternalStorageDirectory().absolutePath}/${split[1]}"
                    } else {
                        null
                    }
                } else if (isDownloadsDocument(imageUri)) {
                    val contentUri = ContentUris.withAppendedId(Uri.parse("content://downloads/public_downloads"), DocumentsContract.getDocumentId(imageUri).toLong())
                    getDataColumn(context, contentUri, null, null)
                } else if (isMediaDocument(imageUri)) {
                    val split = DocumentsContract.getDocumentId(imageUri).split(":")
                    val type = split[0].toLowerCase()
                    var contentUri: Uri? = null
                    if (type == "image") {
                        contentUri = MediaStore.Images.Media.EXTERNAL_CONTENT_URI
                    } else if (type == "video") {
                        contentUri = MediaStore.Video.Media.EXTERNAL_CONTENT_URI
                    } else if (type == "audio") {
                        contentUri = MediaStore.Audio.Media.EXTERNAL_CONTENT_URI
                    }
                    val selection = "${MediaStore.Images.Media._ID}=?"
                    val selectionArgs = arrayOf(split[1])
                    getDataColumn(context, contentUri, selection, selectionArgs)
                } else {
                    null
                }
            } else if (imageUri.scheme.toLowerCase() == "content") {
                if (isGooglePhotosUri(imageUri)) {
                    imageUri.lastPathSegment
                } else {
                    getDataColumn(context, imageUri, null, null)
                }
            } else if (imageUri.scheme.toLowerCase() == "file") {
                imageUri.path
            } else {
                null
            }

}