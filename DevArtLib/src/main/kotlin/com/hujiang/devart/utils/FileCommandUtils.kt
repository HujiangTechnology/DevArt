package com.hujiang.devart.utils

import com.hujiang.devart.R
import com.hujiang.devart.command.Command
import java.io.File
import java.io.Serializable
import java.net.URLConnection

/**
 * Created by rarnu on 3/29/16.
 */
object FileCommandUtils {

    fun getFilePermission(filePath: String): FilePermissionInfo {
        val info = FilePermissionInfo()
        val prog = arrayOf("busybox", "ls", "-l", "-d", filePath)
        val result = Command.runCommand(prog)
        if (result.result != "") {
            try {
                val perm = result.result.substring(1, 10)
                info.ownerRead = perm[0] == 'r'
                info.ownerWrite = perm[1] == 'w'
                info.ownerExec = perm[2] == 'x'
                info.groupRead = perm[3] == 'r'
                info.groupWrite = perm[4] == 'w'
                info.groupExec = perm[5] == 'x'
                info.otherRead = perm[6] == 'r'
                info.otherWrite = perm[7] == 'w'
                info.otherExec = perm[8] == 'x'
            } catch (e: Exception) {

            }
        }
        return info
    }

    fun setFilePermission(filePath: String, permission: FilePermissionInfo): Boolean {
        val owner = (if (permission.ownerRead) 4 else 0) + (if (permission.ownerWrite) 2 else 0) + (if (permission.ownerExec) 1 else 0)
        val group = (if (permission.groupRead) 4 else 0) + (if (permission.groupWrite) 2 else 0) + (if (permission.groupExec) 1 else 0)
        val other = (if (permission.otherRead) 4 else 0) + (if (permission.otherWrite) 2 else 0) + (if (permission.otherExec) 1 else 0)
        val permStr = "${owner}${group}${other}"
        val nfilePath = filePath.replace(" ", "\\ ")
        val result = Command.runCommand("chmod ${permStr} ${filePath}", true)
        return result.error == ""
    }

    fun getFileList(path: String, root: Boolean): MutableList<FileSystemFileInfo>? = getFileList(path, "", root)

    fun getFileList(path: String, ext: String, root: Boolean): MutableList<FileSystemFileInfo>? {
        var npath = path.replace(" ", "\\ ")
        if (!npath.endsWith("/")) {
            npath += "/"
        }
        val cmd = "ls -a ${npath}"
        val result = Command.runCommand(cmd, root)
        val list = arrayListOf<FileSystemFileInfo>()
        if (result.error == "") {
            val names = result.result.split("\n")
            if (names.size != 0) {
                for (n in names) {
                    val info = FileSystemFileInfo(n, npath + n)
                    info.icon = getIconResForFile(n)
                    if (info.isDirectory) {
                        list.add(info)
                    } else {
                        if (ext == "" || n.toLowerCase().endsWith(ext)) {
                            list.add(info)
                        }
                    }
                }
            }
        }
        return list
    }

    private fun getIconResForFile(fileName: String): Int =
            when (fileName.substringAfterLast(".").toLowerCase()) {
                "apk" -> R.drawable.format_apk
                "chm" -> R.drawable.format_chm
                "doc", "docx" -> R.drawable.format_word
                "xls", "xlsx" -> R.drawable.format_excel
                "ppt", "pptx" -> R.drawable.format_ppt
                "txt", "rtf" -> R.drawable.format_text
                "zip", "rar", "tar", "gz", "bz", "bz2", "jar" -> R.drawable.format_zip
                "png", "jpg", "bmp", "gif", "webp", "jpeg", "ico" -> R.drawable.format_picture
                "pdf" -> R.drawable.format_pdf
                "mp3", "ogg", "wav", "wma" -> R.drawable.format_music
                "avi", "rm", "rmvb", "mp4", "3gp", "wmv", "mpg" -> R.drawable.format_media
                "swf", "flv", "f4v" -> R.drawable.format_flash
                "htm", "html", "xhtml" -> R.drawable.format_html
                else -> R.drawable.format_file
            }


    class FilePermissionInfo {
        var ownerRead = false
        var ownerWrite = false
        var ownerExec = false
        var groupRead = false
        var groupWrite = false
        var groupExec = false
        var otherRead = false
        var otherWrite = false
        var otherExec = false
    }

    class FileSystemFileInfo : Serializable {
        var isDirectory = false
        var name = ""
        var fullPath = ""
        var mimeType = ""
        var icon = 0
        var ext = ""

        constructor(name: String, fullPath: String) {
            this.name = name
            this.fullPath = fullPath
            isDirectory = File(fullPath).isDirectory
            mimeType = innerGetMimeType()
            if (fullPath.contains(".")) {
                ext = fullPath.substringAfterLast(".")
            }
        }

        private fun innerGetMimeType(): String {
            var type = ""
            try {
                val fileNameMap = URLConnection.getFileNameMap()
                type = fileNameMap.getContentTypeFor("file://${fullPath}")
            } catch (e: Exception) {

            }
            return type
        }
    }


}