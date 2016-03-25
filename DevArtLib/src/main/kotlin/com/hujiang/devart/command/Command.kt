package com.hujiang.devart.command

import java.io.BufferedReader
import java.io.DataOutputStream
import java.io.File
import java.io.InputStreamReader

/**
 * Created by rarnu on 3/23/16.
 */
object Command {

    private var _rejected = false
    var rejected: Boolean = false
        get() {
            return _rejected
        }
    var rooted: Boolean = false
        get() {
            val suFiles = arrayOf("/system/bin/su", "/system/xbin/su")
            var hasSu = false
            for (f in suFiles) {
                if (File(f).exists()) {
                    hasSu = true
                    break
                }
            }
            return hasSu
        }

    var busyboxInstalled: Boolean = false
        get() {
            val busyboxFiles = arrayOf("/system/xbin/busybox", "/system/bin/busybox")
            var hasBusybox = false
            for (f in busyboxFiles) {
                if (File(f).exists()) {
                    hasBusybox = true
                    break
                }
            }
            return hasBusybox
        }

    fun runCommand(command: String, root: Boolean): CommandResult = runCommand(arrayOf(command), root, null)

    fun runCommand(prog: Array<String>): CommandResult = runCommand(prog, false, null)

    fun runCommand(prog: Array<String>, listener: OnCommandExecutionListener?): CommandResult = runCommand(prog, false, listener)

    fun runCommand(command: String, root: Boolean, listener: OnCommandExecutionListener?): CommandResult = runCommand(arrayOf(command), root, listener)

    fun runCommand(command: Array<String>, root: Boolean, listener: OnCommandExecutionListener?): CommandResult {
        val ret = CommandResult(this, "", "")
        var process: Process?
        var rootOs: DataOutputStream? = null
        var procOutOs: BufferedReader? = null
        var procErrOs: BufferedReader? = null
        listener?.onCommandStart(this)
        try {
            if (root) {
                process = Runtime.getRuntime().exec("su")
                rootOs = DataOutputStream(process.outputStream)
                rootOs.writeBytes("${command[0]}\n")
                rootOs.writeBytes("exit\n")
                rootOs.flush()
            } else {
                if (command.size == 1) {
                    process = Runtime.getRuntime().exec(command[0])
                } else {
                    process = Runtime.getRuntime().exec(command)
                }
            }
            procOutOs = BufferedReader(InputStreamReader(process.inputStream))
            procErrOs = BufferedReader(InputStreamReader(process.errorStream))
            var outStr = StringBuffer()
            var errStr = StringBuffer()
            var line: String?
            while (true) {
                line = procOutOs.readLine()
                if (line != null) {
                    outStr.append("${line}\n")
                    listener?.onCommandReadLine(this, line)
                } else {
                    break
                }
            }
            while (true) {
                line = procErrOs.readLine()
                if (line != null) {
                    errStr.append("${line}\n")
                    listener?.onCommandReadError(this, line)
                } else {
                    break
                }
            }
            process.waitFor()
            ret.result = outStr.toString().trim()
            ret.error = errStr.toString().trim()
        } catch(e: Exception) {
            if (e.message != null) {
                ret.error = e.message!!
            }
        } finally {
            rootOs?.close()
            procOutOs?.close()
            procErrOs?.close()
        }
        listener?.onCommandComplete(this)
        return ret
    }


    fun remount() {
        val cmd = "mount -o remount,rw /system"
        _rejected = false
        val ret = runCommand(cmd, true, null)
        if (ret.error.toLowerCase().contains("denied")) {
            _rejected = true
        }
    }


}