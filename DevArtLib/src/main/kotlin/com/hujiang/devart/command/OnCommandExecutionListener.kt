package com.hujiang.devart.command

/**
 * Created by rarnu on 3/23/16.
 */
interface OnCommandExecutionListener {

    fun onCommandStart(cmd: Command)
    fun onCommandReadLine(cmd: Command, line: String)
    fun onCommandReadError(cmd: Command, line: String)
    fun onCommandComplete(cmd: Command)

}