package latebot

import java.util.Calendar
import java.text.SimpleDateFormat

class Message(raw: String) {

  val ms = System.currentTimeMillis()
  private val created = Calendar.getInstance().getTime()
  private val timeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")

  def time = timeFormat.format(created)

  val type: String = {
      raw.split(' ')(1)
  }

  def address: String = {
    if(raw.contains("irc.cs.hut.fi")){
    "Server"
    } else {
      var recipent = raw.split("PRIVMSG ")(1).takeWhile(_ != ' ')
      if (recipent(0) == '#' || recipent(0) == '!') {
        recipent
      } else {
        recipent = line.split(":")(1).split("!")(0)
        recipent
      }
    }
  }

  val dataSplit = raw.split(':')

  def nick = {
    if(this.type == "PRIVMSG"){
      this.dataSplit(1).split("!")(0)
    }
  }

  def mkString = this.time + ": " + raw
}
