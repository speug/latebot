package latebot

import java.util.Calendar
import java.text.SimpleDateFormat

class Message(raw: String) {

  val ms = System.currentTimeMillis()
  private val created = Calendar.getInstance().getTime()
  private val timeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")

  val time = timeFormat.format(created)

  val type: String = raw.split(' ')(1)

  val address: String = {
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

  val nick: Some(String) = {
    if(this.type == "PRIVMSG"){
      Some(this.dataSplit(1).split("!")(0))
    } else {
      None
    }
  }

  /**
   * Returns the command word in a this message.
   *
   * @returns a wrapped command word, if such exists; otherwise None.
   */

  val command = {
      if(this.type == "PRIVMSG"){
        val potential = line.split(":").last.dropWhile(_ != '!').takeWhile(_ != ' ').trim()
        if(potential(0) == '!'){
          Some(potential)
        } else {
          None
        }
      } else {
        None
      }
    }

  def mkString = this.time + ": " + raw
}
