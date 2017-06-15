package latebot

import java.util.Calendar
import java.text.SimpleDateFormat

class Message(s: String) {

  val raw = s
  val ms = System.currentTimeMillis()
  private val created = Calendar.getInstance().getTime()
  private val timeFormat = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")

  val time = timeFormat.format(created)

  val msgType: String = if(raw(0)==':'){ raw.split(' ')(1) } else { raw.split(' ')(0) }

  val address: String = {
    if(raw.contains("irc.cs.hut.fi")){
    "Server"
  } else if(this.msgType == "PRIVMSG") {
      var recipent = raw.split("PRIVMSG ")(1).takeWhile(_ != ' ')
      if (recipent(0) == '#' || recipent(0) == '!') {
        recipent
      } else {
        recipent = raw.split(":")(1).split("!")(0)
        recipent
      }
    } else {
      "none"
    }
  }

  val dataSplit = raw.split(':')

  val nick: Option[String] = {
    var out: Option[String] = None
    if(this.msgType == "PRIVMSG"){
      out = Some(this.dataSplit(1).split("!")(0))
    }
    out
  }

  /**
   * Returns the command word in a this message.
   *
   * @returns a wrapped command word, if such exists; otherwise None.
   */

  val command = {
      if(this.msgType == "PRIVMSG"){
        val potential = raw.split(":").last.dropWhile(_ != '!').takeWhile(_ != ' ').trim()
        if(potential.lift(0).isDefined && potential(0) == '!'){
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
