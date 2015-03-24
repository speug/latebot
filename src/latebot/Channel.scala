package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue

class Channel(recipient: String, incoming: Queue[(Int, String)], out: BufferedWriter, homeChannel: String, bot: latebot) extends Conversation(recipient, incoming, out, homeChannel, bot) {

  private val chatters = Buffer[Chatter]()
  
  def takeLine(line: (Int,String), nick: String): Unit = {
    if(!this.chatters.find(_.nick == nick).isDefined){
      val newChatter = new Chatter(nick, this)
      this.chatters += newChatter
      newChatter.takeLine(line)
    } else {
      val chatter = this.chatters.find(_.nick == nick).get
      if(chatter.isSpam(line)){
        this.spam(chatter, out)
      }
    }
  }
    
   def spam(who: Chatter, out: BufferedWriter) = { 
   this.bot.addToBlackList(who)
   this.bot.blackList(who) match {
     case 0 =>
     case 1 => this.warnSpammer(who.nick, out)
     case 2 => this.kickSpammer(recipient, who.nick, out)
     case _ => this.kickBan(recipient, who.nick, out)
    }
   }
  
  def warnSpammer(nick: String, out: BufferedWriter) = {
    val message = "Cease the spam, " + nick + "."
    this.sendMessage(out, message, nick)
  }
  
  def kickSpammer(channel: String, nick: String, out: BufferedWriter) = {
    this.sendData(out, "KICK " + channel + nick + " :You need to chill, " + nick + ".")
  }
  
  def kickBan(channel: String, nick: String, out: BufferedWriter) = {
    this.sendData(out, "MODE " + nick + " -o")
    this.sendData(out, "KICK " + channel + nick + " :You need to chill, " + nick + ".")
    this.sendData(out, "MODE " + nick + " +b")
  }
}