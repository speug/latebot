package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue

class Channel(recipient: String, incoming: Queue[(Long, String)], out: BufferedWriter, homeChannel: String, bot: latebot, historySize: Int, messageHistory: Queue[(Long, String)]) extends Conversation(recipient, incoming, out, homeChannel, bot, historySize, messageHistory) {

  private val chatters = Buffer[Chatter]()
  
  def takeLine(line: (Long,String), nick: String): Unit = {
    if(!this.chatters.find(_.nick == nick).isDefined){
      val newChatter = new Chatter(nick, this)
      this.chatters += newChatter
      newChatter.takeLine(line)
    } else {
      val chatter = this.chatters.find(_.nick == nick).get
      chatter.takeLine(line)
      if(chatter.isSpam(line)){
        this.spam(chatter, out)
      }
    }
    this.addToHistory(line)
  }
   

   def spam(who: Chatter, out: BufferedWriter) = { 
   this.bot.addToBlackList(who)
   this.bot.blackList(who) match {
     case 0 =>
     case 1 => this.warnSpammer(who, out)
     case 2 => this.kickSpammer(recipient, who, out)
     case _ => this.kickBan(recipient, who, out)
    }
   }
  
  def warnSpammer(spammer: Chatter, out: BufferedWriter) = {
    println("Warning " + spammer.nick)
    spammer.flushQueue
    val message = "Cease the spam, " + spammer.nick + "."
    this.sendMessage(out, message, spammer.nick)
    
  }
  
  def kickSpammer(channel: String, spammer: Chatter, out: BufferedWriter) = {
    println("Kicking " + spammer.nick)
    spammer.flushQueue
    this.sendData(out, "KICK " + this.recipient + " " + spammer.hostmask + " :You need to chill, " + spammer.nick + ".")
  }
  
  def kickBan(channel: String, spammer: Chatter, out: BufferedWriter) = {
    println("Kickbanning " + spammer.nick)
    spammer.flushQueue
    this.sendData(out, "MODE " + channel + " " + spammer.hostmask + " -o")
    this.sendData(out, "KICK " + channel + " " + spammer.hostmask + " :The robotic justive is swift, " + spammer.hostmask.takeWhile(_ != '!') + "!")
    this.sendData(out, "MODE " + channel + " " + spammer.hostmask + " +b")
  }
  
  
}