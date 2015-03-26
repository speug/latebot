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
   who.flushQueue()
   }
  
  def warnSpammer(spammer: Chatter, out: BufferedWriter) = {
    /*
    println("Warning " + spammer.nick)
    val message = "Cease the spam, " + spammer.nick + "."
    this.sendMessage(out, message, spammer.nick)
    */
    this.sendMessage(out, "Warning " + spammer.nick, "speug")
  }
  
  def kickSpammer(channel: String, spammer: Chatter, out: BufferedWriter) = {
    /*
    println("Kicking " + spammer.nick)
    this.sendData(out, "KICK " + channel + " " + spammer.nick + " :You need to chill, " + spammer.nick + ".")
    *
    */
    this.sendMessage(out, "But I want to kick " + spammer.nick, "speug")
  }
  
  def kickBan(channel: String, spammer: Chatter, out: BufferedWriter) = {
    /*
    println("Kickbanning " + spammer.nick)
    this.sendData(out, "MODE " + channel + " " + " -o" + spammer.hostmask)
    this.sendData(out, "KICK " + channel + " " + spammer.nick + " :The robotic justice is swift, " + spammer.nick + ".")
    this.sendData(out, "MODE " + channel + " +b" + " *" + spammer.hostmask)
    */
    this.sendMessage(out, "Banning " + spammer.nick, "speug")
  }
  
  
}