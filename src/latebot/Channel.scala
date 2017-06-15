package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue

class Channel(recipient: String, incoming: Queue[Message], out: BufferedWriter, homeChannel: String, bot: latebot, historySize: Int, messageHistory: Queue[Message]) extends Conversation(recipient, incoming, out, homeChannel, bot, historySize, messageHistory) {

  private val chatters = Buffer[Chatter]()

  def takeLine(msg: Message): Unit = {
    if(!this.chatters.find(_.nick == msg.nick.get).isDefined){
      val newChatter = new Chatter(msg.nick.get, this)
      this.chatters += newChatter
      newChatter.takeLine(msg)
    } else {
      val chatter = this.chatters.find(_.nick == msg.nick.get).get
      chatter.takeLine(msg)
      if(chatter.isSpam(msg)){
        this.spam(chatter, out)
      }
    }
    this.addToHistory(msg)
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
    this.sendMessage(out, "Warning " + spammer.nick + " at " + this.recipient, "speug")
  }

  def kickSpammer(channel: String, spammer: Chatter, out: BufferedWriter) = {
    /*
    println("Kicking " + spammer.nick)
    this.sendData(out, "KICK " + channel + " " + spammer.nick + " :You need to chill, " + spammer.nick + ".")
    *
    */
    this.sendMessage(out, "But I want to kick " + spammer.nick + " at " + this.recipient, "speug")
  }

  def kickBan(channel: String, spammer: Chatter, out: BufferedWriter) = {
    /*
    println("Kickbanning " + spammer.nick)
    this.sendData(out, "MODE " + channel + " " + " -o" + spammer.hostmask)
    this.sendData(out, "KICK " + channel + " " + spammer.nick + " :The robotic justice is swift, " + spammer.nick + ".")
    this.sendData(out, "MODE " + channel + " +b" + " *" + spammer.hostmask)
    */
    this.sendMessage(out, "Banning " + spammer.nick + " at " + this.recipient, "speug")
  }

  def confirmQuote(toBeConfirmed: String) = {
    println("Cannot confirm quote; this is a Conversation.")
  }

}
