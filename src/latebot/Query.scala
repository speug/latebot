package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue

class Query(recipient: String, incoming: Queue[(Long, String)], out: BufferedWriter, homeChannel: String, bot: latebot, historySize: Int, messageHistory: Queue[(Long, String)]) extends Conversation(recipient, incoming, out, homeChannel, bot, historySize, messageHistory)  {
  
  private val chatter = new Chatter(recipient, this)

  def takeLine(line:(Long,String), nick: String) = {
    this.addToHistory(line)
      if(this.isSpammed(line._1)){
        this.bot.addToBlackList(this.chatter)
        this.spam(this.chatter, out)
      }
  }
  
  def isSpammed(line: (Long)) = {
    val times = Buffer[Long]()
    for (i <- 1 until this.messageHistory.size) {
      times += this.messageHistory(i)._1 - this.messageHistory(i - 1)._1
    }
    times += line - this.messageHistory(this.messageHistory.size - 1)._1
    times.filter(_ <= 3000).size > 7
  }
  
  def spam(spammer: Chatter, out: BufferedWriter) = {
    this.bot.blackList(spammer) match{
      case 0 => 
      case 1 => this.warn(spammer, out)
      case 2 => this.warn(spammer, out)
      case 3 => this.sleep()
    }
    
  }
  
  // tää kaikki on paskaa
  
  def warn(spammer: Chatter, out: BufferedWriter) = {
    /*
    this.messageHistory.clear()
    if(this.bot.blackList(spammer) == 1){
      this.sendMessage(out, "Stop the query spam, or else!", spammer.nick)
    } else {
      this.sendMessage(out, "Cease this instant, or you will be ignored for the next 24 hours!", spammer.nick)
    }
    */
    this.sendMessage(out, "Query spam from " + spammer.nick, "speug")
  }
  
  
  // WHAT WERE YOU THINKING?!?!?!?!?!?!? what does it even doooooooooooooooooo
  def sleep() = {
    Thread.sleep(86400000)
    this.bot.blackList -= chatter
  }
  
  def confirmQuote(quoteToConfirm: String) = this.synchronized {
    this.sendMessage(out, "Do you wish to add the following quote?", this.recipient)
    this.sendMessage(out, quoteToConfirm, this.recipient)
    this.sendMessage(out, "[y/n]", this.recipient)
    while(this.incoming.isEmpty){
      this.wait()
    } 
    if(this.incoming.dequeue._2.trim.equalsIgnoreCase("y")){
      this.bot.writeToFile("quotes.txt", quoteToConfirm)
      this.sendMessage(out, "The quote has been saved.", this.recipient)
    }
  }

}