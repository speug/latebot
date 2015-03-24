package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue

class Query(recipient: String, incoming: Queue[(Int, String)], out: BufferedWriter, homeChannel: String, bot: latebot) extends Conversation(recipient, incoming, out, homeChannel, bot)  {
  
  private val messageHistory = new Queue[(Int, String)]()

  def takeLine(line:(Int,String), nick: String) = {
    if(this.messageHistory.size <= 10){
      this.messageHistory += line
    } else {
      this.messageHistory.dequeue()
      this.messageHistory += line
      if(this.isSpammed(line._1)){
        val spammer = new Chatter(nick, this)
        this.bot.addToBlackList(spammer)
        this.spam(spammer, out)
      }
    }
  }
  
  def isSpammed(line: (Int)) = {
    val times = Buffer[Int]()
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
  
  
  def warn(spammer: Chatter, out: BufferedWriter) = {
    if(this.bot.blackList(spammer) == 1){
      this.sendMessage(out, "Stop the query spam, or else!", spammer.nick)
    } else {
      this.sendMessage(out, "Cease this instant, or you will be ignored for the next 24 hours!", spammer.nick)
    }
  }
  
  def sleep() = {
    
  }

}