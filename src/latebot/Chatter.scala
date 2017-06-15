package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue

class Chatter(val nick: String, val conversation: Conversation) {

  private val messageHistory = new Queue[Message]()

  def takeLine(msg: Message): Unit = {
    if (this.messageHistory.size <= 10) {
      this.messageHistory += msg
    } else {
      val toRemove = this.messageHistory.dequeue()
      this.messageHistory += msg
    }
  }

  def hostmask = {
    this.messageHistory(0).raw.dropWhile(_ != '!').takeWhile(_ != ' ')
  }

  def isSpam(msg: Message) = {
    if(this.conversation.isChannel){
    (this.isTooFrequent(msg.ms) || (this.messageHistory.filter((historyMsg: Message) => historyMsg.raw == msg.raw && !historyMsg.raw.split(":").contains('!')).size >= 5))
    } else {
      false
    }
  }

  def isTooFrequent(line: (Long)) = {
    val times = Buffer[Long]()
    for (i <- 1 until this.messageHistory.size) {
      times += this.messageHistory(i).ms - this.messageHistory(i - 1).ms
    }
    times += line - this.messageHistory(this.messageHistory.size - 1).ms
    times.filter(_ <= 3000).size > 5
  }

  def flushQueue() = {
    this.messageHistory.clear()
  }

}
