package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue

class Chatter(val nick: String, val conversation: Conversation) {

  private val messageHistory = new Queue[(Long, String)]()

  def takeLine(line: (Long, String)): Unit = {
    if (this.messageHistory.size <= 10) {
      this.messageHistory += line
    } else {
      val toRemove = this.messageHistory.dequeue()
      this.messageHistory += line
    }
  }
  
  def hostmask = {
    this.messageHistory(0)._2.dropWhile(_ == ':').takeWhile(_ != ' ')
  }

  def isSpam(line: (Long, String)) = {
  (this.isTooFrequent(line._1) || (this.messageHistory.filter((historyLine: (Long, String)) => historyLine._2 == line._2 && !historyLine._2.split(":").contains('!')).size >= 5))
  }

  def isTooFrequent(line: (Long)) = {
    val times = Buffer[Long]()
    for (i <- 1 until this.messageHistory.size) {
      times += this.messageHistory(i)._1 - this.messageHistory(i - 1)._1
    }
    times += line - this.messageHistory(this.messageHistory.size - 1)._1
    times.filter(_ <= 3000).size > 5
  }
  
  def flushQueue() = {
    this.messageHistory.clear()
  }

}