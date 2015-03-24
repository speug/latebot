package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue

class Chatter(val nick: String, val conversation: Conversation) {

  private val messageHistory = new Queue[(Int, String)]()

  def takeLine(line: (Int, String)): Unit = {
    if (this.messageHistory.size <= 10) {
      this.messageHistory += line
    } else {
      val toRemove = this.messageHistory.dequeue()
      this.messageHistory += line
    }
  }

  def isSpam(line: (Int, String)) = {
  (this.isTooFrequent(line._1) || (this.messageHistory.filter(_._2 == line._2).size >= 5))
  }

  def isTooFrequent(line: (Int)) = {
    val times = Buffer[Int]()
    for (i <- 1 until this.messageHistory.size) {
      times += this.messageHistory(i)._1 - this.messageHistory(i - 1)._1
    }
    times += line - this.messageHistory(this.messageHistory.size - 1)._1
    times.filter(_ <= 3000).size > 5
  }

}