package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue

class Channel(recipient: String, incoming: Queue[(String, Int)], out: BufferedWriter, homeChannel: String) extends Conversation(recipient, incoming, out, homeChannel) {

  def findNick(line: String) = {
    line.split(":")(1).split("!")(0)
  }  
  
  def isSpam = {
    val nicks = this.incoming.map(_._1).map(this.findNick(_))
    val messagesByNick = nicks.zip(this.incoming).toBuffer.groupBy( _._1 => _)
    if
  }
}