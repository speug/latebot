package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue

class Query(recipient: String, incoming: Queue[(Int, String)], out: BufferedWriter, homeChannel: String, bot: latebot) extends Conversation(recipient, incoming, out, homeChannel, bot)  {
  
  private val messageHistory = new Queue[(Int, String)]()


}