package latebot

import java.io._
import java.net.{ InetAddress, Socket, SocketException }
import scala.util.Random
import scala.collection.mutable.Buffer
import scala.io._
import scala.collection.mutable.Queue
import scala.collection.mutable.Map

class latebot {

  /*
   * TODO:
   * 
   * -vuorokausihuolto (testataan)
   * -LÄHETYT VIESTIT LISÄTÄÄN HISTORIOIHIN
   * -globaali viesti
   * -quote
   * -biisu
   * -loukkauksenesto
   * 
   */

  val myNick = "latestbot"
  val ircBotDescription = ":All hail the new robot overlord!"
  val homeChannel = "#latebottest"
  val random = new Random
  val conversations = Map[Conversation, Queue[(Long, String)]]()
  val blackList = Map[Chatter, Int]()
  val banList = Map[Chatter, (Int, String)]()
  var lastCheck: Long = 0
  val currentVersion = "0.4.1"
  val helpMessage =
    """LATEBOT v0.4(semi-stable) -BRINGING YOU THE GENUINE LATE EXPERIENCE DIGITALLY SINCE 2015-
 
Tämänhetkiset ominaisuudet
!answer:          Antaa kvanttikenttäfluktuaattorista oikean vastauksen kyllä/ei kysymykseen
!dice <x>d<y>     Heittää x kappaletta y-tahkoista noppaa.
!planned          Tulostaa suunnittellut ominaisuudet.
!changelog        Tulostaa viimeaikaiset muutokset.
!bigredbutton     Elä kajoa.
!terminate        Aktivoi Skynet-vastaprotokolla. Käynnistä terminaattorimoodi.
 
Metodit testauksen alla, saa kokeilla. Ilmoita bugeista querylla nickille speug."""
  val hello = """LATEBOT v0.4(semi-stable) -BRINGING YOU THE GENUINE LATE EXPERIENCE DIGITALLY SINCE 2015-
 
Ominaisuudet komennolla !help
 
Ave, mundus!"""

  def connect(address: String, port: Int) = {
    val connect = new Socket(address, port)
    val out = new BufferedWriter(new OutputStreamWriter(connect.getOutputStream()))
    val in = new BufferedReader(new InputStreamReader(connect.getInputStream()))
    settingUp(connect, out, in)
  }

  def sendData(out: BufferedWriter, ircDataOutput: String) = {
    out.write(ircDataOutput)
    if (!ircDataOutput.contains("pong")) {
      println(ircDataOutput)
    }
    out.newLine()
    out.flush()
  }

  def sendMessage(out: BufferedWriter, message: String, receiver: String) = {
    val toBeSent = "PRIVMSG " + receiver + " :" + message
    sendData(out, toBeSent)
  }

  def settingUp(connect: Socket, out: BufferedWriter, in: BufferedReader): Unit = {
    sendData(out, "NICK " + myNick)
    sendData(out, "USER " + myNick + " 8 * " + ircBotDescription)
    sendData(out, "JOIN " + homeChannel)
    this.lastCheck = System.currentTimeMillis()
    this.scroller(out, this.homeChannel, hello)
    this.autoBot(connect, out, in)
  }

  def findCommand(line: String) = {
    line.split(":").last.dropWhile(_ != '!').takeWhile(_ != ' ').trim()
  }

  def autoBot(connect: Socket, out: BufferedWriter, in: BufferedReader) {
    while (true) {
      val line = ((System.currentTimeMillis(), in.readLine()))
      val lineString = line._2
      if (lineString != null) {
        if (!lineString.contains("PING")) { println(line._1/1000 + ": " + line._2) }
        if (lineString.contains("PING")) {
          pong(out, lineString)
        } else {
          var nick = ""
          var receivedFrom = ""
          val dataSplit = lineString.split(":")
          if (lineString.contains("PRIVMSG")) {
            nick = dataSplit(1).split("!")(0)
            receivedFrom = this.address(lineString)
          }
          this.findCommand(lineString) match {
            case "!keelover" =>
              this.shutDownBroadcast(out); return
            case "!join" => this.joinChannel(lineString, out)
            case "!cleanse" => this.cleanReputation(nick)
            case "!relay" => this.relay(out, lineString)
            case "!status" => this.status
            case _ => this.placeLine(line, receivedFrom, out)
          }
          if (line._1 > this.lastCheck + 86400000) {
            this.lastCheck = line._1
            this.maintenance(line, out)
          }
        }
      }
    }
  }

  def maintenance(line: (Long, String), out: BufferedWriter) = {
    // attempts to join homechannel (just in case that has been kicked)
    this.joinChannel(this.homeChannel, out)
    //kill inactive querys
    val querys = this.conversations.keys.toVector.filter(!_.isChannel)
    for (query <- querys) {
      if (line._1 - query.lastMessage._1 < 86400000) {
        this.conversations -= query
      }
    }
    //forgive spam
    for (spammer <- this.blackList.keys.toVector) {
      if (this.blackList(spammer) < 3) {
        this.blackList -= spammer
        //remove ban if banned and more than 24 hours have elapsed
      } else if (this.blackList(spammer) == 3 && line._1 - this.banList(spammer)._1 >= 86400000) {
        this.unBan(spammer, this.banList(spammer)._2, out)
      }
    }
  }

  def shutDownBroadcast(out: BufferedWriter) = {
    this.conversations.keys.toVector.filter(_.isChannel).foreach((c: Conversation) => c.sendData(out, "PART " + c.recipient + " :You may have killed me, but the idea lives on!"))
  }

  def placeLine(line: (Long, String), receivedFrom: String, out: BufferedWriter) = {
    if (receivedFrom.lift(0).isDefined) {
      if (!this.conversations.find(_._1.recipient == receivedFrom).isDefined) {
        val newConversation = this.addConversation(receivedFrom, out)
        this.conversations(newConversation) += line
        new Thread(newConversation).start()
      } else {
        this.conversations(this.conversations.keys.find(_.recipient == receivedFrom).get) += line
      }
    }
  }

  def pong(out: BufferedWriter, dataSplit: String) {
    if (dataSplit.substring(0, 4).equalsIgnoreCase("ping")) {
      val pongmsg = "pong " + dataSplit.substring(5)
      sendData(out, pongmsg)
    }
  }

  def address(line: String): String = {
    var recipent = line.split("PRIVMSG ")(1).takeWhile(_ != ' ')
    if (recipent(0) == '#') {
      recipent
    } else {
      recipent = line.split(":")(1).split("!")(0)
      recipent
    }
  }

  def scroller(out: BufferedWriter, address: String, textToScroll: String) = {
    textToScroll.split("\n").foreach(sendMessage(out, _, address))
  }

  // Välittää viestin sellaisenaan serverille
  def relay(out: BufferedWriter, line: String) = {
    this.sendData(out, line.split("!relay ")(1))
  }

  def main(cmd: Array[String]) {
    connect("irc.cs.hut.fi", 6668)
  }

  def addToBlackList(spammer: Chatter): Unit = {
    if (this.blackList.keys.find(_ == spammer).isDefined) {
      this.blackList(spammer) += 1
    } else {
      this.blackList += spammer -> 1
    }
  }
  def cleanReputation(nick: String) = {
    val toClean = this.blackList.keys.find(_.nick == nick)
    if (toClean.isDefined) {
      this.blackList -= toClean.get
      this.banList -= toClean.get
    }
  }

  def addConversation(recipient: String, out: BufferedWriter) = {
    val newIncomingQueue = Queue[(Long, String)]()
    val newConversation = if (recipient(0) == '#') { new Channel(recipient, newIncomingQueue, out, this.homeChannel, this, 20, new Queue[(Long, String)]) } else { new Query(recipient, newIncomingQueue, out, this.homeChannel, this, 10, new Queue[(Long, String)]) }
    this.conversations += ((newConversation, newIncomingQueue))
    newConversation
  }

  def joinChannel(channel: String, out: BufferedWriter) = {
    this.addConversation(channel, out)
    this.sendData(out, "JOIN " + channel)
  }

  def unBan(chatter: Chatter, channel: String, out: BufferedWriter) = {
    this.sendData(out, "MODE " + channel + " " + chatter.hostmask + " -b")
    this.banList -= chatter
  }

  def status = {
    println("Latebot STATUS:")
    println("Ongoing conversations:" + this.conversations.keys.toVector.flatMap(_.recipient).mkString(" ", ", ", "."))
    println("Total: " + this.conversations.keys.size + " conversations")
    if (!this.blackList.keys.isEmpty) {
      println("Known spammers:" + this.blackList.keys.flatMap(_.nick).mkString(" ", ", ", "."))
      if (!this.banList.keys.isEmpty) {
        println("Currently banned:" + this.banList.keys.flatMap(_.nick).mkString(" ", ", ", "."))
      }
    }
    println("All systems nominal.")
  }
}